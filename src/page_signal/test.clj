(ns page-signal.test
  (:use [page-signal.core]))

;;; BLOCK IDENTIFICATION AND ANALYSIS WITHOUT DOM RETENTION

(defn merge-text
  [s1 s2]
  (if (s/blank? s2)
    s1
    (if (s/blank? s1)
      s2
      (str s1 " " (s/trim s2)))))

(defn process-node
  "Processes node and returns altered blocks."
  [blocks node]
  (if (string? node)
    (let [text (s/replace node "\n" "") 
          block (peek blocks)]
      (if (s/blank? text)
        blocks ; return unchanged
        (conj (pop blocks) (merge-with merge-text block {:text text}))))
    
    (let [tag (:tag node)]
      (cond
        (ignorable-tag? tag)
        blocks ; return unchanged

        (= :a tag)
        (let [block (peek blocks)
              anchor-text (s/replace (get-anchor-text node) "\n" "")
              link-words (:link-words block 0)]
          (if (s/blank? anchor-text)
            blocks ; return unchanged
            (conj (pop blocks) {:text (merge-text (:text block) anchor-text)
                                :link-words (+ link-words
                                               (count-words anchor-text))})))
        
        :else
        (if-let [content (:content node)]
          (if (= (peek blocks) {}) ;; {} signifies a 'fresh' block
            (reduce process-node blocks content)
            (reduce process-node (conj blocks {}) content))
          blocks)))))

(defn text-blocks
  "Processes nodes and returns atomic text blocks =
any character sequence not interrupted by an HTML tag, except the A tag."
  [nodes]
  (let [body (h/select nodes [:body])
        blocks (reduce process-node [{}] body)]
    (if (= {} (peek blocks)) ;; sometimes end up with empty block
      (pop blocks)
      blocks)))

; some text blocks have no words => divide by zero i.e. :text "\n \n"
(defn assoc-word-count
  "Assocs the word-count into block."
  [{:keys [text] :as block}]
  (when text
    (assoc block :total-words (count-words text))))

(defn assoc-link-density
  "Assocs link density into block."
  [{:keys [total-words link-words] :as block}]
  (if (or (nil? link-words) (zero? total-words)) ;; zero? total-words re-examine
    (assoc block :link-density 0.0)
    (assoc block :link-density (double (/ link-words total-words)))))

(defn annotate-blocks
  "Annotates blocks with information necessary for boilerplate analysis."
  [blocks]
  (let [f (comp assoc-link-density
                assoc-word-count)]
    (map f blocks)))

(defn boiler?
  "Takes current block, previous block, and next block. Returns true if current block is deemed boilerplate."
  [{curr-words :total-words curr-ldens :link-density}
   {prev-words :total-words prev-ldens :link-density}
   {next-words :total-words next-ldens :link-density}]
  ;(println curr-words prev-words next-words curr-ldens prev-ldens
                                        ;next-ldens)
  ; throw excpetions when certain map keys missing?
  (when (or (> curr-ldens 0.333333)
            (and (<= prev-ldens 0.555556)
                 (<= curr-words 16)
                 (<= next-words 15)
                 (<= prev-words 4))
            (and (<= curr-words 40)
                 (<= next-words 17)))
    true))

(defn mark-boiler
  "Marks each annotated block in blocks as boilerplate or content."
  [blocks]
  (map (fn [curr prev next]
         (if (boiler? curr prev next)
          (assoc curr :boiler true)
          (assoc curr :boiler false)))
       blocks
       (conj blocks {:text "" :total-words 0 :link-words 0 :link-density 0.0})
       (concat blocks [{:text "" :total-words 0 :link-words 0 :link-density 0.0}])))

(defn get-full-text
  "Takes text blocks that have been annotated and marked for boilerplate. Returns full text as a string."
  [blocks]
  (let [content (filter #(not (:boiler %)) blocks)]
    (reduce str (map :text content))))

(defn file->full-text
  [f]
  (-> f
      file->nodes
      text-blocks
      annotate-blocks
      mark-boiler
      get-full-text))

;;;; DOM LEVEL BLOCK IDENTIFICATION WITH ZIPPERS

(defn remove
  "Removes loc and return next loc to be processed."
  [loc]
  (z/next (z/remove loc)))

(defn block-wrap
  "Wraps loc in block tag and returns next loc to be processed. Assumes the node at loc has no children. What if anchor tag has children?"
  [loc]
  (-> loc
      (z/edit loc #(array-map :tag :block :content [%]))
      z/down z/next))

(defn process-block-tag
  "Processes block tag and returns the next loc to be processed."
  [loc node]
  (if-let [left-loc (z/left loc)]
    (if (= :block (-> left-loc z/node :tag))
      (-> loc z/remove (z/insert-right node) z/down z/rightmost z/next)
      (block-wrap loc))
    (block-wrap loc)))

(defn process-loc
  [loc]
  (if (z/end? loc)
    loc
    (let [node (z/node loc)]
      (println "NODE: " node)
      (cond
       (ignorable-tag? (:tag node))
       (process-loc (remove loc))

       (string? node)
       (let [text (s/replace node "\n" "")]
         (if (s/blank? text) ;; allow whitespace?
           (process-loc (remove loc))
           (process-loc (process-block-tag text))))
       
       (block-tag? node)
       (process-loc (process-block-tag loc node))

       (nil? (:content node)) ;; treat same as block item for now
       (process-loc (process-block-tag loc node))

       :else ;; a separating tag
       (process-loc (z/next loc))))))

(defn id-blocks
  [root]
  (let [body (first (h/select root [:body]))
        loc (z/xml-zip body)
        blocks (process-loc loc)]
    (z/root blocks)))


;;;; ORIGINAL PARTITION AND PROCESS

(defn process-part
  "p is seq of nodes that are expected to either all be block nodes or all be non-block nodes. Seqs of atomic blocks are cleaned up and wrapped in a block tag. Seqs of non-block nodes are recursively processed by partition-and-process."
  [p]
  (if (block-item? (first p))
    (let [p (remove-nils (map clean-if-string p))]
      (when (pos? (count p))
        {:tag :block :content p}))
    (remove-nils (map partition-and-process p))))

(defn partition-and-process
  "Partitions child nodes of root into block nodes / non-block nodes and recursively submits each partition to be processed by process-part. Assumes root is a non-block node. Retruns nil for ignorable tags."
  [{:keys [tag content] :as root}]
  (when-not (ignorable-tag? tag)
    (if content
      (let [parts (partition-by block-item? content)]
        (assoc root :content (-> (map process-part parts)
                                 flatten
                                 remove-nils)))
      root)))

;;; Testing Word Matchin Performance

(def pool "ABC")
(defn get-random-id [n] (apply str (repeatedly n #(rand-nth pool))))
(def a1 (into-array (take 10000 (repeatedly #(get-random-id 5)))))
(def a2 (into-array (take 10000 (repeatedly #(get-random-id 5)))))

;;; Block Fusion

;; recalculate text-density when blocks fused?

(defn slope-delta
  [den1 den2]
  (double (/ (Math/abs (- den1 den2)) (max den1 den2))))

(defn fuse?
  [{den1 :text-density :as block1} {den2 :text-density :as block2} thresh]
  (<= (slope-delta den1 den2) thresh))

;; Terminating Blocks

(def starts-with #{"comments"
                   "@reuters"
                   "please rate this"
                   "poast a comment"})

(def contains #{"what you think..."
                "add your comment"
                "add coment"
                "reader views"
                "have your say"
                "reader comments"
                "thanks for your comments - this feedback is now closed"})

(defn mark-terminating-block
  [{:keys [total-words link-words] :as block}]
  (let [^String text (u/text block)]
    ;; need a short-circuiting version?
    (reduce #(or %1 %2)
            (map #(.startsWith text %) starts-with))))

;; short-circuiting
(for [x starts-with
      :while (not (.startsWith "comment" x))]
  x)

(defn ignore-blocks-after-content
  "Marks all blocks that occure after :end-of-text as :boiler"
  [])

;;; Extract Article Title

; combos
(defn f*
  [n v r]
  (if (>= (count v) n)
    (recur n (rest v) (conj r (take n v)))
    r))

(defn f
  [n v] (f* n v []))

(defn g
  [v]
  (mapcat f
          (map inc (range (count v)))
          (repeat v)))


(def sep #" \| | \- |: ")

(defn make-class-set
  [ts]
  (clojure.set/union (set (map #(keyword (str "." %)) ts))
                     (set (map #(keyword (str "#" %)) ts))))

(defn get-class-titles
  [ts hres]
  (let [tags (h/select hres [:body (make-class-set ts)])]
    (map u/text tags)))

(defn get-title-tag-titles
  [hres]
  (let [sep #" \| | \- |: "
        title-tag-text (doc-title-text hres)]
    (s/split title-tag-text sep)))

(defn mark-title-match
  [root title]
  (map-blocks #(assoc % :title-match
                        ((partial match-score title) (u/text %)))
              root))

(defn map-non-block-tag-nodes
  "Returns root with function f applied to every block node and every tag node that isn't inside a block, starting at root and proceeding via depth first traversal. Non-tag nodes (i.e. strings) and nodes wrapped in block tag are returned unchaged. If f returns nil for any tag node, its descendants are lost. Assumes root is a non-block tag node."
  [f root]
  (if (map? root)
    (if-let [{:keys [content] :as froot} (f root)]
      (if (and (not= tag :block) content)
        (assoc froot :content (map (partial map-non-block-tag-nodes f) content))
        froot))
    root))

(defn remove-linky
  [nodes]
  (filter #(zero? (:link-density %)) (get-htags nodes))) ;; link
;; density only in blocks

(defn find-headline-block
  [root]
  (let [candidates (filter #(and (> (:title-match %) 0.333)
                                 (zero? (:link-density %)))
                           (seq-view root))]
    (reduce #(max (:title-match %1)
                  (:title-match %2)) candidates)))

(defn get-htags
  [nodes]
  (group-by :tag (h/select nodes [#{:h1 :h2 :h3}])))

(defn h-tags
  [candidates]
  (filter #(or (= (:enclosing-tag %) :h1)
               (= (:enclosing-tag %) :h2)
               (= (:enclosing-tag %) :HEADLINE)) candidates))

(defn class-set
  [coll]
  (clojure.set/union (set (map #(h/attr-contains :class %) coll))
                     (set (map #(h/attr-contains :id %) coll))))

(def headline-strings #{"title" "headline" "heading" "header"})

(defn get-classes
  [class-names nodes]
  (h/select nodes [(class-set class-names)]))

(defn non-title-tag
  [nodes]
  (let [title-class-nodes ((get-classes title-strings nodes))]
    (if-not (empty? title-class-nodes)
      (first title-class-nodes)
      (let [htag-nodes (filter #(zero? (:link-density %)) (get-htags nodes))]))))

; old title matching
(defn match-score
  [s1 s2]
  (if (or (s/blank? s1) (s/blank? s2))
    0.0
    (let [^objects a1 (into-array (u/words s1))
          ^objects a2 (into-array (u/words s2))
          match-count (u/count-matches a1 a2)]
      (if (zero? match-count)
        0.0
        (double (/ match-count (max (alength a1) (alength a2))))))))

(defn max-match
  "Scores every string in block against title and returns the best matching string and its score in a vector: [best-matching-string score]."
  [block title]
  (reduce (fn [[s1 score1] [s2 score2]]
            (if (> score2 score1)
              [s2 score2]
              [s1 score1]))
          ["" 0.0]
          (map (fn [s]
                 [s (match-score title s)])
               (seq-strings block))))

(defn mark-title-match
  [root title]
  (map-blocks (fn [block]
                (let [[title-text score] (max-match block title)]
                  (assoc block :title-match score :title-text title-text)))
              root))

(defn headline-block
  [nodes]
  (let [candidates (filter #(and (> (:title-match %) 0.28)
                                 (zero? (:link-density %)))
                           (seq-blocks nodes))
        num-cand (count candidates)]
    (cond
     (zero? num-cand)
     (let [r {:title-text (non-title-tag (z/xml-zip nodes))}]
       nil)

     (= 1 num-cand)
     (first candidates)

     :else
     (first candidates)))) ; first works better than longest string
                                        ; (434 vs 396)

;;; OLD HEADLINE EVALUATION



(defn annot-headline
  [f]
  (let [span (-> f
                 in/file->nodes
                 first
                 (get-span #{(:headline annotations)})
                 first)
        headline (u/clean-string (u/text span))]
    (if span
      (if headline
        headline
        :span-but-no-headline)
      :not-annotated)))

(defn headline
  [f]
  (try (-> f
           h/html-resource
           c/process
           c/headline-block
           :title-text)
       (catch Exception e {:exception (.getMessage e)}))) ;; likely no
;; body

(defn eval-headline
  [n f-out]
  (let [orig (get-sample orig-dir n)
        annot (files-from-dir annot-dir orig)
        h-retriv (map (fn [f]
                     {:file (.getName f) :headline (headline f)}) orig)
        h-annot (map (fn [f]
                       {:file (.getName f) :headline (annot-headline f)}) annot)
        res (map (fn [{head-r :headline file-r :file} {head-a :headline file-a :file}]
                   (let [match (if (or (map? head-r) ; exception in head-r
                                       (= :not-annotated head-a)
                                       (= :span-but-no-headline head-a))
                                 :na
                                 (= head-r head-a))] 
                     {:file file-r :head-r head-r :head-a head-a :match match}))
                 h-retriv
                 h-annot)
        no-headline-ret (filter #(and (false? (:match %)) (nil? (:head-r %))) res)
        headline-mismatch (filter #(and (false? (:match %)) (not (nil? (:head-r %)))) res)
        successes (filter #(true? (:match %)) res)
        exceptions (filter #(map? (:head-r %)) res)
        match-na (filter #(= :na (:match %)) res)
        not-annotated (filter #(= :not-annotated (:headline %)) h-annot)
        span-but-no-headline (filter #(= :span-but-no-headline (:headline %)) h-annot)]
    (spit f-out (str "REPORT: " (count successes) " out of " (- n (count match-na)) "\n\n"))
    (spit f-out (str (count no-headline-ret) " FAILURES -- NO HEADLINE RETRIEVED: \n")
          :append true)
    (doseq [r no-headline-ret]
      (spit f-out (str r "\n") :append true))
    (spit f-out (str "\n\n" (count headline-mismatch) " FAILURES -- MISMATCH: \n")
          :append true)
    (doseq [r headline-mismatch]
      (spit f-out (str r "\n") :append true))
    (spit f-out (str "\n\n" (count successes) " SUCCESSES: \n") :append true)
    (doseq [r successes]
      (spit f-out (str r "\n") :append true))
    (spit f-out (str "\n\n" (count exceptions) " EXCEPTIONS: \n") :append true)
    (doseq [r exceptions]
      (spit f-out (str r "\n") :append true))
    (spit f-out (str "\n\n" (count match-na) " MATCH N/A: \n") :append true)
    (doseq [r match-na]
      (spit f-out (str r "\n") :append true))
    (spit f-out (str "\n\n" (count not-annotated) " NOT ANNOTATED: \n")
          :append true)
    (doseq [r not-annotated]
      (spit f-out (str r "\n") :append true))
    (spit f-out (str "\n\n" (count span-but-no-headline) " SPAN BUT NO HEADLINE: \n")
          :append true)
    (doseq [r span-but-no-headline]
      (spit f-out (str r "\n") :append true))
    (println (count successes) " out of " (- n (count match-na)))))