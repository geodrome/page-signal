(ns page-signal.core
  (:use [clojure.pprint])
  (:require [page-signal.input :as in]
            [page-signal.util :as u]
            [net.cgrand.enlive-html :as h]
            [clojure.string :as s]
            [clojure.zip :as z])
  (:import [com.github.geodrome.wordmatcher WordMatcher]))

(set! *warn-on-reflection* true)

;;; Tree-processing

(defn map-nodes
  "Returns root with function f applied to every node starting at root and proceeding via depth first traversal. If f returns nil for any tag node, its descendants are lost. f must return a proper node.f should expect a map or a color-theme-gandalfstring."
  [f root]
  (if-let [{:keys [content] :as froot} (f root)]
    (if content
      (assoc froot :content (map (partial map-nodes f) content))
      froot)))

(defn map-tag-nodes
  "Returns root with function f applied to every tag node starting at root and proceeding via depth first traversal. Non-tag nodes (i.e. strings) are returned unchaged. If f returns nil for any tag node, its descendants are lost."
  [f root]
  (if (map? root)
    (if-let [{:keys [content] :as froot} (f root)]
      (if content
        (assoc froot :content (map (partial map-tag-nodes f) content))
        froot))
    root))

(defn map-string-nodes
  "Returns root with function f applied to every string starting at root and proceeding via depth first traversal."
  [f {:keys [content] :as root}]
  (if (string? root)
    (f root)
    (if content
      (assoc root :content (map (partial map-string-nodes f) content))
      root)))

(defn map-tag
  "Returns root with function f applied to all t tag nodes starting at root and proceeding via depth first traversal. Function f takes a single argument (a block)."
  [f t {:keys [tag content] :as root}]
  (if (= t tag)
    (f root)
    (if content
      (assoc root :content (map (partial map-tag f t) content))
      root)))

(defn map-blocks
 [f root]
 (map-tag f :block root))

(defn seq-blocks
  "Returns a seq of all blocks via depth-first travesrsal originating at node."
  [{:keys [tag content] :as node}]
  (if (= :block tag)
    node
    (flatten (map seq-blocks content))))

(defn seq-tags
  [node]
  (tree-seq map? :content node))

(defn seq-strings
  [{:keys [content] :as node}]
  (if (string? node)
    node
    (flatten (map seq-strings content))))

(defn prune-nils
  "Removes nils from :content of every tag node."
  [root]
  (map-tag-nodes (fn [{:keys [content] :as node}]
                   (let [clean (remove nil? content)]
                     (if (empty? clean)
                       (assoc node :content nil)
                       (assoc node :content clean))))
             root))

;;; Remove ignorable tags

(def ignorable-tag? #{:style :script :noscript :object :embed :applet :link :form :input
                      :button :label :fieldset :legend :select :textarea :optgroup
                      :option :datalist :keygen :output})

(defn remove-ignorable-tags
  [root]
  (->> root
       (map-tag-nodes (fn [{:keys [tag] :as node}]
                        (when-not (ignorable-tag? tag)
                          node)))
       prune-nils))

;;; Clean strings

(defn remove-whitespace-strings
  [root]
  (->> root
       (map-string-nodes u/clean-string)
       prune-nils))

;;; Identifying Atomic Text Blocks

(def enclosing-tag? #{:h1 :h2 :h3 :h4 :h5 :h6 :p :div})


(def inline-tag? #{:b :big :i :small :tt :abbr :acronym :cite :code :dfn :em
                    :kbd :strong :samp :var :a :bdo :br :img :map :object :q
                    :span :sub :sup :button :input :label :select :textarea :font
                    :strike :u :s})

(def gap-enforcing-tags #{:h1 :h2 :h3 :h4 :h5 :h6
                          :ul :dl :ol :table :address :hr :img :script})
(def gap-avoiding-tags #{:a :b :br :em :font :i :s
                         :span :strong :sub :sup :u :tt})

;; img? - its own block since not always content when mixed with text?
;; excluding :img creates lots of "|" blocks -> img | img | img
;; may need to filter out :img from inside anchor tags
;; empty a tags and other tags
;; div and h3 inside a tag - produces :content ()
;; test for content nil and ()?
;; hr?
;; html comments?
;should li be a block-tag?

(def block-tag? #{:b :big :i :small :tt :abbr :acronym :cite :code :dfn :em
                    :kbd :strong :samp :var :a :bdo :br :q
                    :span :sub :sup :font :strike :u :s :img :map})

(defn block-item?
  "Must return strictly true or false for partition-by to work."
  [{:keys [tag] :as node}]
  (or (string? node) (contains? block-tag? tag)))

(declare mark-blocks)

(defn process-part
  "p is a seq of nodes that are expected to either all be block nodes or all be non-block nodes. Seqs of atomic blocks are cleaned up and wrapped in a block tag. Seqs of non-block nodes are recursively processed by partition-and-process."
  [etag p]
  (if (block-item? (first p))
    {:tag :block :enclosing-tag etag :content p}
    (map mark-blocks p)))

(defn mark-blocks
  "Partitions child nodes of root into block nodes / non-block nodes and recursively submits each partition to be processed by process-part. Assumes root is a non-block node."
  [{:keys [tag content] :as root}]
  (if content
    (let [parts (partition-by block-item? content)]
      (assoc root :content (-> (map (partial process-part tag) parts)
                               flatten)))
    root))

(defn most-content
  [{cont1 :content :as node1}
   {cont2 :content :as node2}]
  (when (and cont1 cont2)
    (if (> (count cont1) (count cont2))
      node1
      node2)))

(defn get-body
  "Takes nodes produced by enlive/html-resource and extracts the body. Consider adding body, if doesn't exist in doc."
  [nodes]
  (let [body-tags (h/select nodes [:body])
        bt-count (count body-tags)]
    (cond
     (zero? bt-count)
     (throw (Exception. "There is no body tag!"))

     (= 1 bt-count)
     (first body-tags)

     (> 1 bt-count) ; never get to the else on purpose
     (throw (Exception. "More than one body tag!"))

     :else
     (let [body (reduce most-content body-tags)]
       (if body
         body
         (throw (Exception. "There is no content in any body tag!")))))))

(defn get-html
  "Critical to remove title tag in order for headline extraction to work properly."
  [nodes]
  (first (h/at (h/select nodes [:html]) [:title] nil)))

;;; Mark Total Word Count


(defn mark-word-count*
  "Handle HR tag"
  [{:keys [content] :as block}]
  (reduce (fn [block node] ;; assumes node is a string or anchor tag - WRONG
            (if (string? node)
              (merge-with + block {:total-words (u/count-words node)
                                   :link-words 0})
              (let [word-count (u/count-words (u/text node))]
                (merge-with + block {:total-words word-count
                                     :link-words word-count}))))
          block content))

(defn mark-word-count*
  [block]
  (merge block {:total-words (u/count-words (u/text block))}))

(defn mark-word-count
  "For each block counts total number of words and number of words in anchor text and stores these values under :total-words and :link-words respectively."
  [root]
  (map-blocks mark-word-count* root))

;;; Mark Link Word Count

(defn count-link-words
  [{:keys [tag content] :as node}]
  (if (= :a tag)
    (u/count-words (u/text node)) ;; assumes no :a nested in :a
    (let [a-tags (filter #(= :a (:tag %)) content)
          non-a-tags (filter #(and (map? %) (not= :a (:tag %))) content)]
      (+ (reduce + (map #(u/count-words (u/text %)) a-tags))
         (reduce + (map count-link-words non-a-tags))))))

(defn mark-link-word-count*
  [{:keys [enclosing-tag content] :as block}]
  (if (= :a enclosing-tag)
    (merge block {:link-words (u/count-words (u/text block))})
    (merge block {:link-words (reduce + (map count-link-words content))})))

(defn mark-link-word-count
  [root]
  (map-blocks mark-link-word-count* root))

;;; Mark Link Density

(defn mark-link-density*
  [{:keys [total-words link-words] :as block}]
  (if (zero? total-words)
    (assoc block :link-density :na) ;; reconsider this later
    (assoc block :link-density
                 (double (/ link-words total-words)))))

(defn mark-link-density
  "For each block calculates link density and stores under :link-density."
  [root]
  (map-blocks mark-link-density* root))

;;; Mark Text Density

(defn place-word
  "'Places' word on a line at give wrap-width. Returns a vector of three elements: number of full lines, number of chars on last line, and number of words on last line."
  [wrap-width [full-lines last-line-chars last-line-words] ^String word]
  (let [word-len (.length word)
        last-line-chars (+ last-line-chars (inc word-len))]
    (if (<= last-line-chars wrap-width)
      ;; place on existing line
      [full-lines last-line-chars (inc last-line-words)]
      ;; place on new line
      [(inc full-lines) word-len 1])))

(defn calc-text-density
  ([txt]
     (calc-text-density txt 80))
  ([txt wrap-width]
  (let [words (u/words txt)
        [full-lines _ last-line-words] (reduce (partial place-word wrap-width)
                                               [0 0 0] words)
        word-count (count words)
        words-on-full-lines (- word-count last-line-words)]
    (if (zero? full-lines)
      (double word-count)
      (double (/ words-on-full-lines full-lines))))))

(defn mark-text-density*
  [block]
  (assoc block :text-density
         (calc-text-density (u/text block))))

(defn mark-text-density
  [root]
  (map-blocks mark-text-density* root))

;;; Extract Article Title

;; getting non-title-tag title

(defn headline-class?
  [^String class]
  (let [hclass #{"title" "headline"}]
    (when (and class
               (reduce #(or %1 (.contains class %2)) false hclass)) ; short-circuit
      true)))

(defn cand?
  [loc]
  (let [node (z/node loc)]
    (when (and (map? node)
               (or (#{:h1 :h2 :h3 :HEADLINE} (:tag node))
                   (headline-class? (-> node :attrs :class))
                   (headline-class? (-> node :attrs :id))))
      true)))

(defn find-cands*
  [loc cands]
  (cond
   (z/end? loc) cands
   (cand? loc) (recur (z/next loc) (conj cands loc))
   :else (recur (z/next loc) cands)))

(defn find-cands [loc] (find-cands* loc nil))

(defn see-cands [cands]
  (pprint (map #(z/node %) cands)))

(defn non-title-tag-title
  [loc]
  (let [cand-locs (filter #(zero? (count-link-words (z/node %)))
                      (find-cands loc))
        titles (remove s/blank? (map #(u/text (z/node %)) cand-locs))]
    (first titles))) ;; for now

;; getting title tag matching titles

(defn matching-title
  "more than one match is rare, so returning first is okay"
  [^String titles ^String s]
  (first (remove nil? (map #(when (.equals s %) s) titles))))

(defn longer-string
  [^String s1 ^String s2]
  (if (> (.length s2) (.length s1)) s2 s1))

(defn title-match
  "Checks every string in a block and returns an exact match from among titiles."
  [block titles]
  (let [match (reduce
                  longer-string
                  ""
                  (remove nil? (map (partial matching-title titles)
                                    (seq-strings block))))]
    (when-not (= "" match) match)))

(defn n-frags*
  [n frags seps res]
  (if (>= (count frags) n)
    (let [word-group (take n frags)
          sep (take n (cycle (concat seps [""])))]
      (recur n (rest frags) (rest seps)
             (conj res (butlast (interleave word-group sep)))))
    res))

(defn n-frags
  "Return a seq of all strings that are combinations of n fragments interleaved with corresponding separators."
  [n frags seps]
  (map s/trim (map s/join (n-frags* n frags seps []))))

(defn potential-titles
  "Given title tag text, return substrings of title likely to be the page title."
  [title]
  (let [sep-re #"[:|\-»,/]"
        frags (s/split title sep-re)
        seps (re-seq sep-re title)]
    (mapcat n-frags
            (map inc (range (count frags)))
            (repeat frags)
            (repeat seps))))

;; when two matches of decen length attmept further logic (e.g.
;; location - proximity to main text, enclosing tag)
(defn best-matching-block
  "Return the block from candidates most likely to contain the page title."
  [candidates]
  (reduce (fn [b1 b2]
            (if (> (.length (:title-text b2))
                   (.length (:title-text b1)))
              b2
              b1))
          candidates))

(defn mark-title-match
  [root title]
  (let [titles ( title)]
    (map-blocks (fn [block]
                  (let [title-text (title-match block titles)]
                    (assoc block :title-text title-text)))
                root)))

; get from meta tag
(defn headline-block
  "Return block most likely to contain the headline."
  [nodes]
  (let [candidates (filter #(:title-text %)
                           (seq-blocks nodes))
        num-cand (count candidates)
        r {:title-text (non-title-tag-title (z/xml-zip nodes))}]
    (cond
     (zero? num-cand)
     r

     (= 1 num-cand)
     (if r
       r
       (first candidates))

     :else
     (best-matching-block candidates))))

(defn doc-title-text
  [nodes]
  (u/text (first (h/select nodes [:title]))))

;;; Testing

;(def dummy (in/file->nodes "test.html"))
;(def obama (in/file->nodes "obama.html"))

(defn process
  [nodes]
  (let [title (doc-title-text nodes)]
    (-> nodes
        get-html
        ;get-body
        ;remove-ignorable-tags
        remove-whitespace-strings
        mark-blocks
        mark-word-count
        mark-link-word-count
        mark-link-density
        mark-text-density
        (mark-title-match title))))

(defn see
  [nodes]
  (pprint (process nodes)))
