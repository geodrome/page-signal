(ns page-signal.core
  (:use [clojure.pprint])
  (:require [page-signal.input :as in]
            [page-signal.util :as u]
            [net.cgrand.enlive-html :as h]
            [clojure.string :as s]
            [clojure.zip :as z])
  (:import [com.github.geodrome.wordmatcher WordMatcher]))


;;; Web pages are fetched with Enlive libraries' html-resource function,
;;; which returns a nested data structure representing the HTML
;;; The following functions will facilitate further transformation on this data structure

; Currently unused but could be handy
(defn map-nodes
  "Returns root with function f applied to every node starting at root and
  proceeding via depth first traversal. If f returns nil for any tag node, its descendants
  are lost. f must return a proper node.f should expect a map or a color-theme-gandalfstring."
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
 "Returns root with function f applied to all :block tags."
 [f root]
 (map-tag f :block root))

(defn seq-blocks
  "Returns a seq of all blocks via depth-first travesrsal originating at node."
  [{:keys [tag content] :as node}]
  (if (= :block tag)
    node
    (flatten (map seq-blocks content))))

;; Unused, but could be handy
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

;; These are currently unused, but here to help me think
(def enclosing-tag? #{:h1 :h2 :h3 :h4 :h5 :h6 :p :div})


(def inline-tag? #{:b :big :i :small :tt :abbr :acronym :cite :code :dfn :em
                    :kbd :strong :samp :var :a :bdo :br :img :map :object :q
                    :span :sub :sup :button :input :label :select :textarea :font
                    :strike :u :s})

(def gap-enforcing-tags #{:h1 :h2 :h3 :h4 :h5 :h6
                          :ul :dl :ol :table :address :hr :img :script})
(def gap-avoiding-tags #{:a :b :br :em :font :i :s
                         :span :strong :sub :sup :u :tt})
;;;;;;;;;;;;;;;;;;;

;; img? - its own block since not always content when mixed with text?
;; excluding :img creates lots of "|" blocks -> img | img | img
;; may need to filter out :img from inside anchor tags
;; div and h3 inside a tag - produces :content ()
;; test for content nil and ()?
;; hr?
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

((->
            (h/select (in/file->nodes "nytimes.html") [:body])))

;;; Mark Total Word Count

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
    (assoc block :link-density 0.0)
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

;;;; Let's put it all together...
;;; we can start with get-html or get-body and we can optionally pass the tree
;;; trough remove-ignorable-tags

(defn annotate
  "Annotates dom tree."
  [nodes]
  (-> nodes
      get-html
      ;get-body
      remove-ignorable-tags
      remove-whitespace-strings
      mark-blocks
      mark-word-count
      mark-link-word-count
      mark-link-density
      mark-text-density))

(pprint (in/file->nodes "nytimes.html"))

;;; Now that everything has been annotated we can use the annotations
;;; to make decisions about what's content and what's boileplate

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
  "Takes a seq of blocks and marks each annotated block as boilerplate or content."
  [blocks]
  (map (fn [curr prev next]
         (if (boiler? curr prev next)
           (assoc curr :boiler true)
           (assoc curr :boiler false)))
       blocks
       (conj blocks {:text "" :total-words 0 :link-words 0 :link-density 0.0})
       (concat blocks [{:text "" :total-words 0 :link-words 0 :link-density 0.0}])))

;;; WHAT'S THIS?
(defn get-full-text
  "Takes text blocks that have been annotated and marked for boilerplate.
  Returns full text as a string."
  [blocks]
  (let [content (filter #(not (:boiler %)) blocks)]
    (reduce str (map :text content))))

(defn remove-boiler
  [blocks]
  (remove #(:boiler %) blocks))

(defn extract-text
  [blocks]
  (apply str (map u/text blocks)))

;;; Now Try It!

(defn get-content [nodes]
  (-> nodes
      annotate
      ;seq-blocks
      ;mark-boiler
      ;remove-boiler
      ;extract-text
      ))

(defn get-content-url [url]
  (get-content (in/url->nodes url)))

(defn get-content-file [file]
  (get-content (in/file->nodes file)))

(pprint (get-content-file "nytimes.html"))

(def urls {:a "http://www.scientificamerican.com/article.cfm?id=bacteria-discovered-spacecraft-clean-rooms&WT.mc_id=SA_sharetool_Twitter"
           :b "http://citusdata.com/blog/72-linux-memory-manager-and-your-big-data"
           :c "http://mortoray.com/2013/11/27/the-string-type-is-broken/"
           :d "http://www.nytimes.com/2013/11/28/us/politics/years-delay-expected-in-major-element-of-health-law.html?_r=0"})

(pprint (get-content-url (:d urls)))
