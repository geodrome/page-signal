(ns page-signal.util
  (:use [clojure.pprint])
  (:require [page-signal.input :as in]
            [net.cgrand.enlive-html :as h]
            [clojure.string :as s])
  (:import [com.github.geodrome.wordmatcher WordMatcher]))

(defn count-matches
  "a1 and a2 should be arrays of strings. Returns the number of matching strings... per algo"
  [^objects a1 ^objects a2]
  (if (or (nil? a1)
          (nil? a2)
          (zero? (alength a1))
          (zero? (alength a2)))
    0
    (WordMatcher/count a1 a2)))

;;; Block Processing Helpers

(defn text*
  "Performs depth first traversal of root and returns all decendants of root that are strings in nested vectors."
  [root]
  (let [children (:content root)]
    (reduce (fn [str-vec child]
              (if (string? child)
                (conj str-vec child)
                (conj str-vec (text* child))))
            []
            children)))

(defn text
  "Performs depth first traversal of root and returns a lazy seq of all decendants of root that are strings. Consider storing :full-text as block attribute. Eliminate spacing when dealing with inline elements? Separates with sep"
  ([root]
     (text root " "))
  ([root sep]
     (s/join sep (flatten (text* root)))))

; q tag should encapsulate in quotation marks, pre tag doesn't ignore whitespace
(defn convert-break-tags
  "Replaces line breaking tags with newline strings."
  [{:keys [tag] :as node}]
  (cond
   (#{:br} tag) "\n"
   (#{:img :map :hr} tag) "\n\n"
   :else node))

; doesn't work for block level elements
(defn text
  "Performs depth first traversal of root and returns a lazy seq of all decendants of root that are strings. Consider storing :full-text as block attribute. Eliminate spacing when dealing with inline elements? "
  [node]
  (cond
   (string? node) node
   (map? node) (->> node
                    :content
                    (map convert-break-tags ,,,)
                    (map text ,,,)
                    (remove empty? ,,,) ;; necessary?
                    (apply str ,,,)
                    )
   :else ""))

(defn clean-string
  "is trim correct? i think it is - how the browser seems to do it"
  [st]
  (let [txt (s/replace st #"[ \t\n\x0B\f\r]+" " ")]
    (when-not (s/blank? txt)
      (s/trim txt))))

;(re-seq #"\w+" "Obama Signals He'd Let Cuts Stand to Avoid U.S. Shutdown - NYTimes.com")
; #"[a-zA-Z_0-9|.|']+"
(defn words [s] (re-seq #"\w+" s))

(defn count-words [s] (count (words s)))

;;; Testing Word Matchin Performance

(def pool "ABC")
(defn get-random-id [n] (apply str (repeatedly n #(rand-nth pool))))
(def a1 (into-array (take 10000 (repeatedly #(get-random-id 5)))))
(def a2 (into-array (take 10000 (repeatedly #(get-random-id 5)))))
