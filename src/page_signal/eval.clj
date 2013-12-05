(ns page-signal.eval
  (:use [clojure.pprint]
        [page-signal.test-data]) ;; since data take a while to load we separate it out
  (:require [page-signal.input :as in]
            [page-signal.util :as u]
            [page-signal.core :as c]
            [clojure.string :as s]
            [clojure.zip :as z]
            [net.cgrand.enlive-html :as h]
            [incanter.stats :as stats])
  (:import [java.io File]))

;;; to improve precision only retrieve relevant text
;;; to improve recall don't miss any of the relevant text

;;;; Evaluate Full Text

(defn get-sample
  "Returns a sample of n files from dir."
  [dir n]
  (map #(.getName %) (stats/sample (.listFiles (File. dir))
                                   :size n :replacement false)))


(defn text->array
  "Splits text into words, converts the words to lower case, and sticks them in a Java array."
  [text]
  (when-not (empty? text)
    (-> text
        u/clean-string
        (#(re-seq #"\w+" %))
        (#(map s/lower-case %))
        into-array)))


(defn score
  "Calculates precision, recall, and f1 score."
  [file-name]
  (println "Scoring: " file-name)
  (let [retrieved (-> (get orig-nodes file-name)
                      c/get-content
                      text->array)
        relevant (-> (get annot-nodes file-name)
                     ;(#(h/select % [:body #{(:headline annotations)
                      ;                      (:full-text annotations)}]))
                     (#(h/select % [:body #{(:full-text annotations)}]))
                     (#(apply str (map (fn [x] (first (:content x))) %)))
                     text->array)
        ret-count (count retrieved)
        rel-count (count relevant)
        match-count (u/count-matches retrieved relevant)
        res {:file file-name}]
    (cond
     (and (zero? ret-count) (zero? rel-count))
     (assoc res :precision :inf :recall :inf :f1 :nan)

     (zero? ret-count)
     (assoc res :precision :inf :recall 0 :f1 :nan)

     (zero? rel-count)
     (assoc res :precision 0 :recall :inf :f1 :nan)

     (zero? match-count)
     (assoc res :precision 0 :recall 0 :f1 :inf)

     :else
     (let [precision (/ match-count ret-count)
           recall (/ match-count rel-count)
           f1 (* 2 (/ (* precision recall)
                      (+ precision recall)))]
       (assoc res
         :precision (double precision)
         :recall (double recall)
         :f1 (double f1))))))


(defn eval-full-text
  [n f-out]
  "Evaluates the algorithm on a sample of n files and writes the results to f-out."
  (let [files (get-sample orig-dir n)
        results (map score files)
        results-clean (filter #(number? (:f1 %)) results)
        results-special (filter #(keyword? (:f1 %)) results)
        f1s  (map :f1 results-clean)
        avg (/ (reduce + f1s) (count f1s))]
    (spit f-out (str "Evaluated " n " files.\nMEAN F1 SCORE: " (format "%.2f" avg) "\n===============================================\n"))
    (doseq [r results-clean]
      (spit f-out (str "F1: " (format "%.2f" (:f1 r))
                       " Precision: " (format "%.2f" (:precision r))
                       " Recall: " (format "%.2f" (:recall r))
                       " File: " (:file r) "\n")
            :append true))
    (doseq [r results-special]
      (spit f-out (str "F1: " (:f1 r)
                       " Precision: " (:precision r)
                       " Recall: " (:recall r)
                       " File: " (:file r) "\n")
            :append true))
    (println "MEAN: " (format "%.2f" avg))))
