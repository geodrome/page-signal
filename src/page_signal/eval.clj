(ns page-signal.eval
  (:use [clojure.pprint])
  (:require [page-signal.input :as in]
            [page-signal.util :as u]
            [page-signal.core :as c]
            [clojure.string :as s]
            [clojure.zip :as z]
            [net.cgrand.enlive-html :as h]
            [incanter.stats :as stats])
  (:import [java.io File]))

;; These are the directories where original and annotated
;; pages of the dataset are located
(def orig-dir "L3S-GN1-20100130203947-00001/original/")
(def annot-dir "L3S-GN1-20100130203947-00001/annotated/")

;; Pages are annotated by wrapping sections
;; with :span tags of different classes
(def annotations {:not-content :span.x-nc-sel0
                  :headline :span.x-nc-sel1
                  :full-text :span.x-nc-sel2
                  :supplemental :span.x-nc-sel3
                  :related-content :span.x-nc-sel4
                  :comments :span.x-nc-sel5})


(def orig-files (.listFiles (File. orig-dir)))
(def orig-nodes (zipmap (map #(.getName %) orig-files)
                              (map #(in/file->nodes %) orig-files)))

(def annot-files (.listFiles (File. annot-dir)))
(def annot-nodes (zipmap (map #(.getName %) annot-files)
                         (map #(in/file->nodes %) annot-files)))
;; Requires a lot of heap space

(def file-names (map #(.getName %) orig-files))

(defn get-span
  [nodes span-set]
  (h/select nodes [:body span-set]))


;;; Evaluate Headline

(defn annot-headline
  [nodes]
  (let [span (-> nodes
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
  [nodes]
  (try (-> nodes
           c/process
           c/headline-block
           :title-text)
       (catch Exception e {:exception (.getMessage e)})))

(def h-annot (map (fn [fname]
                       {:file fname
                        :headline (annot-headline (annot-nodes fname))}) file-names))

(defn eval-headline
  [f-out]
  (let [n (count file-names)
        h-retriv (map (fn [fname]
                        {:file fname
                         :headline (headline (orig-nodes fname))}) file-names)
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

;;;; Evaluate Full Text

(defn get-sample
  "Returns a sample of n files, as a lazy seq, from dir with or without replacement."
  [^String dir n]
  (stats/sample (.listFiles (File. dir))
                :size n :replacement false))

(defn files-from-dir
  "Takes a seq of File objects (files) and returns a seq of files with the same name from dir directory."
  [dir files]
  (map (fn [^java.io.File f]
         (File. (str dir (.getName f))))
       files))

(defn str->seq [s] (re-seq #"\w+" s))

(defn seq->lower [s] (map s/lower-case s))

(defn text->array
  ""
  [text]
  (-> text
      u/clean-string
      str->seq
      seq->lower
      into-array))

(defn annot-full-text
  [f]
  (-> f
      in/file->nodes
      (get-span #{(:headline annotations)
                  (:full-text annotations)})
      u/text))

(defn full-text
  [] "")

(defn score
  [file-orig file-annot]
  (println "orign: " file-orig)
  (println "annot: " file-annot)
  (let [retrieved (text->array (full-text file-orig)) ;; write full-text
        relevant (text->array (annot-full-text file-annot)) ;;; #### THIS STEP SHOULDNT BE REPEATED ALL THE TIME
        ret-count (count retrieved)
        rel-count (count relevant)
        match-count (u/count-matches retrieved relevant)
        res {:file (.getName file-orig)}]
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

;;; MODIFY TO USE IN MEMORY FILE DATA
(defn eval-full-text
  [n f-out]
  "n is the number of files to evaluate against."
  (let [orig (get-sample orig-dir n)
        annot (files-from-dir annot-dir orig)
        results (map score orig annot)
        results-clean (filter #(number? (:f1 %)) results)
        results-special (filter #(keyword? (:f1 %)) results)
        f1s  (map :f1 results-clean)
        avg (/ (reduce + f1s) (count f1s))]
    (spit f-out (str "Evaluated " n " files...\n"))
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
