(ns page-signal.test-data
  (:require [page-signal.input :as in])
  (:import [java.io File]))

;; These are the directories where original and annotated
;; pages of the dataset are located
(def orig-dir "L3S-GN1-20100130203947-00001/original/")
(def annot-dir "L3S-GN1-20100130203947-00001/annotated/")


;; Get the nodes
(def orig-files (.listFiles (File. orig-dir)))
(def orig-nodes (zipmap (map #(.getName %) orig-files)
                        (map #(in/file->nodes %) orig-files)))

(def annot-files (.listFiles (File. annot-dir)))
(def annot-nodes (zipmap (map #(.getName %) annot-files)
                         (map #(in/file->nodes %) annot-files)))

(def file-names (map #(.getName %) orig-files))

;; Pages are annotated by wrapping with :span tags of different classes
(def annotations {:not-content :span.x-nc-sel0
                  :headline :span.x-nc-sel1
                  :full-text :span.x-nc-sel2
                  :supplemental :span.x-nc-sel3
                  :related-content :span.x-nc-sel4
                  :comments :span.x-nc-sel5})
