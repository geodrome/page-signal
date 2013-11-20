(ns page-signal.input
  (:require [net.cgrand.enlive-html :as h])
  (:import [de.l3s.boilerpipe]
           [java.io.ByteArrayInputStream]
           [org.xml.sax.InputSource]))

;;; Text docs from Java Boilerpipe lib

(defn str->text-doc
  [s]
  (.getTextDocument
   (de.l3s.boilerpipe.sax.BoilerpipeSAXInput.
    (.toInputSource
     (de.l3s.boilerpipe.sax.HTMLDocument. s)))))

(defn url->text-doc
  [url]
  (.getTextDocument
   (de.l3s.boilerpipe.sax.BoilerpipeSAXInput.
    (.toInputSource
     (de.l3s.boilerpipe.sax.HTMLFetcher/fetch (java.net.URL. url))))))

(defn file->text-doc
  [f]
  (-> f
      slurp
      str->text-doc))

(defn print-text-doc
  [td]
  (doseq [x (.getTextBlocks td)] (println x)))

;;; Creating Nodes For Clojure lib
;; TODO can write function nodes that will do dispatch, but try clojure polymorphism
(defn url->nodes
  [url]
  (h/html-resource (java.net.URL. url)))

(defn str->nodes
  [s]
  (h/html-resource (java.io.StringReader. s)))

(defn file->nodes [f]
  ;; can use h/html-resouce directly
  (-> f
      slurp
      str->nodes))
