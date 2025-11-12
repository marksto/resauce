(ns resauce.test.data
  "Use some concrete resources that we know live on the classpath or somewhere
   in the file system, either as regular files/directories, or inside some JAR."
  (:require [babashka.fs :as fs]
            [clojure.java.io :as io])
  (:import [java.net URL]
           [java.nio.file Path]))

;;; FS Files/Dirs

(defn some-file-path ^Path []
  (->> (fs/list-dir (fs/home))
       (filter fs/regular-file?)
       (first)))

(defn some-file-url ^URL []
  (io/resource "resauce/core.clj"))

(defn some-fs-files []
  [(some-file-path)
   (some-file-url)])

(defn some-fs-dirs []
  [(str (fs/home))
   (str (io/as-file (io/resource "resauce")))])

;;; JARs

(defn some-jar-file-urls []
  [(io/resource "clojure/core.clj")
   (io/resource "META-INF/leiningen/medley/medley/README.md")])

(defn some-jar-dir-urls []
  [(io/resource "clojure")
   (io/resource "medley")])
