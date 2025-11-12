(ns resauce.test.utils
  (:require [babashka.fs :as fs]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [resauce.test.data :as td])
  (:import [java.io File]
           [java.net URI URL]
           [java.nio.file Path]))

(defn- str->uri [^String s]
  (when s
    (try
      (.toURI (io/as-url s))
      (catch Exception _
        (.toUri (fs/path s))))))

;;; FS Files/Dirs

(defn- prefixed-w-file? [f]
  (str/starts-with? (str f) "file:"))

(defn- prefix-with-file [file-path]
  (str "file:" file-path))

(defn- strip-file-prefix [f]
  (subs (str f) (count "file:")))

(defn- ->file-reprs
  "File | Path | URL | URI | \"...\" -> [File
                                         Path
                                         URL
                                         URI
                                         \"...\"
                                         \"file:...\"]"
  [f]
  ((cond
     (instance? File f)
     (juxt identity
           fs/path
           io/as-url
           (memfn ^File toURI)
           str
           prefix-with-file)

     (instance? Path f)
     (juxt fs/file
           identity
           (comp io/as-url fs/file)
           (memfn ^Path toUri)
           str
           prefix-with-file)

     (instance? URL f)
     (juxt fs/file
           fs/path
           identity
           (memfn ^URL toURI)
           strip-file-prefix
           str)

     (instance? URI f)
     (juxt fs/file
           fs/path
           (memfn ^URI toURL)
           identity
           strip-file-prefix
           str)

     (string? f)
     (juxt fs/file
           fs/path
           (comp io/as-url fs/file)
           str->uri
           (if (prefixed-w-file? f) strip-file-prefix identity)
           (if (prefixed-w-file? f) identity prefix-with-file)))
   f))

(defn fs-files []
  (mapcat ->file-reprs (td/some-fs-files)))

(defn- ->dir-endings [f]
  ((juxt identity #(str % "/")) (str f)))

(defn correct-fs-dirs []
  (->> (td/some-fs-dirs)
       (mapcat ->dir-endings)
       (mapcat ->file-reprs)
       (distinct)))

(defn incorrect-fs-dirs []
  (->> (td/some-fs-dirs)
       (mapcat ->dir-endings)
       (map prefix-with-file)
       (mapcat (juxt fs/file
                     fs/path))
       (distinct)))

;;; JARs

;; NB: Syntax is from `java.net.JarURLConnection`.
(def jar-url-re #"jar:(?<url>[^!]+)!/(?<entry>.+)?")

(defn- ->jar-file
  "Yields 'file:/<...>.jar' string."
  [^URL jar-url]
  (when-some [[_ url _entry] (re-matches jar-url-re (str jar-url))]
    url))

(defn- ->jar-file-w-trailing-bang
  "Yields 'file:/<...>.jar!' string."
  [^URL jar-url]
  (str (->jar-file jar-url) "!"))

(defn- ->jar-root-dir
  "Yields 'jar:file:/<...>.jar!/' string."
  [^URL jar-url]
  (str "jar:" (->jar-file jar-url) "!/"))

(defn- ->jar-meta-inf-dir
  "Yields 'jar:file:/<...>.jar!/META-INF' string."
  [^URL jar-url]
  (str "jar:" (->jar-file jar-url) "!/META-INF"))

(defn files-in-jars []
  (mapcat (juxt str
                identity
                (comp str->uri str))
          (td/some-jar-file-urls)))

(defn- ->jar-file-reprs
  "URL | URI | \"...\" -> [URL
                           URI
                           \"...\"]"
  [f]
  ((cond
     (instance? URL f)
     (juxt identity
           (memfn ^URL toURI)
           str)

     (instance? URI f)
     (juxt (memfn ^URI toURL)
           identity
           str)

     (string? f)
     (juxt io/as-url
           str->uri
           identity))
   f))

(defn jar-files [trailing-bang?]
  (->> (td/some-jar-dir-urls)
       (map (if trailing-bang?
              ->jar-file-w-trailing-bang
              ->jar-file))
       (mapcat ->jar-file-reprs)))

(defn correct-jar-dirs []
  (->> (td/some-jar-dir-urls)
       (mapcat (juxt str
                     ->jar-meta-inf-dir))
       (mapcat ->jar-file-reprs)))

(defn incorrect-jar-dirs []
  (->> (td/some-jar-dir-urls)
       (map ->jar-root-dir)
       (mapcat ->jar-file-reprs)))
