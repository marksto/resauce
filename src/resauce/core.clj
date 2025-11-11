(ns resauce.core
  (:require [babashka.fs :as fs]
            [clojure.java.io :as io]
            [resauce.protocols :refer [as-uri]])
  (:import [java.io File]
           [java.net JarURLConnection URI URL]
           [java.util.jar JarEntry JarFile]
           [java.util.regex Pattern]))

(defn- add-ending-slash [^String s]
  (if (.endsWith s "/") s (str s "/")))

(defn- filter-dir-paths [dir paths]
  (let [re (re-pattern (str (Pattern/quote (add-ending-slash dir)) "[^/]+/?"))]
    (filter (partial re-matches re) paths)))

(defn- build-url [base-url dir path]
  {:pre [(.startsWith ^String path dir)]}
  (URL. (str (add-ending-slash (str base-url))
             (subs path (count (add-ending-slash dir))))))

(defn- url-scheme ^String [n]
  ;; Using URI instead of URL to support arguments without schema.
  (when n (.getScheme ^URI (as-uri n))))

(defn- ^File url-file [url]
  (File. ^String (.getPath (io/as-url url))))

(defmulti directory?
  "Returns true if a given 'resource-namish' thing `n` (URL, URI, File, String)
   points to an existing directory (in the file system or inside a JAR file).

   NB: Keep in mind that this function will return `false` for regular files."
  {:arglists '([n])}
  url-scheme)

(defmethod directory? "file" [n]
  (let [path (fs/path (as-uri n))]
    (and (fs/exists? path)
         (fs/directory? path))))

(defmethod directory? "jar" [n]
  (let [url-conn ^JarURLConnection (.openConnection (io/as-url n))
        jar-file ^JarFile (.getJarFile url-conn)
        entry-name (.getEntryName url-conn)]
    (boolean
      (when (and jar-file entry-name)
        (some-> jar-file
                (.getEntry (add-ending-slash entry-name))
                (.isDirectory))))))

(defmethod directory? :default [_]
  false)

(defmulti url-dir
  "Returns a list of URLs contained by a given 'resource-namish' thing `n` (URL,
   URI, File, String)."
  {:arglists '([n])}
  url-scheme)

(defmethod url-dir "file" [n]
  (map #(io/as-url (.toFile %)) (fs/list-dir n)))

(defmethod url-dir "jar" [n]
  (let [url-conn ^JarURLConnection (.openConnection (io/as-url n))
        jar-file ^JarFile (.getJarFile url-conn)
        entry-name (.getEntryName url-conn)]
    (when (and jar-file entry-name)
      (->> (.entries jar-file)
           (enumeration-seq)
           (map (memfn ^JarEntry getName))
           (filter-dir-paths entry-name)
           (map #(build-url n entry-name %))))))

(defmethod url-dir :default [_])

(defn- default-loader []
  (.getContextClassLoader (Thread/currentThread)))

(defn resources
  "Returns *all* the URLs for a resource with the given `name` on the classpath.
   Uses the context class loader if no loader is specified."
  ([^String name] (resources name (default-loader)))
  ([^String name ^ClassLoader loader]
   (enumeration-seq (.getResources loader name))))

(defn resource-dir
  "Returns a list of resource URLs on the classpath that have the `path` prefix.
   Uses the context class loader if no `loader` is specified."
  ([^String path] (resource-dir path (default-loader)))
  ([^String path ^ClassLoader loader]
   (mapcat url-dir (resources path loader))))
