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
  "Return a list of URLs contained by this URL, if the protocol supports it."
  {:arglists '([url])}
  url-scheme)

(defmethod url-dir "file" [url]
  (map io/as-url (.listFiles (url-file url))))

(defmethod url-dir "jar" [url]
  (let [conn (.openConnection (io/as-url url))
        jar  (.getJarFile ^JarURLConnection conn)
        path (.getEntryName ^JarURLConnection conn)]
    (->> (.entries jar)
         (enumeration-seq)
         (map (memfn ^JarEntry getName))
         (filter-dir-paths path)
         (map (partial build-url url path)))))

(defn- default-loader []
  (.getContextClassLoader (Thread/currentThread)))

(defn resources
  "Returns *all* the URLs for a named resource. Uses the context class loader
  if no loader is specified."
  ([n] (resources n (default-loader)))
  ([n ^ClassLoader loader] (enumeration-seq (.getResources loader n))))

(defn resource-dir
  "Return a list of resource URLs on the classpath that have the supplied
  path prefix."
  ([path] (resource-dir path (default-loader)))
  ([path loader] (mapcat url-dir (resources path loader))))
