(ns resauce.protocols
  (:require [babashka.fs :as fs]
            [clojure.java.io :as io])
  (:import [java.io File]
           [java.net URI URL]
           [java.nio.file Path]))

(defprotocol AsURI
  (as-uri ^URI [n]
    "Coerces a given 'resource-namish' thing `n` (URL, URI, File, Path, String)
     to a `URI`."))

(defn str->uri
  "Coerces the given string `s` to a URI.

   If a URL can be constructed from `s`, e.g. when `s` starts with some scheme
   (e.g. \"file:\"), then it will be used as an interim representation for `s`.
   Otherwise, a Path will be used."
  [s]
  (when s
    (try
      (.toURI (io/as-url s))
      (catch Exception _
        (.toUri (fs/path s))))))

(extend-protocol AsURI
  URI
  (as-uri [n] n)
  URL
  (as-uri [n] (.toURI n))
  File
  (as-uri [n] (.toURI n))
  Path
  (as-uri [n] (.toUri n))
  String
  (as-uri [n] (str->uri n)))
