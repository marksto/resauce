(ns resauce.core-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [resauce.core :refer :all]))

(deftest test-directory?
  (is (directory? (io/resource "resauce")))
  (is (directory? (io/resource "clojure")))
  (is (not (directory? (io/resource "resauce/core.clj"))))
  (is (not (directory? (io/resource "clojure/core.clj"))))
  (is (not (directory? nil))))

(deftest test-resources
  (let [rs (sort (map str (resources "resauce")))]
    (is (= 2 (count rs)))
    (is (re-find #"src/resauce$" (first rs)))
    (is (re-find #"test/resauce$" (second rs)))))

(deftest test-url-dir
  (testing "file URL"
    (let [rs (sort (map str (url-dir (io/as-url (io/file "src/resauce")))))]
      (is (= 2 (count rs)))
      (is (re-matches #"file:.*src/resauce/core\.clj" (first rs)))
      (is (re-matches #"file:.*src/resauce/protocols\.clj" (second rs)))))
  (testing "jar URL"
    (let [rs (sort (map str (url-dir (io/resource "medley"))))]
      (is (= 3 (count rs)))
      (is (re-matches #"jar:.*medley/core\.clj" (first rs)))
      (is (re-matches #"jar:.*medley/core\.cljs" (second rs)))
      (is (re-matches #"jar:.*medley/core\.cljx" (nth rs 2))))))

(deftest test-resource-dir
  (let [rs (sort (map str (resource-dir "resauce")))]
    (is (= 3 (count rs)))
    (is (re-find #"src/resauce/core\.clj$" (first rs)))
    (is (re-find #"src/resauce/protocols\.clj$" (second rs)))
    (is (re-find #"test/resauce/core_test\.clj$" (nth rs 2)))))
