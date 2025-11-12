(ns resauce.core-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [resauce.core :refer :all]
            [resauce.test-utils :as tu]))

(deftest test-inside-jar?
  (testing "corner cases"
    (is (not (inside-jar? nil))))
  (testing "regular files"
    (is (every? false? (map inside-jar? (tu/fs-files)))))
  (testing "regular directories"
    (is (every? false? (map inside-jar? (tu/correct-fs-dirs))))
    (is (every? false? (map inside-jar? (tu/incorrect-fs-dirs)))))
  (testing "JAR file itself"
    (is (every? false? (map inside-jar? (tu/jar-files false))))
    (is (every? false? (map inside-jar? (tu/jar-files true)))))
  (testing "resources inside a JAR file"
    (is (every? true? (map inside-jar? (tu/files-in-jars))))
    (is (every? true? (map inside-jar? (tu/correct-jar-dirs))))
    (is (every? true? (map inside-jar? (tu/incorrect-jar-dirs))))))

(deftest test-directory?
  (testing "corner cases"
    (is (not (directory? nil))))
  (testing "regular files + JAR file itself"
    (is (every? false? (map directory? (tu/fs-files)))))
  (testing "regular directories"
    (is (every? true?  (map directory? (tu/correct-fs-dirs))))
    (is (every? false? (map directory? (tu/incorrect-fs-dirs)))))
  (testing "JAR file itself"
    (is (every? false? (map directory? (tu/jar-files false))))
    (is (every? false? (map directory? (tu/jar-files true)))))
  (testing "resources inside a JAR file"
    (is (every? false? (map directory? (tu/files-in-jars))))
    (is (every? true?  (map directory? (tu/correct-jar-dirs))))
    (is (every? false? (map directory? (tu/incorrect-jar-dirs))))))

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
    (is (= 5 (count rs)))
    (is (re-find #"src/resauce/core\.clj$" (first rs)))
    (is (re-find #"src/resauce/protocols\.clj$" (second rs)))
    (is (re-find #"test/resauce/core_test\.clj$" (nth rs 2)))
    (is (re-find #"test/resauce/protocols_test\.clj$" (nth rs 3)))
    (is (re-find #"test/resauce/test_utils\.clj$" (nth rs 4)))))
