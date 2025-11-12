(ns resauce.protocols-test
  (:require [babashka.fs :as fs]
            [clojure.test :refer :all]
            [clojure.java.io :as io]
            [resauce.protocols :refer [as-uri]]
            [resauce.test.data :as td]))

(deftest as-uri-test
  (testing "regular files"
    (testing "Path [-> File/String?] -> URI [-> File?] -> Path"
      (let [file-path (td/some-file-path)]
        (is (= file-path (fs/path (as-uri file-path))))
        (is (= file-path (fs/path (as-uri (fs/file file-path)))))
        (is (= file-path (fs/path (as-uri (str file-path)))))
        (is (= file-path (fs/path (fs/file (as-uri file-path)))))
        (is (= file-path (fs/path (fs/file (as-uri (fs/file file-path))))))
        (is (= file-path (fs/path (fs/file (as-uri (str file-path))))))))
    (testing "URL [-> File/String?] -> URI [-> File?] -> URL"
      (let [file-url (td/some-file-url)]
        (is (= file-url (io/as-url (as-uri file-url))))
        (is (= file-url (io/as-url (as-uri (fs/file file-url)))))
        (is (= file-url (io/as-url (as-uri (str file-url)))))
        (is (= file-url (io/as-url (fs/file (as-uri file-url)))))
        (is (= file-url (io/as-url (fs/file (as-uri (fs/file file-url))))))
        (is (= file-url (io/as-url (fs/file (as-uri (str file-url)))))))))

  (testing "resources inside a JAR file"
    (testing "URL [-> String?] -> URI -> URL"
      (let [jar-file-url (first (td/some-jar-file-urls))]
        (is (= jar-file-url (io/as-url (as-uri jar-file-url))))
        (is (= jar-file-url (io/as-url (as-uri (str jar-file-url))))))
      (let [jar-dir-url (first (td/some-jar-dir-urls))]
        (is (= jar-dir-url (io/as-url (as-uri jar-dir-url))))
        (is (= jar-dir-url (io/as-url (as-uri (str jar-dir-url)))))))))
