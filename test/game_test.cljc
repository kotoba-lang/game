(ns game-test
  (:require [clojure.test :refer [deftest is testing]]
            [game]))
(deftest namespace-loads
  (testing "the restored CLJC namespace loads"
    (is (some? game))))
