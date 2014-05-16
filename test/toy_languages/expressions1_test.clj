(ns toy-languages.expressions1-test
  (:require [clojure.test :refer :all]
            [toy-languages.expressions1.parser :as exp1]))

(def single-value1 "1")
(def single-value2 "false")
(def single-value3 "\"hello\"")
(def single-value4 "true")
(def simple-sum "4 + 3")
(def simple-sub "4 - 7")
(def simple-and "true and false")
(def simple-or "false or true")
(def simple-eq "true == false")
(def simple-concat "\"a\" ++ \"b\"")

(deftest parse-single-value1-test
  (testing "Parsing a single integer value"
    (def result (exp1/parse single-value1))
    (def expected [:program [:value [:int 1]]])
    (is (= result expected))))

(deftest parse-single-value2-test
  (testing "Parsing a single boolean value"
    (def result (exp1/parse single-value2))
    (def expected [:program [:value [:bool false]]])
    (is (= result expected))))

(deftest parse-single-value3-test
  (testing "Parsing a single string value"
    (def result (exp1/parse single-value3))
    (def expected [:program [:value [:string "hello"]]])
    (is (= result expected))))

(deftest parse-single-value4-test
  (testing "Parsing a single value enclosed by parenthesis"
    (def result (exp1/parse single-value4))
    (def expected [:program [:value [:bool true]]])
    (is (= result expected))))

(deftest parse-simple-sum-test
  (testing "Parsing a sum between two integers"
    (def result (exp1/parse simple-sum))
    (def expected [:program [:binary-exp :add [:value [:int 4]] [:value [:int 3]]]])
    (is (= result expected))))

(deftest parse-simple-sub-test
  (testing "Parsing a subtraction between two integers"
    (def result (exp1/parse simple-sub))
    (def expected [:program [:binary-exp :sub [:value [:int 4]] [:value [:int 7]]]])
    (is (= result expected))))

(deftest parse-simple-and-test
  (testing "Parsing an and between two booleans"
    (def result (exp1/parse simple-and))
    (def expected [:program [:binary-exp :and [:value [:bool true]] [:value [:bool false]]]])
    (is (= result expected))))

(deftest parse-simple-or-test
  (testing "Parsing an or between two booleans"
    (def result (exp1/parse simple-or))
    (def expected [:program [:binary-exp :or [:value [:bool false]] [:value [:bool true]]]])
    (is (= result expected))))

(deftest parse-simple-eq-test
  (testing "Parsing an equals between two booleans"
    (def result (exp1/parse simple-eq))
    (def expected [:program [:binary-exp :equals [:value [:bool true]] [:value [:bool false]]]])
    (is (= result expected))))

(deftest parse-simple-concat-test
  (testing "Parsing an concat between two strings"
    (def result (exp1/parse simple-concat))
    (def expected [:program [:binary-exp :concat [:value [:string "a"]] [:value [:string "b"]]]])
    (is (= result expected))))
