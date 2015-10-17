(ns euler.core-test
  (:require [clojure.test :refer :all]
            [euler.core :refer :all]))

(deftest test-euler1
  (testing "Euler 1." (is (= euler1 233168))))

(deftest test-euler2
  (testing "Euler 2." (is (= euler2 4613732))))

(deftest test-euler3
  (testing "Euler 3." (is (= euler3 6857))))

(deftest test-euler4
  (testing "Euler 4." (is (= euler4 906609))))

(deftest test-euler5
  (testing "Euler 5." (is (= euler5 232792560))))

(deftest test-euler6
  (testing "Euler 6." (is (= euler6 25164150))))

(deftest test-euler7
  (testing "Euler 7." (is (= euler7 104743))))

(deftest test-euler8
  (testing "Euler 8." (is (= euler8 23514624000))))

