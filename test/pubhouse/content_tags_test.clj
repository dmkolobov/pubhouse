(ns pubhouse.content-tags-test
  (:require [clojure.test :refer :all]
            [me.raynes.fs :as fs]
            [pubhouse.content-tags :refer :all]))

(def sample-code-1
  ["foo{{{(+ 2"
   "2)}}}bar"])

(def sample-result-1
  ["foo4bar"])

(def sample-code-2
  ["foo"
   "hello{{{(clojure.string/upper-case"
   "\", world\")}}}! I say FOO!"
   "bar"])

(def sample-result-2
  ["foo" "hello, WORLD! I say FOO!" "bar"])

(defn test-evaluation
  [code result]
  (is (= (eval-tagged-lines code) result)))

(deftest test-evalutor-sample-1
  (test-evaluation sample-code-1 sample-result-1))

(deftest test-evalutor-sample-2
  (test-evaluation sample-code-2 sample-result-2))
