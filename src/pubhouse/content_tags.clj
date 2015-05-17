(ns pubhouse.content-tags
  (:require [clojure.string :refer [join]]))

(defn split-string
  "Splits a string into two parts around 'key'. Returns nil if 
   's' contains no occurances of 'key'."
  [s key]
  (let [parts (clojure.string/split s key)]
    (if (= (join parts) s)
      nil
      (let [ks (re-find key s)]
        [(first parts) (clojure.string/join ks (rest parts))]))))

(def start-tag #"\{\{\{")

(def end-tag #"\}\}\}")

(defn contains-end-tag? [line] (re-find end-tag line))

(defn contains-start-tag? [line] (re-find start-tag line))

(defn contains-tag?
  [line]
  (or (contains-start-tag? line)
      (contains-end-tag? line)))

(defn eval-lines [lines] (->> lines (join "\n") (read-string) (eval)))

(defn eval-tagged-lines
  [lines]
  (let [step (fn [lines]
               (when (seq lines)
                 (if-let [[before expr] (split-string (first lines) start-tag)]
                   (let [[exp more-lines] (split-with (complement contains-tag?)
                                                      (cons expr (rest lines)))]
                     (if (seq more-lines)
                       (if (contains-start-tag? (first more-lines))
                         (throw
                          (Exception. (str "Unexpected " start-tag ", expected " end-tag)))
                         (let [[last-exp after] (split-string (first more-lines) end-tag)
                               value (eval-lines (concat exp (list last-exp)))]
                           (cons (str before value after)
                                 (eval-tagged-lines (rest more-lines)))))
                       (throw
                        (Exception. (str "Unexpected EOF. Expected" end-tag)))))
                   (cons (first lines) (eval-tagged-lines (rest lines))))))]
    (lazy-seq (step lines))))
