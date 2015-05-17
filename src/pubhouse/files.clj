(ns pubhouse.files
  (:require [me.raynes.fs :as fs]))

(defn rel-path
  [path]
  (->> (fs/split path)
       (drop (count (fs/split fs/*cwd*)))
       (clojure.string/join "/")))

(defn with-parent-dir
  "Ensure that the parent directory struture of the path
  exists before invoking the function f. Most likely, f
  will write to the file at path."
  [path f]
  (let [parent (fs/parent path)]
    (if (fs/exists? parent)
      (f)
      (do (fs/mkdirs parent) (f)))))

(defn map-directory!
  "Filter (file-seq path) by the function pred and apply the function f with
  the file as the first argument, and a line-seq of the file's contents as its
  second argument."
  [f pred path]
  (doall
   (let [root (fs/file path)]
     (for [file (->> root (file-seq) (filter pred))]
       (with-open [reader (clojure.java.io/reader file)]
         (f file (line-seq reader)))))))

(defn do-directory!
  "Similar to map-directory, except the files from the sequence are
  consumed via a doseq iteration."
  [f pred path]
  (let [root (fs/file path)]
    (doseq [file (->> root (file-seq) (filter pred))]
      (with-open [reader (clojure.java.io/reader file)]
        (f file (line-seq reader))))))
