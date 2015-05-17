(ns pubhouse.files
  (:require [me.raynes.fs :as fs]))

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
  "For all files from (file-seq path) for which pred returns true,
  apply the function f with the file as the first argument, and line-seq
  of its contents as the second argument."
  [f pred path]
  (doall
   (let [root (clojure.java.io/as-file path)]
     (for [file (->> root (file-seq) (filter pred))]
       (with-open [reader (clojure.java.io/reader file)]
         (f file (line-seq reader)))))))
