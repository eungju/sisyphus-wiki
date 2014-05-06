(ns sisyphus-wiki.vfile-test
  (:use midje.sweet
        clojure.java.io)
  (:require [sisyphus-wiki.vfile :refer :all])
  (:import [java.io File]
           [org.eclipse.jgit.errors RepositoryNotFoundException]
           [sisyphus_wiki.vfile GitStore]))

(against-background
  [(around
    :facts (let [temp (doto (File/createTempFile "test-" "-git")
                        (.delete))] ?form))]

  (fact "error if the dir does not exist."
    (git-store temp)
    => (throws RepositoryNotFoundException))

  (fact "error if the dir is not a directory."
    (git-store (doto temp (.createNewFile)))
    => (throws RepositoryNotFoundException))

  (fact "error if the dir is not a git repository."
    (git-store (doto temp (.mkdir)))
    => (throws RepositoryNotFoundException))

  (fact "init if the init parameter is true and the dir is an empty directory."
    (git-store (doto temp (.mkdir)) :init true)
    => (checker [actual] (instance? GitStore actual)))

  (fact "init if the init parameter is true and the dir does not exist."
    (git-store temp :init true)
    => (checker [actual] (instance? GitStore actual)))

  (fact "open if the dir is a git repository."
    (git-store (doto temp (.mkdir)) :init true)
    (git-store temp)
    => (checker [actual] (instance? GitStore actual)))
)

(defn- create-file [f content]
  (with-open [w (writer f)]
    (.write w content)))

(defn- modify-file [f content]
  (with-open [w (writer f)]
    (.write w content)))

(defn- git-commit [git pattern message]
  (-> git (.add) (.addFilepattern pattern) (.call))
  (-> git (.commit) (.setMessage message) (.call)))

(against-background
  [(around
    :facts (let [dir (doto (File/createTempFile "test-" "-git") (.delete) (.mkdir))
                 dut (git-store dir :init true)
                 git (.git dut)] ?form (close dut)))]

  (fact "the root is always exist."
    (let [root-dir (root dut)]
      (directory? root-dir) => true
      (parent root-dir) => nil
      (basename root-dir) => nil
      (pathname root-dir) => ""
      (children root-dir) => []
      (child root-dir "not-exist") => nil))

  (fact "a directory can contain regular files."
    (dorun (map #(.createNewFile (file dir %)) ["file1", "file2"]))
    (git-commit git "." "first commit.")
    (let [root-dir (root dut)]
      (map basename (children root-dir)) => ["file1", "file2"]
      (map pathname (children root-dir)) => ["file1", "file2"]
      (every? #(not (directory? %)) (children root-dir)) => true
      (map parent (children root-dir)) => [root-dir root-dir]))

  (fact "a directory can contain directories."
    (.mkdir (file dir "dir1"))
    (.createNewFile (file dir "dir1/file1"))
    (git-commit git "." "first commit.")
    (let [root-dir (root dut)
          dir1 (child root-dir "dir1")]
      (every? directory? (children root-dir)) => true
      (map basename (children dir1)) => ["file1"]
      (map pathname (children dir1)) => ["dir1/file1"]
      (map parent (children dir1)) => [dir1]))

  (fact "the root contains all committed files."
    (dorun (map #(.createNewFile (file dir %)) ["file1", "file2"]))
    (git-commit git "file1" "first commit.")
    (git-commit git "file2" "second commit.")
    (let [root-dir (root dut)]
      (map basename (children root-dir)) => ["file1", "file2"]))
)

(facts "about change sets."
  (against-background
    [(around
      :facts (let [dir (doto (File/createTempFile "test-" "-git")
                         (.delete)
                         (.mkdir))
                   dut (git-store dir :init true)
                   git (.git dut)] ?form (close dut)))]

    (fact "There can be no change set."
      (empty? (change-sets (root dut))) => true)

    (fact "Change sets of a directory contains all changes of the directory and the children."
      (let [file1 (file dir "file1")
            file2 (file dir "file2")]
        (create-file file1 "body1")
        (git-commit git "file1" "first.")
        (create-file file2 "body2")
        (git-commit git "file2" "second.")
        (map message (change-sets (root dut))) => ["second.", "first."]))

    (fact "Change sets of a regular file contains all changes of the file."
      (let [file1 (file dir "file1")
            file2 (file dir "file2")]
        (create-file file1 "body1")
        (git-commit git "file1" "first.")
        (create-file file2 "body2")
        (git-commit git "file2" "second.")
        (modify-file file1 "body changed.")
        (git-commit git "file1" "third.")
        (map message (change-sets (child (root dut) "file1"))) => ["third.", "first."]
))))
