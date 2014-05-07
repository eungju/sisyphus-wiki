(ns sisyphus-wiki.vfile-test
  (:use midje.sweet
        clojure.java.io)
  (:require [sisyphus-wiki.vfile :refer :all])
  (:import [java.io File]
           [java.util Date]
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

(def default-committer {:name "John Doe" :email "john@doe.com"})

(defn- git-commit [git pattern message]
  (-> git (.add) (.addFilepattern pattern) (.call))
  (-> git (.commit)
      (.setMessage message)
      (.setCommitter (:name default-committer) (:email default-committer))
      (.call)))

(defn- git-create-file [git path body message]
  (let [target-file (file (-> git (.getRepository) (.getWorkTree)) path)]
    (.mkdirs (.getParentFile target-file))
    (create-file target-file body)
    (git-commit git path message)))

(defn- git-modify-file [git path body message]
  (let [target-file (file (-> git (.getRepository) (.getWorkTree)) path)]
    (modify-file target-file body)
    (git-commit git path message)))

(against-background
  [(around
    :facts (let [dir (doto (File/createTempFile "test-" "-git") (.delete) (.mkdir))
                 dut (git-store dir :init true)
                 git (.git dut)] ?form (close dut)))]

  (fact "The root is always exist."
    (let [root-dir (root dut)]
      (directory? root-dir) => true
      (parent root-dir) => nil
      (basename root-dir) => nil
      (pathname root-dir) => ""
      (children root-dir) => []
      (child root-dir "not-exist") => nil
      (empty? (change-sets (root dut))) => true))

  (fact "A directory can contain regular files."
    (dorun (map #(.createNewFile (file dir %)) ["file1", "file2"]))
    (git-commit git "." "first commit.")
    (let [root-dir (root dut)]
      (map basename (children root-dir)) => ["file1", "file2"]
      (map pathname (children root-dir)) => ["file1", "file2"]
      (every? #(not (directory? %)) (children root-dir)) => true
      (map parent (children root-dir)) => [root-dir root-dir]))

  (fact "A directory can contain directories."
    (.mkdir (file dir "dir1"))
    (.createNewFile (file dir "dir1/file1"))
    (git-commit git "dir1/file1" "first commit.")
    (let [root-dir (root dut)
          dir1 (child root-dir "dir1")]
      (every? directory? (children root-dir)) => true
      (map basename (children dir1)) => ["file1"]
      (map pathname (children dir1)) => ["dir1/file1"]
      (map parent (children dir1)) => [dir1]))

  (fact "A directory contains all committed children."
    (doto git
      (git-create-file "file1" "" "first commit.")
      (git-create-file "file2" "" "second commit."))
    (map basename (children (root dut))) => ["file1", "file2"])

  (fact "Change sets of a directory contains all changes of the directory and the children."
    (doto git
      (git-create-file "file1" "body1" "first.")
      (git-create-file "file2" "body2" "second."))
    (map message (change-sets (root dut))) => ["second.", "first."])

  (fact "Change sets of a regular file contains all changes of the file."
    (doto git
      (git-create-file "file1" "body1" "first.")
      (git-create-file "file2" "body2" "second.")
      (git-modify-file "file1" "body changed." "third."))
    (map message (change-sets (child (root dut) "file1"))) => ["third.", "first."])

  (fact "A change set has a message, a commit time and a committer."
    (git-create-file git "file1" "body1" "first.")
    (let [result (first (change-sets (root dut)))]
      (.getTime (commit-time result)) => (roughly (.getTime (Date.)) 1000)
      (committer result) => default-committer))
)
