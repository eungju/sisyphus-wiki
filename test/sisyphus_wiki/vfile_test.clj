(ns sisyphus-wiki.vfile-test
  (:use midje.sweet)
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

(against-background
  [(around
    :facts (let [dir (doto (File/createTempFile "test-" "-git") (.delete) (.mkdir))
                 dut (git-store dir :init true)
                 git (.git dut)] ?form (close dut)))]

  (fact "root directory is always exist."
    (directory? (root dut)) => true
    (basename (root dut)) => nil
    (children (root dut)) => []
    (child (root dut) "not-exist") => nil)

  (fact "a directory can contain files."
    (dorun (map #(.createNewFile (File. dir %)) ["file1", "file2"]))
    (-> git (.add) (.addFilepattern ".") (.call))
    (-> git (.commit) (.setMessage "first commit.") (.call))
    (map basename (children (root dut))) => ["file1", "file2"]
    (every? #(not (directory? %)) (children (root dut))) => true)

  (fact "a directory can contain directories."
    (.mkdir (File. dir "dir1"))
    (.createNewFile (File. dir "dir1/file1"))
    (-> git (.add) (.addFilepattern ".") (.call))
    (-> git (.commit) (.setMessage "first commit.") (.call))
    (every? directory? (children (root dut))) => true
    (map basename (children (child (root dut) "dir1"))) => ["file1"]))
