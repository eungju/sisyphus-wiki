(ns sisyphus-wiki.vfile
  (:use clojure.java.io)
  (:import [org.eclipse.jgit.api Git]
           [org.eclipse.jgit.lib Constants]
           [org.eclipse.jgit.revwalk RevWalk]
           [org.eclipse.jgit.treewalk TreeWalk]
           [org.eclipse.jgit.treewalk.filter TreeFilter PathFilter AndTreeFilter]
           [java.util Date]))

(defprotocol VFileStore
  (close [this])
  (root [this]))

(defprotocol VFile
  (parent [this])
  (basename [this])
  (pathname [this])
  (directory? [this])
  (children [this])
  (child [this name])
  (change-sets [this]))

(defprotocol VChangeSet
  (revision [this])
  (message [this])
  (commit-time [this])
  (committer [this]))

(deftype GitChangeSet [repo object-id message commit-time committer]
  VChangeSet
  (revision [this] (.name object-id))
  (message [this] message)
  (commit-time [this] commit-time)
  (committer [this] {:name (.getName committer)
                     :email (.getEmailAddress committer)}))

(defn- git-change-sets [node repo object-id]
  (if (nil? object-id)
    []
    (let [start-id (.resolve repo Constants/HEAD)
          walk (RevWalk. repo)]
      (try
        (if-not (nil? (parent node))
          (.setTreeFilter walk (AndTreeFilter/create
                                (PathFilter/create (pathname node))
                                TreeFilter/ANY_DIFF)))
        (.markStart walk (.parseCommit walk start-id))
        (doall (map #(GitChangeSet. repo (.getId %) (.getFullMessage %) (Date. (* (.getCommitTime %) 1000)) (.getCommitterIdent %)) (iterator-seq (.iterator walk))))
        (finally (.dispose walk))))))

(deftype GitFile [repo parent object-id name]
  VFile
  (parent [this] parent)
  (basename [this] name)
  (pathname [this] (if (nil? (basename parent))
                     name
                     (str (pathname parent) "/" name)))
  (directory? [this] false)
  (change-sets [this] (git-change-sets this repo object-id)))

(deftype GitDirectory [repo parent object-id name]
  VFile
  (parent [this] parent)
  (basename [this] name)
  (pathname [this] (cond
                    (nil? parent) ""
                    (nil? (basename parent)) name
                    true (str (pathname parent) "/" name)))
  (directory? [this] true)
  (children [this]
    (if (nil? object-id)
      []
      (let [walk (TreeWalk. repo)]
        (try
          (doto walk
            (.addTree object-id)
            (.setRecursive false))
          ((fn [acc]
             (if (.next walk)
               (let [child-id (.getObjectId walk 0)
                     child-name (.getNameString walk)]
                 (recur (conj acc
                              (if (.isSubtree walk)
                                (GitDirectory. repo this child-id child-name)
                                (GitFile. repo this child-id child-name)))))
               acc)) [])
          (finally (.release walk))))))
  (child [this name]
    (first (drop-while #(not= (.name %) name) (children this))))
  (change-sets [this] (git-change-sets this repo object-id)))

(deftype GitStore [repo git]
  VFileStore
  (close [this] (.close repo))
  (root [this]
    (let [head-id (.resolve repo Constants/HEAD)
          walk (RevWalk. repo)]
      (try
        (GitDirectory. repo
                       nil
                       (if head-id
                         (-> walk (.parseCommit head-id) (.getTree)))
                       nil)
        (finally (.dispose walk))))))

(defn git-store [dir &{:keys [init] :or {init false}}]
  (let [git (if (and init
                     (or (and (.isDirectory dir) (.listFiles dir))
                         (not (.exists dir))))
              (-> (Git/init) (.setBare false) (.setDirectory dir) (.call))
              (Git/open dir))]
    (GitStore. (.getRepository git) git)))
