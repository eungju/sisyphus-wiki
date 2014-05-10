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
  (child [this name]))

(defprotocol VChangeset
  (revision [this])
  (message [this])
  (commit-time [this])
  (committer [this]))

(deftype GitChangeset [repo object-id message commit-time committer]
  VChangeset
  (revision [this] (.name object-id))
  (message [this] message)
  (commit-time [this] commit-time)
  (committer [this] {:name (.getName committer)
                     :email (.getEmailAddress committer)}))

(deftype GitFile [repo parent object-id name]
  VFile
  (parent [this] parent)
  (basename [this] name)
  (pathname [this] (if (nil? (basename parent))
                     name
                     (str (pathname parent) "/" name)))
  (directory? [this] false))

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
    (first (drop-while #(not= (.name %) name) (children this)))))

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

(defmulti changesets (fn [node & _] (class node)))

(derive GitFile ::git-file)
(derive GitDirectory ::git-file)

(defmethod changesets ::git-file [node &{:keys [limit] :or {limit nil}}]
  (let [repo (.repo node)
        object-id (.object-id node)]
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
          (let [i (iterator-seq (.iterator walk))]
            (doall (map #(GitChangeset. repo (.getId %) (.getFullMessage %) (Date. (* (.getCommitTime %) 1000)) (.getCommitterIdent %))
                        (if (nil? limit) i (take limit i)))))
          (finally (.dispose walk)))))))

(defn git-store [dir &{:keys [init] :or {init false}}]
  (let [git (if (and init
                     (or (and (.isDirectory dir) (.listFiles dir))
                         (not (.exists dir))))
              (-> (Git/init) (.setBare false) (.setDirectory dir) (.call))
              (Git/open dir))]
    (GitStore. (.getRepository git) git)))
