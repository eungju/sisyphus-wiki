(ns sisyphus-wiki.vfile
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

(deftype GitChangeset [repo oid message commit-time committer]
  VChangeset
  (revision [this] (.name oid))
  (message [this] message)
  (commit-time [this] commit-time)
  (committer [this] {:name (.getName committer)
                     :email (.getEmailAddress committer)}))

(deftype GitFile [repo oid name parent]
  VFile
  (parent [this] parent)
  (basename [this] name)
  (pathname [this] (if (nil? (basename parent))
                     name
                     (str (pathname parent) "/" name)))
  (directory? [this] false))

(deftype GitDirectory [repo oid name parent]
  VFile
  (parent [this] parent)
  (basename [this] name)
  (pathname [this] (cond
                    (nil? parent) ""
                    (nil? (basename parent)) name
                    true (str (pathname parent) "/" name)))
  (directory? [this] true)
  (children [this]
    (if (nil? oid)
      []
      (let [walk (TreeWalk. repo)]
        (try
          (doto walk
            (.addTree oid)
            (.setRecursive false))
          ((fn [acc]
             (if (.next walk)
               (let [child-id (.getObjectId walk 0)
                     child-name (.getNameString walk)]
                 (recur (conj acc
                              (if (.isSubtree walk)
                                (GitDirectory. repo child-id child-name this)
                                (GitFile. repo child-id child-name this)))))
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
                       (if head-id
                         (-> walk (.parseCommit head-id) (.getTree)))
                       nil
                       nil)
        (finally (.dispose walk))))))

(defmulti changesets (fn [node & _] (class node)))

(derive GitFile ::git-file)
(derive GitDirectory ::git-file)

(defn- git-commit-to-vchangeset [repo c]
  (GitChangeset. repo
                 (.getId c)
                 (.getFullMessage c)
                 (Date. (* (.getCommitTime c) 1000))
                 (.getCommitterIdent c)))

(defmethod changesets ::git-file [node &{:keys [limit start] :or {limit nil start Constants/HEAD}}]
  (let [repo (.repo node) oid (.oid node)]
    (if (nil? oid)
      []
      (let [walk (RevWalk. repo)]
        (try
          (if-not (nil? (parent node))
            (.setTreeFilter walk (AndTreeFilter/create
                                  (PathFilter/create (pathname node))
                                  TreeFilter/ANY_DIFF)))
          (.markStart walk (.parseCommit walk (.resolve repo start)))
          (let [i (iterator-seq (.iterator walk))]
            (doall (map #(git-commit-to-vchangeset repo %)
                        (if (nil? limit) i (take limit i)))))
          (finally (.dispose walk)))))))

(defn git-store [dir &{:keys [init] :or {init false}}]
  (let [git (if (and init
                     (or (and (.isDirectory dir) (.listFiles dir))
                         (not (.exists dir))))
              (-> (Git/init) (.setBare false) (.setDirectory dir) (.call))
              (Git/open dir))]
    (GitStore. (.getRepository git) git)))
