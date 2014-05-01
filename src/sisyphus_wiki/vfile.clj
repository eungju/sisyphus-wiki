(ns sisyphus-wiki.vfile
  (:import [org.eclipse.jgit.api Git]
           [org.eclipse.jgit.lib Constants FileMode]
           [org.eclipse.jgit.revwalk RevWalk]
           [org.eclipse.jgit.treewalk TreeWalk]))

(defprotocol VFileStore
  (close [this])
  (root [this]))

(defprotocol VFile
  (parent [this])
  (basename [this])
  (directory? [this])
  (children [this])
  (child [this name]))

(deftype GitFile [repo parent object-id name]
  VFile
  (parent [this] parent)
  (basename [this] name)
  (directory? [this] false))

(deftype GitDirectory [repo parent object-id name]
  VFile
  (parent [this] parent)
  (basename [this] name)
  (directory? [this] true)
  (children [this]
    (if (nil? object-id)
      []
      (let [walk (doto (TreeWalk. repo)
                   (.addTree object-id)
                   (.setRecursive false))]
        ((fn [acc]
           (if (.next walk)
             (let [child-id (.getObjectId walk 0)
                   child-name (.getNameString walk)]
               (recur (conj acc
                            (if (.isSubtree walk)
                              (GitDirectory. repo this child-id child-name)
                              (GitFile. repo this child-id child-name)))))
             acc)) []))))
  (child [this name]
    (first (drop-while #(not= (.name %) name) (children this)))))

(deftype GitStore [git]
  VFileStore
  (close [this] (.close git))
  (root [this]
    (let [repo (.getRepository git)
          head-id (.resolve repo Constants/HEAD)
          root-id (if head-id (-> (RevWalk. repo)
                                  (.parseCommit head-id)
                                  (.getTree)))]
      (GitDirectory. repo nil root-id nil))))

(defn git-store [dir &{:keys [init] :or {init false}}]
  (GitStore.
   (if (and init
            (or (and (.isDirectory dir) (.listFiles dir))
                (not (.exists dir))))
     (.call (doto (Git/init)
              (.setDirectory (doto dir (.mkdirs)))
              (.setBare false)))
     (Git/open dir))))
