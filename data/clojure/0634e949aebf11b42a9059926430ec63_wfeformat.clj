(ns wfeditor.io.file.wfeformat
  (:require [wfeditor.io.util.xml :as xml-util]
            [wfeditor.model.workflow :as wf]
            [clojure.string :as string]
            [clojure.contrib.zip-filter.xml :as zfx]
            [clojure.zip :as zip])
  (:import [wfeditor.model.workflow Job Workflow WFInstance]
           [java.io ByteArrayInputStream]))


(def file-filter-extensions ["*.xml"])
(def file-filter-names ["Extensible Markup Language (XML) Files"])

(def format-reqd-states {:workflow :reqd, :wfinstance :reqd
                         :username :req-when-parent, :exec-domain :req-when-parent, :wf-name :req-when-parent, :wf-ver :req-when-parent, :wf-format-ver :req-when-parent, :name :req-when-parent, :prog-exec-loc :req-when-parent, :prog-args :req-when-parent, :prog-opts :req-when-parent, :flag :req-when-parent, :val :req-when-parent, :dep :req-when-parent, :task-id :req-when-parent, :status :req-when-parent, :start :req-when-parent, :end :req-when-parent
                         :meta nil, :parent nil, :parent-ver nil, :parent-file nil, :parent-hash nil, :jobs nil, :job nil, :id nil :desc nil, :prog-name nil, :prog-ver nil, :prog-exec-ver nil, :arg nil, :opt nil, :std-out-file nil, :std-err-file nil, :deps nil, :task-statuses nil, :task nil, :array nil, :step nil, :index-var nil, :scheduler nil, :sched-opts nil})

(def format-hierarchy {:wfinstance [:username :exec-domain :workflow], :workflow [:meta :scheduler :jobs], :meta [:wf-name :wf-ver :wf-format-ver :parent], :parent [:parent-ver :parent-file :parent-hash], :scheduler [:exec-domain :sched-opts], :sched-opts :opt, :jobs :job, :job [:id :name :desc :prog-name :prog-ver :prog-exec-loc :prog-exec-ver :prog-args :prog-opts :std-out-file :std-err-file :job-deps :task-statuses :array :sched-opts], :prog-args :arg, :prog-opts :opt, :opt [:flag :val], :deps :dep, :task-statuses :task, :task [:task-id :status] :array [:start :end :step :index-var]})

;;
;; functions to create XML from datatypes
;;


(defn- remove-fn
  "use this function to prune null/empty non-essential info being represented in a XML tree reprsentation"
  [obj]
  (or (nil? obj) (and (coll? obj) (empty? obj)) (and (string? obj) (empty? obj))))


;; TODO: create test case that asserts that all of the fields that are
;; children of the :job type in format-hierarchy are present in the
;; Job defrecord
;; TODO: create test case that asserts that the  reqd/optional
;; statuses in format-reqd-states match with new-job-fn and/or
;; pre-post condition checks

(defn xml-subtree
  "helper method for creating XML trees to represent values stored in WFE types.
assumes that no attributes are present in any of the tags. (this is acceptable for WFE since attributes are eschewed and substituted by representing them as child elements.)"
  [tag val & [{:keys [prune-empty] :or {prune-empty true}}]]
  (cond
   (remove-fn val) (if (and prune-empty (not (format-reqd-states tag)))
                     nil
                     {:tag tag :attrs nil :content []})
   (sequential? val) {:tag tag :attrs nil :content (into [] (remove nil? (for [x val] (xml-subtree (format-hierarchy tag) x {:prune-empty prune-empty}))))}
   (map? val) (let [keyval-tag (format-hierarchy tag)
                    [key-tag val-tag] (format-hierarchy keyval-tag)]
                {:tag tag :attrs nil :content
                 (into [] (remove nil? (for [[k v] val]
                                         {:tag keyval-tag :attrs nil :content
                                          [(xml-subtree key-tag k {:prune-empty prune-empty})
                                           (xml-subtree val-tag v {:prune-empty prune-empty})]})))})
   (keyword? val) {:tag tag :attrs nil :content [(name val)]}
   ;; we need all other scalars to be cast to Strings for the purposes
   ;; of the Clojure XML emit function
   true {:tag tag :attrs nil :content [(str val)]}))

(defn- map-coll-vals-xml-subtree
  "helper method for maps which map keys to collections. this is for maps like the program options, where one option may occur multiple times with different values"
  [tag map & [{:keys [prune-empty nil-on-empty-map] :or {prune-empty true nil-on-empty-map false}}]]
  (let [keyval-tag (format-hierarchy tag)
        [key-tag val-tag] (format-hierarchy keyval-tag)]
    (when-not (and nil-on-empty-map (empty? map))
      {:tag tag :attrs nil :content
       (into [] (remove nil? (flatten (for [[k coll] map]
                                        (if (or (nil? coll) (empty? coll))
                                          {:tag keyval-tag :attrs nil :content
                                           [(xml-subtree key-tag k {:prune-empty prune-empty})
                                            (xml-subtree val-tag coll {:prune-empty prune-empty})]}
                                          (for [v coll]
                                            {:tag keyval-tag :attrs nil :content
                                             [(xml-subtree key-tag k {:prune-empty prune-empty})
                                              (xml-subtree val-tag v {:prune-empty prune-empty})]}))))))})))

(defn- job-array-xml-tree
  "return the xml subtree for the array map (that is, a map under the field named 'array' in the Job object that represents a job array)"
  [array-map & [{:keys [prune-empty] :or {prune-empty true}}]]
  (when (or (not prune-empty) (seq (remove remove-fn (vals array-map))))
    (let [array-keys (:array format-hierarchy)]
      {:tag :array :attrs nil :content
       (into [] (remove nil? (for [k array-keys]
                               (let [val (get array-map k)]
                                 (xml-subtree k val {:prune-empty prune-empty})))))}
      )))

(defn- job-xml-tree
  "implementation of defmethod for xml-tree multimethod for the Job record class"
  ([job]
     (job-xml-tree nil job {:prune-empty false}))
  ([wf job & [{:keys [prune-empty] :or {prune-empty true}}]]
     (let [job-keys (keys job)]
       {:tag :job :attrs nil :content
        (into [] (remove nil? (concat (for [key job-keys]
                                        (let [val (get job key)]
                                          (condp = key
                                            :prog-opts (map-coll-vals-xml-subtree key val {:prune-empty prune-empty})
                                            :array (job-array-xml-tree val {:prune-empty prune-empty})
                                            :sched-opts (map-coll-vals-xml-subtree key val {:prune-empty prune-empty :nil-on-empty-map true})
                                            (xml-subtree key val {:prune-empty prune-empty}))))
                                      (remove nil? [ (when wf
                                                       (let [job-deps (map #(get % :name) ((:neighbors (wf/dep-graph wf)) job))]
                                                         (xml-subtree :deps job-deps {:prune-empty prune-empty})))]))))})))

(defn- wf-meta-xml-tree
  "helper method for wf-xml-tree for meta element"
  [wf]
  (let [wf-get-fn (fn [key] (get wf key))
        meta-wf-tags [:wf-name :wf-ver :wf-format-ver]
        meta-wf-vals (map wf-get-fn meta-wf-tags)
        meta-parent-tags [:parent-ver :parent-file :parent-hash]
        meta-parent-vals (map wf-get-fn meta-parent-tags)
        meta-tags (concat meta-wf-tags meta-parent-tags)
        meta-vals (concat meta-wf-vals meta-parent-vals)
        or-fn (fn ([a] a) ([a b] (or a b)))]
    (when (reduce or-fn meta-vals)
      (let [new-tag-fn (fn [tag] (xml-subtree tag (wf-get-fn tag)))]
        {:tag :meta :attrs nil :content (remove nil?  [(new-tag-fn :wf-name)
                                                       (new-tag-fn :wf-ver)
                                                       (new-tag-fn :wf-format-ver)
                                                       (when (reduce or-fn meta-parent-vals) {:tag :parent :attrs nil :content (remove nil? [(new-tag-fn :parent-ver) (new-tag-fn :parent-file) (new-tag-fn :parent-hash)])})])}))))

(defn- wf-scheduler-xml-tree
  "helper method for wf-xml-tree for scheduler element"
  [wf]
  (let [wf-get-fn (fn [key] (get wf key))
        meta-scheduler-tags [:exec-domain :sched-opts]
        meta-scheduler-vals [(wf-get-fn :exec-domain) (when (seq (wf-get-fn :sched-opts)) (wf-get-fn :sched-opts))]
        or-fn (fn ([a] a) ([a b] (or a b)))]
    (when (reduce or-fn meta-scheduler-vals)
      (let [new-tag-fn (fn [tag] (xml-subtree tag (wf-get-fn tag)))]
        {:tag :scheduler :attrs nil :content (remove nil? [(new-tag-fn :exec-domain)
                                                           (when (seq (wf-get-fn :sched-opts)) (map-coll-vals-xml-subtree :sched-opts (wf-get-fn :sched-opts) {:prune-empty true}))])}))))

(defn- wf-xml-tree
  "implementation of defmethod for xml-tree multimethod for the Workflow record class"
  [wf]
  (let [meta-subtree (wf-meta-xml-tree wf)
        scheduler-subtree (wf-scheduler-xml-tree wf)
        job-seq (wf/wf-job-seq wf)
        jobs-subtree {:tag :jobs :attrs nil :content (remove nil? (into [] (map (partial job-xml-tree wf) job-seq)))}]
    {:tag :workflow :attrs nil :content (remove nil? [meta-subtree scheduler-subtree jobs-subtree])}))

(defn- wfinstance-xml-tree
  "implementation of defmethod for xml-tree multimethod for the WFInstance record class"
  [wfinst]
  (let [username (:username wfinst) 
        exec-domain (:exec-domain wfinst)
        wf (:workflow wfinst)]
    {:tag :wfinstance :attrs nil :content [(xml-subtree :username username) (xml-subtree :exec-domain exec-domain) (wf-xml-tree wf)]}))

;; return an XML tree for a given object as XML trees are returned by clojure.xml/parse
(defmulti xml-tree class)
;; (defmethod xml-tree wfeditor.model.workflow.Job [obj] (job-xml-tree obj))
(defmethod xml-tree wfeditor.model.workflow.Workflow [obj] (wf-xml-tree obj))
(defmethod xml-tree wfeditor.model.workflow.WFInstance [obj] (wfinstance-xml-tree obj))

(defn workflow-to-string
  "return an XML string representation of the current workflow object"
  [wf]
  (xml-util/tree-to-ppxml-str (xml-tree wf)))

(defn workflow-instance-to-string
  "return an XML string representation of the current workflow instance object"
  [wfinst]
  (xml-util/tree-to-ppxml-str (xml-tree wfinst)))

;;
;; functions to create datatypes from XML
;;

(defn nil-pun-empty-str
  "if the input is an empty string, return nil.  else, return the input val"
  [s]
  (if (and (string? s) (empty? s))
    nil
    s))

(defn map-keyval-seq-from-zip
  "return a Clojure sequence of keyval pairs from a zipper of an XML tree that represents a map, where the zipper is set to a loc of the XML element representing the map, the children elements represent each key-val pair, and each key-val pair's element has 2 children elements for the key and value"
  ([z tag]
     (map-keyval-seq-from-zip z tag identity))
  ([z tag val-func]
     (when z
       (let [keyval-tag (format-hierarchy tag)
             keyval-zip-seq (zfx/xml-> z keyval-tag)
             [key-tag val-tag] (format-hierarchy keyval-tag)]
         (for [keyval keyval-zip-seq]
           (let [key (first (zfx/xml-> keyval key-tag zfx/text))
                 val (nil-pun-empty-str (first (zfx/xml-> keyval val-tag zfx/text)))]
             {key (val-func val)}))))))

(def merge-with-fn
  ;; the merge fn for the merge-with fn to be used for maps that may or may not have multiple vals. this fn will be applied to the respective vals of the 2 maps being merged

  ;; the default implementation of a map-from-zip is to assume that
  ;; each key maps to a scalar (i.e., single val), and if there are
  ;; repeat keys, then only take the last val 
  (comp last list))

(defn map-keyval-seq-to-map
  "turn a keyval seq for constructing a map into the final map"
  ([keyval-seq]
     (map-keyval-seq-to-map keyval-seq merge-with-fn))
  ([keyval-seq merge-fn & [{:keys [prune-empty] :or {prune-empty true}}]]
     (let [remove-fn (if prune-empty (fn [[k v]] (not (or (nil-pun-empty-str k) (nil-pun-empty-str v)))) (fn [_] false))]
       ;; make sure that maps are all sorted, as described here http://tech.puredanger.com/2010/02/09/clojure-3-sorted-maps/
       ;; keep the merge-with command for the time when multiple vals
       ;; per opt is supported
       (into (sorted-map) (remove remove-fn (apply merge-with merge-fn keyval-seq))))))

(defn- map-from-zip
  "return a map of string keys -> string values created from an XML zip (z) using a tag (tag), representing the map, that has children which contain pairs of leaf tags representing the key-value pairs of the map"
  ([z tag]
     ;; the default implementation of a map-from-zip is to assume that
     ;; each key maps to a scalar (i.e., single val), and if there are
     ;; repeat keys, then only take the last val 
     (map-from-zip z tag merge-with-fn identity))
  ([z tag merge-fn val-func & [{:keys [prune-empty] :or {prune-empty true}}]]
     (when z
       (map-keyval-seq-to-map (map-keyval-seq-from-zip z tag val-func) merge-fn {:prune-empty prune-empty}))))

(def map-of-coll-vals-list-fn
  ;; this is the fn applied to the vals of the map that is culled by
  ;; map-from-zip for the purpose of maps like prog-opts where the
  ;; vals are sequential collections
  (fn [x] (if (nil? x) nil (list x))))

(defn map-of-coll-vals-from-zip
  "similar to map-from-zip, but the vals of the map are collections.  this made for data strucutres like program options, where one option flag might occur with multiple values."
  [z tag]
  (let [list-fn map-of-coll-vals-list-fn]
    (map-from-zip z tag concat list-fn)))

(defn vector-from-zip
  "return a vector of string values created from an XML zip (z) using a tag (tag), representing the vector, that has 0+ children of leaf tags, representing the values"
  [z tag & [{:keys [prune-empty] :or {prune-empty true}}]]
  (when z
    (let [remove-fn (if prune-empty remove-fn (fn [_] false))]
      (into [] (remove remove-fn (map nil-pun-empty-str (zfx/xml-> z (format-hierarchy tag) zfx/text)))))))

(defn- scalar-from-zip
  "return a scalar, of type string, created from an XML zip (z) within a child tag (tag)"
  [z tag]
  (first (zfx/xml-> z tag zfx/text)))

(defn- deps-from-zip
  "returns the dependencies of job given a job zipper as a vector of job names"
  [z]
  (vector-from-zip (zfx/xml1-> z :deps) :deps))

(defn- map-to-flat-vector
  "return a vector where each key in the map is followed by its value. if function fn is provided, then it will be applied to every vector of key-val pairs"
  ([map]
     (map-to-flat-vector map identity))
  ([map fn]
      (flatten (into [] (into {} (for [[k v] map] (fn [k v])))))))

(defn- job-array-from-zip
  "return the map in the field named 'array' in the Job object, given a zipper at the job node"
  [z]
  (when-let [array-prop-nodes (zfx/xml-> z :array zip/children)]
    (into {}
          (for [prop-node array-prop-nodes]
            (let [key (:tag prop-node)
                  prop-zip (xml-util/xml-tree-to-zip prop-node)
                  val (zfx/text prop-zip)
                  parsed-val (if (#{:start :end :step} key) (Integer/parseInt val) val)]
              [key parsed-val])))))

(defn task-statuses-from-zip
  "return the task statuses map in the field named 'task-statuses' in the Job object, given a zipper at the job node"
  ([z]
     ;; TODO: generalize parsing of
     ;; task-statuses (and even the
     ;; scalar-from-zip that other
     ;; fn's call) by storing type of
     ;; each element of format hierarchy
     (task-statuses-from-zip z :task-statuses))
  ([z field]
     (when-let [task-statuses-map (map-from-zip z field)]
       (into {} (map
                 (fn [[k v]]
                   (let [k (if (string? k) (Integer/parseInt k) k)
                         v (if (string? v) (keyword v) v)]
                     [k v]))
                 task-statuses-map)))))

(defn job-from-zip
  "return a new Job instance when given a XML zipper that is currently at a job node"
  [z]
  (let [fields (format-hierarchy :job)
        field-map (apply merge
                         (letfn [(field-val [field]
                                   (condp = field
                                     :id (when-let [id-str (scalar-from-zip z field)] (Integer/parseInt id-str))
                                     :prog-args (vector-from-zip (zfx/xml1-> z field) field)
                                     :prog-opts (map-of-coll-vals-from-zip (zfx/xml1-> z field) field)
                                     ;; TODO: generalize parsing of
                                     ;; task-statuses (and even the
                                     ;; scalar-from-zip that other
                                     ;; fn's call) by storing type of
                                     ;; each element of format hierarchy
                                     :task-statuses (task-statuses-from-zip (zfx/xml1-> z field) field)
                                     :array (job-array-from-zip z)
                                     :sched-opts (map-of-coll-vals-from-zip (zfx/xml1-> z field) field)
                                     (scalar-from-zip z field)))]
                           (for [f fields :when (not (#{:deps} f))]
                             {f (field-val f)})))
        ;; keep reqd-fields as vector for the apply func call
        reqd-fields [:name :prog-exec-loc :prog-args :prog-opts]
        reqd-vals (map #(field-map %) reqd-fields)
        ;; keep optional-fields as vector like reqd-fields just in case
        optional-fields (remove (set reqd-fields) fields)
        optional-field-vals (map-to-flat-vector field-map (fn [[k v]] (when ((set optional-fields) k) [k v])))]
    ;; TODO: make sure that required and optional fields match up with
    ;; format-reqd-states map above and
    ;; wfeditor.model.workflow/new-job-fn as well    
    (apply wfeditor.model.workflow/new-job-fn (concat reqd-vals optional-field-vals))))

(defn- meta-from-zip
  "return the meta info of the workflow given an XML zipper of the workflow as a map of tags to values"
  [z]
  (when-let [meta-zip (first (zfx/xml-> z :meta))]
    (let [meta-map (into {} (for [tag [:wf-name :wf-ver :wf-format-ver]] [tag (scalar-from-zip meta-zip tag)]))
          meta-map (if-let [parent-zip (first (zfx/xml-> meta-zip :parent))]
                     (let [parent-meta-map (into {} (for [tag [:parent-ver :parent-file :parent-hash]] [tag (scalar-from-zip parent-zip tag)]))]
                       (merge meta-map parent-meta-map))
                     meta-map)]
      meta-map)))

(defn- scheduler-from-zip
  "return the scheduler info configured at the workflow-level, and return as a map (to be merged into the WF), given an XML zipper of the workflow"
  [z]
  (when-let [scheduler-zip (zfx/xml1-> z :scheduler)]
    (let [exec-domain (scalar-from-zip scheduler-zip :exec-domain)
          sched-opts (when-let [sched-opts-zip (zfx/xml1-> scheduler-zip :sched-opts)]
                       (map-of-coll-vals-from-zip sched-opts-zip :sched-opts))
          sched-map {:exec-domain exec-domain
                     :sched-opts sched-opts}]
      sched-map)))

(defn- workflow-from-zip
  "return a workflow given an XML zipper of the workflow"
  [z]
  (let [meta-map (meta-from-zip z)
        scheduler-map (scheduler-from-zip z)]
    (loop [job-zip-seq (zfx/xml-> z :jobs :job)
           job-set #{}
           dep-name-map {}]      
      (if (empty? job-zip-seq)
        (let [graph (wf/new-graph-fn :nodes job-set :neighbors (wf/job-dep-map job-set dep-name-map))
              wf-meta-fields (remove #(= :parent %) (concat (format-hierarchy :meta) (format-hierarchy :parent)))
              wf-meta-map (into {} (map (fn [k] [k (get meta-map k)]) wf-meta-fields))
              meta-field-vals (map-to-flat-vector wf-meta-map)
              wf-scheduler-fields (format-hierarchy :scheduler)
              wf-scheduler-map (into {} (map (fn [k] [k (get scheduler-map k)]) wf-scheduler-fields))
              scheduler-field-vals (map-to-flat-vector wf-scheduler-map)
              wf (apply wfeditor.model.workflow/new-workflow-fn (concat [:graph graph] meta-field-vals scheduler-field-vals))]
          wf)
        (let [jz (first job-zip-seq)
              job (job-from-zip jz)
              job-name (:name job)
              job-deps (deps-from-zip jz)]
          (recur (rest job-zip-seq) (conj job-set job) (assoc dep-name-map job-name job-deps)))))))

(defn- workflow-from-file
  "return a workflow based on an input XML string representation"
  [file-name]
  (let [wf-xml-tree (xml-util/xml-file-to-tree file-name)
        wf-xml-zip (xml-util/xml-tree-to-zip wf-xml-tree)]
    (workflow-from-zip wf-xml-zip)))

(def workflow-from-stream workflow-from-file)

(defn- wfinstance-from-zip
  "return a workflow instance given an XML zipper of the workflow"
  [z]
  (let [username (scalar-from-zip z :username)
        exec-domain (scalar-from-zip z :exec-domain)
        wf-zip (first (zfx/xml-> z :workflow))
        wf (workflow-from-zip wf-zip)]
    (wfeditor.model.workflow/new-wfinstance-fn username exec-domain wf)))

(defn- wfinstance-from-file
  "return a workflow instance based on an input XML string representation"
  [file-name]
  (let [wfinstance-xml-tree (xml-util/xml-file-to-tree file-name)
        wfinstance-xml-zip (xml-util/xml-tree-to-zip wfinstance-xml-tree)]
    (wfinstance-from-zip wfinstance-xml-zip)))

;; TODO: clean up the __-from-__ style of methods that parse text and
;; return objects so that zipper creation is abstracted 

(defn wfinstance-from-stream
  "return a workflow instance from the input stream"
  [in-stream]
  (let [wfinstance-xml-tree (xml-util/xml-stream-to-tree in-stream)
        wfinstance-xml-zip (xml-util/xml-tree-to-zip wfinstance-xml-tree)]
    (wfinstance-from-zip wfinstance-xml-zip)))

(defn string-input-stream
  "return an input stream from a string"
  [str]
  (ByteArrayInputStream. (.getBytes str)))

(defn wfinstance-from-string
  "return a workflow instance from a string"
  [str]
  (let [str-stream (string-input-stream str)]
    (wfinstance-from-stream str-stream)))

;;
;; developer-friendly high-level functions for loading and saving
;; datatypes from/to XML
;;

(defn set-workflow-from-file
  "set the current state of the workflow based on an input XML string representation"
  [file-name]
  (let [wf (workflow-from-file file-name)]
    (wf/set-workflow wf)))

(defn save-workflow-to-file
  "save (using spit) the contents of the workflow object (wf) to the file provided (file-name) or create it if it doesn't exist"
  [wf file-name]
  (spit file-name (workflow-to-string wf)))

;;
;; other developer-friendly high-level functions
;;

(defn zip-from-job
  "create a zipper given only a Job object"
  [job]
  (let [xml-tree (job-xml-tree job)
        zip (xml-util/xml-tree-to-zip xml-tree)]
    zip))