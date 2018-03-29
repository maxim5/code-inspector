(ns active-paper-clojure-runtime.data
  (:require [clj-hdf5.core :as hdf5])
  (:import active_papers.ActivePaperRef)
  (:import active_paper_runtime.HDF5Node)
  (:import active_paper_runtime.DataAccess)
  (:import java.io.File))

;
; Access to an existing dataset, with reference handling.
;
(defn get-data
  [name]
  (let [node (DataAccess/getData name)]
    (hdf5/make-hdf-node (.getReader node) (.getPath node))))

(defn read-data
  [name]
  (let [node (DataAccess/getData name)]
    (hdf5/read (hdf5/make-hdf-node (.getReader node) (.getPath node)))))

;
; Creation of a new dataset, with all the required attributes.
;
(defn create-data
  ([name data]
     (let [node (DataAccess/createData name)
           clj-node (hdf5/make-hdf-node (.getWriter node) (.getPath node))
           group (hdf5/parent clj-node)
           ds (hdf5/create-dataset group name data)]
       (DataAccess/finalizeData node)
       ds))
  ([name data data-model-name]
     (let [ds (create-data name data)]
       (hdf5/create-attribute ds
                              "active-paper-domain-data-model" data-model-name))))

