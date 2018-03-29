(ns sv.data
  (:require (sv [conf :as conf] [core :as core] [sample-data :as sample-data] [util :as util]))
  (:import [sv.core User]) 
  (:require (fleetdb [embedded :as fleet])))

(declare datastore)

(defn to-fleet [m]
  (apply conj (map (fn [[k v]] {(apply str (rest (str k))) (if (keyword? v) (str v) v)}) m)))

(defn from-fleet [m]
  (if (nil? m) nil (apply conj (map (fn [[k v]] {(keyword k) (if (and (string? v) (= \: (first v))) (keyword (apply str (rest v))) v)}) m))))

(defn user-from-fleet [fleet-user]
  (if (nil? fleet-user) nil (merge (util/empty-record sv.core.User) (from-fleet fleet-user))))

(defn persist-user [x]
   (fleet/query datastore ["insert" "users" [ (to-fleet x) ]]))

(defn find-user-by-email [email]
  (user-from-fleet (first (fleet/query datastore ["select", "users", {"where" ["=", "email", email]}]))))



(def datastore
  (if (conf/prod?)
    (fleet/load-persistent conf/data-path)
    (fleet/init-persistent conf/data-path)))

(when-not (conf/prod?)
  (print "Loading sample data")
  (persist-user sample-data/data))
