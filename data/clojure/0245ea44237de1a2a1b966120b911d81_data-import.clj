(ns project-ns
  (:use [clojure.java.io :only [reader]])
  (:use [datomic.api :only [db q] :as d])
  (:require [gotbs.cta.bustracker-data-parser :as parser])
  (:import java.io.File java.text.SimpleDateFormat))

;; clojure
(def uri "datomic:mem://bustracker")
(d/create-database uri)

(def conn (d/connect uri))
;;(def results (q '[:find ?e :where [?e :db/doc]] (db conn)))

(def schema-tx (read-string (slurp "resources/schema/location-schema.dtm")))

@(d/transact conn schema-tx)


(def files (.listFiles (File. "/Users/daltenburg/dev/busdata/data")))

(def filtered (filter #(.startsWith (.getName %) "vehicles") files))

(def slurped (map slurp filtered))

(def parsed (map parser/as-vehicle-data slurped))

;; "20120307 20:42"
(def date-format (SimpleDateFormat. "yyyyMMdd HH:mm"))

(defn to-vehicle-entity [veh]
  {:db/id (d/tempid :db.part/user),
   :bustracker/vehicle-id (:vid veh),
   :bustracker/update-time (.parse date-format (:tmstmp veh)),
   :bustracker/latitude (Double/parseDouble (:lat veh)),
   :bustracker/longitude (Double/parseDouble (:lon veh)),
   :bustracker/heading (Float/parseFloat (:hdg veh)),
   :bustracker/pattern-id (:pid veh),
   :bustracker/travelled-distance  (Float/parseFloat (:pdist veh)),
   :bustracker/route-id (:rt veh),
   :bustracker/destination (:des veh),
   })

(def v (first parsed))

(def txns (map to-vehicle-entity v))

@(d/transact conn txns)

(def results (q '[:find ?v :where [?v :bustracker/vehicle-id]] (db conn)))


