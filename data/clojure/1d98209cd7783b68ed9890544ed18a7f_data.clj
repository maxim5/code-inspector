(ns cloneit.data
  (:import (org.joda.time DateTime)))

(def data  (ref {"http://www.bestinclass.dk" {:title "Best in Class" :points 1 :date (DateTime.) :poster "LauJensen"}}))
(def users (ref {"lau.jensen@bestinclass.dk" {:username "LauJensen" :password "way2secret"}
                 "pepijndevos@gmail.com" {:username "pepijndevos" :password "fietspomp"}}))
(def online-users (ref {}))
