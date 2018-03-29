(ns janus.test.dsl-test
  [:use [janus.dsl]]
  [:require [midje.sweet :refer :all]])

(unfinished)

(fact "matching-jsonpath creates clauses as required"
            (matching-jsonpath "path" :of-type :type) => [:json-path "path" :of-type :type]
            (matching-jsonpath "foo" :of-type :object 
                         (matching-jsonpath "bar" :matching ...amazing-regex...)) => 
                         [:json-path "foo" :of-type :object 
                          [[:json-path "bar" :matching ...amazing-regex...]]])

(fact "should-have can take in a keyword"
            (should-have :foo) => [:fn :foo {:key :foo} []])

(fact "each for checking colls"
      (each ..subclause..) => [:each [..subclause..]])

(fact "with-length-between"
      (with-length-between 0 100) [:with-length-between 0 100])

(fact "regex match"
      (should-match ..reg..) =>  
      (contains [:all])) ;; not testing fn! ... or actually much. eek
;; [:matching-pred number? "number"]

(fact "should-have takes in subclauses"
            (should-have :foo (of-type :number)) => [:fn :foo {:key :foo} [..subclause...]]
            (provided (of-type :number) => ...subclause...)  )

(fact (of-type :number) => [:predfn number? {:type :number}])

(fact (with-range 0 10) => [:in-range {:min 0 :max 10}])

(fact "url creates a property containing the path"
            (url "path") => [:property {:name "url" :value "path"}])

(fact "method creates a property containing the method"
            (method :meth) => [:method :meth])

(fact "status creates a property containing the status"
            (status 105) => [:status 105])

(fact "body creates a body object with a type and data or matching clauses"
            (json-body (equal-to {:sample "obj"}))
            => [:json-body [[:equal-to {:sample "obj"}]]]
            (json-body (equal-to "foo")) 
            => [:json-body [[:equal-to "foo"]]])


(fact "header creates a header"
            (header "Name" "Value") => [:header {:name "Name" :value "Value"}])

(fact "request creates a request with matchers inside"
            (request (header "Name" "Value")) => [[:header {:name "Name" :value "Value"}]])

(fact "respons creates a response with matchers inside"
            (response (header "Name" "Value")) => [[:header {:name "Name" :value "Value"}]])


(defn contract-with [check-key expected-value]
  (chatty-checker [actual-contract]
                  (= expected-value (check-key (nth actual-contract 1)))))


(fact "defining a contract as a request and response pairing"
            (contract "sample" (request) (response)) => {:name "sample" :request [] :response []})

(fact "defining a contract with an endpoint"
            (contract "sample" "/uri" (request) (response)) => {:name "sample" :endpoint "/uri" :request [] :response []})

(fact "defining a service as a cluster of contracts "
            (service "sample" 
                     (contract "first contract" "/start" (request) (response))
                     (contract "second contract" (request) (response))) => 
                     { :name "sample" 
                      :contracts [{:endpoint "/start", 
                                   :name "first contract", 
                                   :request [], :response []} 
                                  {:name "second contract", :request [], :response []}]})

(fact "loading a DSL program"
            (construct-domain '(service "sample")) => {:name "sample", :contracts []})
