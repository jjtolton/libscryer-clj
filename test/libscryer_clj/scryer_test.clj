(ns libscryer-clj.scryer-test
  (:require [libscryer-clj.scryer :as scryer :reload :all]
            [clojure.test :as t]))



(scryer/initialize!)
(t/deftest create-wam-load-context-solve-query
  (t/testing "libscryer can loading bindings, create wam, consult wam, run query and lazy query")
  (let [wam (scryer/create-wam!)]
    (scryer/wam-consult! wam ":- use_module(library(lists)).")
    (let [result (scryer/wam-query! wam "member(X, [1, 2, 3])")]
      (with-open [query (scryer/wam-get-lazy-query-iterator! wam "member(X, [1, 2, 3])")]
        (let [result1 (into [] (take (count result)) (scryer/lazy-query-from-iterator! query))]
          (t/is (= result result1)))))))







