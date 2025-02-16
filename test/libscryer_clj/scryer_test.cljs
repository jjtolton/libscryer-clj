(ns libscryer-clj.scryer-test
  (:require [libscryer-clj.scryer :as scryer]
            [cljs.test :as t :include-macros true]))

(t/deftest basic-test
  (t/testing "just test"
    (t/is (= 1 1))))

(t/deftest test-create-machine-run-query
  (t/testing "wam creation, query run"
    (let [wam   (scryer/create-wam!)
          query (scryer/run-query! wam "X=1.")]
      query
      (t/is (= (.. ^js (:X (js->clj (.. query next -value -bindings) :keywordize-keys true))
                   -integer)
               (js/BigInt 1))))))


