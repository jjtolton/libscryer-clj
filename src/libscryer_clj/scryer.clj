 (ns libscryer-clj.scryer
  (:require [clojure.string :as str]))
(import 'ScryerJNABindings 'ScryerJNABindings$ScryerProlog)

(comment
  (def bindings (.. ScryerJNABindings$ScryerProlog INSTANCE))

  (def wam (.. bindings scryer_machine_new))

  (.. bindings (scryer_consult_module_string wam ":- use_module(library(clpz))."))

  (def ptr (.. bindings (scryer_run_query wam "3 #= 1 + 2.")))

  (.. ptr (getString 0))

  (.. bindings (scryer_free_c_string  ptr))

  (.. bindings (scryer_machine_free wam)))








(comment
  ;; startup the machine
  (def wam (scryer_machine_new))
  (scryer_machine_free wam)

  (scryer_run_query wam "member(X, [a,\"a\",f(a),\"f(a)\",Y,\"Y\"]) ; dif(X,a), dif(Y,X).)")

  (scryer_consult_module_string wam ":- use_module(library(lists)).")

  ;; consult a module

  ;; start a new lazy query
  (dotimes [_ 5]

    (do (let [p (start_new_query_generator "member(X, [1, 2, 3, 4]).")]
          (println (.. p (getString 0 "UTF-8")))
          (free_c_string p))

        (dotimes [_ 4]
          (time (let [p (run_query_generator)]
                  (println (.. p (getString 0 "UTF-8")))
                  (free_c_string p)))))

    (do (let [p (start_new_query_generator "member(X, [1, 2, 3, 4]).")]
          (println (.. p (getString 0 "UTF-8")))
          (free_c_string p))

        (dotimes [_ 3]
          (let [p (run_query_generator)]
            (println (.. p (getString 0 "UTF-8")))
            (free_c_string p)))

        (let [p (cleanup_query_generator)]
          (println (.. p (getString 0 "UTF-8")))
          (free_c_string p))))

  ;; free the machine

  (machine_free))









