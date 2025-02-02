(ns libscryer-clj.scryer
  (:require
   [clojure.data.json :as json]
   [clojure.edn :as edn]
   [clojure.string :as str])
  (:import
   ScryerJNABindings
   ScryerJNABindings$AutoFreeCString
   ScryerJNABindings$ScryerMachine
   ScryerJNABindings$ScryerMachine$ScryerPrologQueryIter))

(set! *warn-on-reflection* true)

(defonce scryer-bindings (atom nil))

(defonce the-wam (atom nil))

(defonce the-query-state (atom nil))

(defn load-scryer-edn! []
  (-> (try (slurp "scryer.edn")
           (catch java.io.FileNotFoundException _ "{}"))
      edn/read-string))

(defn create-wam! []
  (when-not ^ScryerJNABindings @scryer-bindings
    (throw (Error. "Scryer bindings not loaded. Did you call initialize?")))
  (.. ^ScryerJNABindings @scryer-bindings getScryerMachine))

(defn load-the-wam! [prelude prelude-path ^ScryerJNABindings scryer]
  (let [wam ^ScryerJNABindings$ScryerMachine (.. scryer getScryerMachine)]
    (reset! the-wam wam)
    (when prelude
      (.. ^ScryerJNABindings$ScryerMachine wam (consultModuleString "prelude" prelude)))
    (when prelude-path
      (let [prelude-contents (slurp prelude-path)]
        (.. wam (consultModuleString "prelude-contents" prelude-contents))))))

(defn initialize!
  ([] (initialize! {}))
  ([{scryer-path :libscryer-prolog-path
     :keys       [make-the-wam? prelude prelude-path reload-the-wam?]
     :or         {make-the-wam?   true
                  reload-the-wam? false}
     :as         opts}]
   (let [{scryer-path :libscryer-prolog-path
          :keys       [make-the-wam? prelude prelude-path reload-the-wam?]
          :or         {make-the-wam?   true
                       reload-the-wam? false}} (merge (load-scryer-edn!) opts)
         scryer                                ^ScryerJNABindings (ScryerJNABindings. scryer-path)]
     (when-not @scryer-bindings
       (reset! scryer-bindings scryer))
     (when (or (and reload-the-wam? @the-wam)
               (and make-the-wam? (not @the-wam)))
       (load-the-wam! prelude prelude-path scryer)))))

(defn ^:private prolog-logic-var->lisp-logic-var [key]
  (-> (name key)
      str/lower-case
      (as-> lvar-string (apply str (first "?") lvar-string))
      symbol))

(comment
  (prolog-logic-var->lisp-logic-var :X) ;;=> ?x
  ;;
  )

(defn ^:private lisp-logic-var->prolog-logic-var [lvar]
  (-> (str lvar)
      (subs 1)
      str/capitalize
      keyword))

(comment
  (lisp-logic-var->prolog-logic-var '?hey) ;;=> :Hey
  ;;
  )

(defn cleanup-query-iterator! [^ScryerJNABindings$ScryerMachine$ScryerPrologQueryIter query-iterator]
  (try (.close query-iterator)
       (catch IllegalStateException _
         nil))
  (reset! the-query-state nil))

(defn ^:private process-prolog-result [^ScryerJNABindings$AutoFreeCString ptr]
  (when ptr
    (let [res (-> (json/read-str (.getValue ptr) :key-fn keyword))]
      (if (= "ok" (:status res))
        (if (contains? res :result)
          (let [result (:result res)]
            (if (boolean? result)
              (list result)
              (letfn [(fix-binding-value [m k v]
                        (assoc m k (fix-functor (fix-vars v))))
                      (fix-bindings [node]
                        (as-> (update-keys (:bindings node) prolog-logic-var->lisp-logic-var) node
                          (reduce-kv fix-binding-value node node)))
                      (fix-vars [node]
                        (if (and (map? node)
                                 (:bindings node))
                          {:bindings (fix-bindings node)}
                          node))
                      (fix-functor [node]
                        (cond
                          (:functor node)  (list* (symbol (:functor node)) (map fix-functor (:args node)) )
                          (:atom node)     [(:atom node)]
                          (vector? node)   (map fix-functor node)
                          (:variable node) (symbol (:variable node))
                          :else           node))]
                (fix-functor (fix-vars result)))))
          :ok)
        (throw (ex-info (:error res) res))))))


(defn get-scryer-bindings ^ScryerJNABindings []
  (if-let [bindings @scryer-bindings]
    bindings
    (throw (RuntimeException. "Scryer Prolog not initialied!"))))

(defn get-wam! ^ScryerJNABindings$ScryerMachine []
  (.. ^ScryerJNABindings (get-scryer-bindings) getScryerMachine))

(defn get-the-wam! ^ScryerJNABindings$ScryerMachine []
  (let [the-wam ^ScryerJNABindings$ScryerMachine @the-wam]
    (when-not the-wam
      (throw (RuntimeException. "Scryer Prolog machine not initialized. Did you run `scryer-initialize!` with `{:make-wam? true}`?")))
    the-wam)) 

(defn wam-consult!
  ([^ScryerJNABindings$ScryerMachine wam ^String prolog] (wam-consult! wam "facts" prolog))
  ([^ScryerJNABindings$ScryerMachine wam ^String module-name ^String prolog]
   (.. wam (consultModuleString module-name prolog))
   :ok))

(defn consult! ([^String prolog] (wam-consult! ^ScryerJNABindings$ScryerMachine (get-the-wam!) "facts" prolog))
  ([^String module-name ^String prolog]
   (.. ^ScryerJNABindings$ScryerMachine (get-the-wam!) (consultModuleString module-name prolog))))

(defn terminated-query [q]
  (let [q (str/trim q)]
    (if (not (str/ends-with? q "."))
      (str q ".")
      q)))

(defn wam-query! ^String [^ScryerJNABindings$ScryerMachine wam ^String query]
  (with-open [ptr ^ScryerJNABindings$AutoFreeCString (.. wam (runQuery (terminated-query query)))]
    (->> (process-prolog-result ptr)
         (map :bindings)
         (map (fn [m] (update-keys m prolog-logic-var->lisp-logic-var))))))

(defn query! ^String [^String query]
  (wam-query! ^ScryerJNABindings$ScryerMachine (get-the-wam!) (terminated-query query)))

(defn wam-get-lazy-query-iterator! ^ScryerJNABindings$ScryerMachine$ScryerPrologQueryIter
  [^ScryerJNABindings$ScryerMachine wam ^String query]
  (.. wam (generativeQuery (terminated-query query))))

(defn get-lazy-query-iterator! ^ScryerJNABindings$ScryerMachine$ScryerPrologQueryIter
  [^String query]
  (wam-get-lazy-query-iterator! (get-the-wam!) (terminated-query query)))

(defn lazy-query-from-iterator!
  ([^ScryerJNABindings$ScryerMachine$ScryerPrologQueryIter query-iterator]
   (eduction (take-while some?)
             (map :bindings)
             (repeatedly (fn [] (process-prolog-result (.next query-iterator))))))
  ([^ScryerJNABindings$ScryerMachine$ScryerPrologQueryIter query-iterator n]
   (eduction
         (take-while some?)
         (map :bindings)
         (repeatedly n (fn [] (process-prolog-result (.next query-iterator)))))))

(defn wam-transduce-query! [^ScryerJNABindings$ScryerMachine wam xform f coll ^String query]
  (with-open [lazy ^ScryerJNABindings$ScryerMachine$ScryerPrologQueryIter (wam-get-lazy-query-iterator! wam (terminated-query query))]
    (transduce
     xform
     f
     coll
     (lazy-query-from-iterator! lazy))))

(defn transduce-query! [xform f coll ^String query]
  (wam-transduce-query! ^ScryerJNABindings$ScryerMachine (get-the-wam!) xform f coll (terminated-query query)))


(comment
  (initialize!
   {:libscryer-prolog-path (System/getenv "SCRYER_SHARED_LIBRARY_PATH")
    :prelude
    ":- use_module(library(clpz)).
     :- use_module(library(dif)).
     :- use_module(library(lists))."
    :reload-the-wam? true})

  (query! "X #= 1 + 2.")
  (query! "3 #= 1 + 2.")

  ;; don't do this!!  will result in resource leaks
  ;; (let [query      (get-lazy-query-iterator! "X in 1..10, indomain(X).")
  ;;       lazy-query (lazy-query! query)
  ;;       res        (into [] (comp (take-while map?) (take 20)) lazy-query)]
  ;;   (.close query)
  ;;   res)

  (transduce-query!
   (comp
    (map :bindings))
   (completing conj)
   []
   "X in 1..10, indomain(X).")

  (transduce-query!
   (comp
    (map identity))
   (completing conj)
   []
   "true.")

  (with-open [query (get-lazy-query-iterator! "X in 1..10, indomain(X).")]
    (into [] (take 20) (lazy-query-from-iterator! query)))
  (cleanup-query-iterator! @the-query-state)

  ;; don't do this either!
  ;; (let [query (get-lazy-query-iterator! "X in 1..10, indomain(X).")]
  ;;   (try
  ;;     (let [lazy-query (lazy-query! query)]
  ;;       (take 5 lazy-query))
  ;;     (finally
  ;;       (.close query))))

  ;; throws execption

  (try
    (with-open [query (get-lazy-query-iterator! "X in 1..10, indomain(X).")]
      (lazy-query-from-iterator! query 5))
    (catch java.lang.IllegalStateException _
      :must-realize-results-before-closing-query))

  (with-open [query (get-lazy-query-iterator! "true.")]
    (println (repeatedly 5 (fn [] (process-prolog-result (.next query)))))
    (dotimes [_ 12]
      (println (process-prolog-result (.next query)))))

  (def query-iter (get-lazy-query-iterator! "X in 1..10, indomain(X)."))
  (cleanup-query-iterator! @the-query-state)

  (def query* (lazy-query-from-iterator! query-iter))
  (next query*))


(defn initialize-global-wam! []
  (when-let [scryer-config (not-empty (load-scryer-edn!))]
    (when (true? (:auto-intialize scryer-config))
      (initialize! scryer-config))))


(comment
  (initialize!)
  (initialize-global-wam!)

  (def source (slurp "docs/examples/jugpour.pl"))
  (consult! source)

  (with-open [lazy-query-iterator (get-lazy-query-iterator! "solve(N, Moves)")]
    (into [] (take 3) (lazy-query-from-iterator! lazy-query-iterator)))

  (wam-query! (create-wam!) "member(X, [1, 2, 3])")

  (let [wam (create-wam!)]
    (wam-consult! wam ":- use_module(library(lists)).")
    (wam-query! wam "member(X, [1, 2, 3])"))

  (query! "member(X, [1, 2, 3])")

  (with-open [query (get-lazy-query-iterator! "solve(N, Moves)")]
    (time (first (lazy-query-from-iterator! query))))

  ;; =>{:bindings {?moves ((fill ["a"]) (from_to ["a"] ["b"]) (from_to ["a"] ["c"])), ?n 3}}

  (with-open [query (get-lazy-query-iterator! "X=f(a(b(1)), a(b(2)))")]
    (into [] (take 1 (lazy-query-from-iterator! query))))

  (let [wam (get-wam!)]
    (wam-consult! wam (slurp "docs/examples/jugpour.pl"))
    (with-open [query (wam-get-lazy-query-iterator! wam "solve(N, Moves)")]
      (into [] (lazy-query-from-iterator! query 1))))

  (time
   (with-open [query (wam-get-lazy-query-iterator! (get-wam!) "X=f(a(b(1)), a(b(2)))")]
     (time (first (lazy-query-from-iterator! query 1)))))

  ;; => [{:bindings {?x (f (a (b 1)) (a (b 2)))}}]

  (with-open [query (wam-get-lazy-query-iterator! (get-wam!) "X=1, Y=X")]
    (first (lazy-query-from-iterator! query)))

  (with-open [query (wam-get-lazy-query-iterator! (get-wam!) "X=[1,2,3]")]
    (first (lazy-query-from-iterator! query)))

  (transduce-query!
   (map :bindings)
   (completing conj)
   []
   "X in 1..10, indomain(X).")

  (transduce-query! (map :bindings)
                    (fn ([res] (persistent! res))
                      ([res next]
                       (conj! res next)))
                    (transient [])
                    (str/join "," ["3 #= A+B"
                                   "[A,B] ins -10..10"
                                   "label([A,B])"])))
