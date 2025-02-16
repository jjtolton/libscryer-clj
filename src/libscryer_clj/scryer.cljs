(ns libscryer-clj.scryer)


(defn init []
  (js/console.log "hello"))


(defn create-wam! []
  (.. js/MachineBuilder new build))

(comment
  (def wamo (create-wam!))
  (def query (.. wamo (runQuery "X=1.")))

  (def leaf (.next query))

  (js->clj leaf :keywordize-keys true)
  (.. (:X (js->clj (.. leaf -value -bindings) :keywordize-keys true)) -type)
  (.. (:X (js->clj (.. leaf -value -bindings) :keywordize-keys true)) -integer)

;;
  )


(defn run-query! [^js wam query]
  (.. wam (runQuery query)))




