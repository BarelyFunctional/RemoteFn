(ns form-analysis
  (:use [clojure.walk :only (walk)]
        [clojure.set :only (union difference)]))

(defn- mapunion [f & colls] (apply union (apply map f colls)))

(defmulti 
  ^{:dynamic true}  
  free-vars class)

(defn- coll-free-vars [coll] 
  (mapunion free-vars coll))

(defmethod free-vars :default [_] #{})

(defmethod free-vars clojure.lang.Symbol [form] #{form})

(defmulti list-free-vars first)
(defmethod list-free-vars :default [form] (coll-free-vars form))

(def symbols (comp (partial into #{}) (partial filter symbol?) flatten vector))

; reduce across the bindings? keep track of whats been defined and whats external
; return two sets
(defmethod list-free-vars 'fn* [form]
  (let [free #(let [f (fn [[params & forms]]
                        (difference
                          (coll-free-vars forms)
                          (symbols params)))]
                (if (seq? (first %)) (mapunion f %) (f %)))]
    
    (if (symbol? (second form))
      (disj (free (drop 2 form)) (second form))
      (free (rest form)))))

(defn- bindings-forms-free-vars [[bindings & forms]]
  (let [[free bound] (reduce 
                       (fn [[free bound] [b v]]
                         [(union (apply disj (free-vars v) bound) free)
                          (union bound (symbols b))])
                       [#{} #{}]
                       (partition 2 bindings))]
    (union (difference (coll-free-vars forms) bound) free)))

(defmethod list-free-vars 'def [form] (disj (free-vars (drop 2 form)) (second form)))

(defmethod list-free-vars 'let* [form] (bindings-forms-free-vars (rest form)))

(defmethod list-free-vars 'loop* [form] (bindings-forms-free-vars (rest form)))

(defmethod free-vars clojure.lang.ISeq [form] (list-free-vars form))

(defmethod free-vars clojure.lang.IPersistentCollection [form] (coll-free-vars form))

(defmethod free-vars clojure.lang.IPersistentMap [form]
  (coll-free-vars (apply concat (seq form))))

(declare expand-form)

(defn qualify [^clojure.lang.Namespace n ^clojure.lang.Symbol sym] 
  (let [v (ns-resolve n sym)]
    (if (and v (var? v))
      (symbol (name (ns-name (.ns v))) (name (.sym v)))
      sym)))

(def syms #(->> [%] flatten (filter symbol?)))

(defn- resolve-let-form [n locals [bindings & exprs]]
  (let [f (fn [[locals resolved-bindings] [k v]]
            [(into locals (syms k))
             (into resolved-bindings [k (expand-form n locals v)])])
        [locals resolved-bindings] (reduce f [locals []] (partition 2 bindings))]
    (cons resolved-bindings (map (partial expand-form n locals) exprs))))

(def syms (comp distinct (partial filter symbol?) flatten))

(defn- resolve-fn-body [n locals body]
  (let [f (fn [[bindings & exprs]]
            (cons bindings 
              (let [syms (into locals (syms bindings))]
                (map (partial expand-form n syms) exprs))))]    
	  (if (vector? (first body))
      (f body)
	    (map f body))))

(defn- resolve-fn-form [n locals [fst & rst :as all]]
  (if (symbol? fst)
    (cons fst (resolve-fn-body n (conj locals fst) rst))
    (resolve-fn-body n locals all)))

(defn ^:dynamic expand-form [^clojure.lang.Namespace n locals form]
  (let [form (macroexpand form)
        recurse (partial expand-form n locals)]
		(condp apply [form]
      (every-pred coll? empty?) form
      seq? (let [[fst & rst] form]
             (condp = fst
	             'fn* (cons fst (resolve-fn-form n locals rst))
	             'let* (cons fst (resolve-let-form n locals rst))               
	             'def (let [[sym & init] rst]
                     (list* fst sym (map recurse init)))
               (map recurse form)))
	    symbol? (if (locals form) form (or (qualify n form) form))
      (walk recurse identity form))))