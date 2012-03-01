(ns capture
  (:use form-analysis
        clojure.core.incubator
        [clojure.walk :only (postwalk-replace)]
        [clojure.repl :only (source-fn)]
        ))

; need a more sophisticated returns-fn?

(defn- iterate-state 
  "f is a function which takes state and returns [result new-state]"  
  [f state]
  (->> state f (iterate (comp f second)) (take-while identity) (map first))) 
 
(defn graph-seq [neighbours node]
  (let [i (fn i [[f & r :as s] c v]
            (cond
              (empty? s) (if c (c v))
              (v f) (i r c v)
              :else [f [(neighbours f) (partial i r c) (conj v f)]]))]
    (cons node (iterate-state (partial apply i)
                              [(neighbours node) nil #{node}]))))

(defmacro fn* 
  "Capture the form and attach as meta-data
   and attach the local bindings"
  [& forms]
    (let [closures (vec (filter (free-vars (cons 'fn* forms)) (keys &env)))]
    `(with-meta (fn* ~@forms) 
       {:form '~&form
        :closures (zipmap '~closures ~closures)
        ;:tb (get-thread-bindings)
        })))

(def sym-ns-str #(.getNamespace %))

(def sym-ns #(-?> % sym-ns-str symbol find-ns)) 

(defn resolve-get [sym]
  (if-let [v (resolve sym)]
    (if (bound? v) (var-get v))))

(defn form [f] (-> f meta :form))

(defn sym-var 
  "Returns the var from a fully qualified symbol, 
   doesn't attempt any resolution"
  [sym]
  (if-let [n (sym-ns sym)]
    ((ns-interns n) (symbol (name sym)))))

(defn var-obj [v] (if (bound? v) (var-get v)))

(defn ^:dynamic file-form [sym]
  (if-let [s (source-fn sym)]
    (expand-form (sym-ns sym) #{} (read-string s))))

(defn form-syms 
  "returns a seq of all the symbols within the form"
  [form] (filter symbol? (flatten [form])))

;(defn returns-fn? [form]
;  (->> form form-syms (filter (partial = 'fn*)) rest empty? not)) 

(defn map-sym? [sym]
  (-?> sym sym-var var-obj (every-pred fn? (complement form) file-form)))

(defn map-sym [sym]
  (symbol (str "capture." (.getNamespace sym)) (name sym)))

(defn map-form [form]
  (let [to-map (filter map-sym? (form-syms form))
        old->new (zipmap to-map (map map-sym to-map))]
    (postwalk-replace (assoc old->new 'fn* 'capture/fn*) form)))

(defn intern! [sym]
  (let [split-sym (juxt sym-ns-str name)
	      [nns nname] (->> sym map-sym split-sym (map symbol))]
    (intern (create-ns nns) nname sym)))

(declare capture-fn)

(defn capture-form [form]
  (let [to-map (filter map-sym? (form-syms form))
        old->new (zipmap to-map (map map-sym to-map))]
    (dorun
      (map (comp capture-fn first) (remove (comp resolve last) old->new)))
    (postwalk-replace (assoc old->new 'fn* 'capture/fn*) form)))

(defn capture-fn [sym]
  (let [form (last (file-form sym))
        mapped (map-sym sym)]
    (alter-var-root 
      (intern (sym-ns mapped) (name mapped))
      (fn [_]
        (binding [*ns* (sym-ns sym)]
          (eval (capture-form form)))))))

(def mapped? (comp resolve map-sym))

(defn- to-redef [form]
  (->> form form-syms distinct (filter map-sym?) (remove mapped?)))

(defn all-to-redef [form]
  (reduce into #{}
    (map (partial graph-seq (comp to-redef file-form)) (to-redef form))))

(defn reval! [sym]
;  (println "sym:" sym)
  (binding [*ns* (sym-ns sym)]
    (-?> sym file-form last map-form eval 
      (vary-meta assoc :source-fn sym))))

(defn- 
  capture*
  "takes a form and converts it into something can can be remoted"
  [env form]
  (binding [file-form (memoize file-form)]
	  (let [expanded (expand-form *ns* (or env #{}) form)
	        domap (comp doall map)]
	    (do
	      (domap
	        #(alter-var-root % reval!)
	        (domap intern! (all-to-redef expanded)))
	      (map-form expanded)))))
  
(defmacro
  capture
  "Add this macro around any form which requires functions being captured"
  ([form] (capture* (keys &env) form))
  ([form & more] (cons 'do (map (partial capture* (keys &env)) (cons form more)))))