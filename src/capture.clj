(ns capture
  (:use form-analysis
        [clojure.repl :only (source-fn)]
        [clojure.walk :only (macroexpand-all postwalk-replace)])
  (:import java.io.InputStreamReader
           java.io.PushbackReader
           java.io.LineNumberReader))

(defn add-meta [obj & kvs] (with-meta obj (apply assoc (meta obj) kvs)))

(def ^:dynamic *capture* true)

(defmacro cfn
  "Capture the form and attach as meta-data
   and attach the local bindings"
  [& forms]
  (if *capture*  
	  (let [closures (vec (filter (free-vars (cons 'fn* forms)) (keys &env)))]
      (if (empty? closures)
        `(with-meta (fn* ~@forms) {:form '~&form})
        `(with-meta (fn* ~@forms) {:form '~&form :closures (zipmap '~closures ~closures) })))
   `(fn* ~forms)))

(defn file-forms [filepath line-numbers]
  (when-let [strm (.getResourceAsStream (clojure.lang.RT/baseLoader) filepath)]
    (with-open [rdr (LineNumberReader. (InputStreamReader. strm))]
      (doall
        (map (fn [ln]
               (dotimes [_ (- (dec ln) (.getLineNumber rdr))] (.readLine rdr))
               (read (PushbackReader. rdr))) line-numbers)))))

(defn patch [v p]
  (let [m (meta v)]
    (binding [*ns* (:ns m)]
      (alter-meta!
        (eval (p (first (file-forms (:file m) [(:line m)]))))
        (fn [new-meta] (if new-meta (conj new-meta m) m))))))

(defn patch-letfn []
  (patch #'clojure.core/letfn (partial postwalk-replace {'clojure.core/fn 'fn*})))
  
(defn patch-fn []
  (patch #'clojure.core/fn (partial postwalk-replace {'fn* 'capture2/cfn})))

(defn patch-list []
  (alter-var-root 
    #'clojure.core/list 
    (with-meta (fn [list-fn] 
                 (fn [& args] (apply list-fn args))) {:patched true}))
  (alter-meta! #'clojure.core/list dissoc :file :line))

(defn attach-sym [sym]
  (alter-var-root
    (resolve sym)
    #(vary-meta % assoc :symbol sym)))

(defn macro-var? [v] (:macro (meta v)))

(defn fn-var? [v] (and (not (macro-var? v)) (fn? (var-get v))))

(defn forms [vars]
	(reduce into {}
		(map 
		  (fn [[f vars]] 
		    (let [line (comp :line meta)
		          sorted-vars (sort-by line vars)]
		      (zipmap sorted-vars (file-forms f (map line sorted-vars)))))    
		  (group-by (comp :file meta) (filter (comp (every-pred :file :line) meta) vars)))))

(defn fn-emiting-form? [ns form]
  ;(println form)
  (binding [*ns* ns
            *capture* false]
    (->> form
      macroexpand-all
      flatten
      (filter (partial = 'fn*))
      rest
      empty?
      not)))

(defn reval-ns! [n]
  (println "revaluating functions in namespace:" n)
  (let [v->o (->> n ns-interns vals (filter fn-var?) forms)
        fp ;(every-pred 
             (comp (partial = 'defn) first)
             ;(partial fn-emiting-form? n))
        v->f (->> v->o (filter
                         (comp (partial = 'defn) first last)
                       ;  (every-pred
                       ;          (comp (partial = 'defn) first last)
                       ;          (comp (partial fn-emiting-form? n) last))
                       ))
        reval (fn [[v f]] 
                (let [m (meta v)]
;                  (println v)
                  (alter-meta! (eval f) conj m)))]                 
    (binding [*ns* n]
	    (dorun (map reval v->f)))))

(defn reval-all! [] (doall (map reval-ns! (all-ns))))

(defn attach-fn-meta []
  (dorun
	  (for [n (all-ns)
	        [s v] (filter (comp fn-var? last) (ns-interns n))]
       (try
	       (alter-var-root v add-meta :ns n :name (symbol (name (ns-name n)) (name s)))
        (catch Exception e)))))

(defn fn-sym [f]
  (or (:sym (meta f))
      (and (= f list) 'clojure.core/list)))

(defn capture []
  (patch-list)
  (patch-letfn)
  (patch-fn)
  (reval-all!)
  (attach-fn-meta))
