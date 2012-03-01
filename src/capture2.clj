(ns capture2
  (:use [clojure.walk :only (macroexpand-all)])
  (:import java.io.InputStreamReader
           java.io.PushbackReader
           java.io.LineNumberReader))

; fix-list
(alter-var-root #'clojure.core/list (fn [list-fn] (fn [& args] (apply list-fn args))))
(alter-meta! #'clojure.core/list dissoc :file :line)

(defn file-forms [filepath line-numbers]
  (when-let [strm (.getResourceAsStream (clojure.lang.RT/baseLoader) filepath)]
    (with-open [rdr (LineNumberReader. (InputStreamReader. strm))]
      (doall
        (map (fn [ln]
               (dotimes [_ (- (dec ln) (.getLineNumber rdr))] (.readLine rdr))
               (read (PushbackReader. rdr))) line-numbers)))))

(defn attach-sym [sym]
  (alter-var-root
    (resolve sym)
    #(vary-meta % assoc :symbol sym)))

(defn macro-var? [v] (:macro (meta v)))

(defn fn-var? [v] (and (not (macro-var? v)) (fn? (var-get v))))

; question does the macro introduce a new fn?
; just redef the forms with more than 1 fn, that will work

(defn all-vars [] (mapcat (comp vals ns-interns) (all-ns)))

(defn forms [vars]
	(reduce into {}
		(map 
		  (fn [[f vars]] 
		    (let [line (comp :line meta)
		          sorted-vars (sort-by line vars)]
		      (zipmap sorted-vars (file-forms f (map line sorted-vars)))))    
		  (group-by (comp :file meta) (filter (comp (every-pred :file :line) meta) vars)))))

(defn redefn []
  (doall
  (for [n (all-ns)]
   (do
    (println "Namespace:" n)
    (binding [*ns* n]
      (doall
      (for [[v f] (forms (filter fn-var? (vals (ns-interns n))))]
        (do
          (println v)
;        (if-not (->> f macroexpand-all vector flatten (filter (partial = 'fn*)) (drop 1) empty?)
          (eval f)))))))))

; remember to restore the meta-data
; define a function which creates and caches itself?
(defn lazy-fn [v form]
  (fn [& args]
    (println "In function")
    (let [f (eval form)]
      (alter-var-root v (fn [_] f))
      (apply f args))))

; attach symbols to start with?
; fix list so that it can take meta data?

(defn reval [n v full-form]
  (alter-var-root v
    (fn [old-fn]    
      (binding [*ns* n]
        (-> full-form
	        macroexpand
	        last
	        eval
	        (with-meta (meta old-fn)))))))
                
(defn lazy-reval [n v full-form]
  (alter-var-root v
    (fn [old-fn]
      (with-meta
        (fn [& args]
          (apply (reval n v full-form) (apply args)))
        (meta old-fn)
        ))))

(defn attach-forms [vars]
  (doall
	  (map (fn [[v f]]
	         (alter-var-root v (fn [o] (try
                                      (vary-meta o assoc :form f)
                                      (catch UnsupportedOperationException _ o)
                                      (catch ClassCastException _ o)))))
	    (forms vars))))

(defn test1 [] 1)

(defn test2 [] (test1))

;(defn attach-syms [syms]
;  (doall
;    (map (alter-var-root (resolve %) (var-meta  
  
;(defn foobar []
;  
;  )

(defmacro attach-def [form]
  `(with-meta ~form {:form '~form}))

; inside the capture macro bind all functions to their definition