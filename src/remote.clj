(ns remote
  (:use form-analysis
        capture2))

; special replace function, symbol-> val use lexical scope
(defn lexical-replace [smap form]
  
  )

(defn lexical-bindings [form]
  
  )

; walk two forms, zip forms together?
; transform form, at each level add sym-defs
; transform form, vector sym-defs -> form
; function which takes form and returns defined symbols

; top level objects are tracked across hosts... closures always passed across entirely


; inline depth-first?

; create a function 
; inline simple values?
; closures is a simple 
; can a function be replaced by its symbol?
; symbol replacements and
(defn serialize-fn [f]
  (let [{:keys [name closures form]} (meta f)]
    (if name name
	    (if closures
        (fmap serialize 
        ; is there only one occurance of the symbol in the form?
        ; is so inline the closure
	      (list 'let (vec (apply concat closures)) form)
	      form))))

(defn inline-bindings [env]
  (clojure.walk/prewalk
    #(if (fn? %) (serialize-fn %) %) env))

; 
; given a set of free vars generate a list if symbol -> var mappings.
; chasing down closures where appropiate
; build let around form
;
; with form + full env can deduce global set
;

(defn run-on* [props env form]
  (let [expanded (expand-form env #{} form)
        free (free-vars expanded)]
    ; are any free vars functions?
    ; 1. full capture
    
    
    )
  (free-vars form)
  ; are any of the free vars functions?
  ; create a package
  ; what vars 
  )

(defmacro run-on [props torun]
  ; analyze torun, package execution context
  ; gather list of all global functions
  
  
  )

; props can specify max-nodes, set to 1 to run on 1 node
; stream the coll and stream the result
(defmacro rmap [props f coll]
    
  )
  

