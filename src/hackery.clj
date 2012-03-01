(ns hackery
 (:import java.io.PushbackReader))

(defn dispatch-reader-macro [ch fun]
 (let [dm (.get (doto (.getDeclaredField clojure.lang.LispReader "dispatchMacros")
                  (.setAccessible true))
                nil)]
   (aset dm (int ch) fun)))

(defn sneak-peek-reader [rdr]
 (let [sb (StringBuilder.)
       reader (proxy [PushbackReader] [rdr]
                (unread [c]
                        (proxy-super unread c)
                        (when-not (zero? (.length sb))
                          (.deleteCharAt sb (dec (.length sb)))))
                (read [] (let [result (proxy-super read)]
                           (.append sb (char result))
                           result)))]
   {:sb sb, :reader reader}))

(defn read-anonymous-fn [rdr paren]
 (let [{:keys [sb reader]} (sneak-peek-reader rdr)
       f (.invoke (clojure.lang.LispReader$FnReader.) reader paren)]
   (with-meta f {:source (str "#" sb)})))
