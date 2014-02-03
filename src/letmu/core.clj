(ns letmu.core
  (:import [java.io Writer]))

(defprotocol IStream
  (-sfirst [_ env])
  (-srest [_ env]))

(declare sempty)

(deftype SNil []
  IStream
  (-sfirst [this env] nil)
  (-srest [this env] sempty))

(def snil (SNil.))
(def sempty (Stream. {} snil))

(deftype SVar [id]
  IStream
  (-sfirst [_ env]
    (-sfirst (env id) env))
  (-srest [_ env]
    (-srest (env id) env))
  )

(deftype Mu [id xs]
  IStream
  (-sfirst [_ env]
    (-sfirst xs (assoc env id xs)))
  (-srest [_ env]
    (-srest xs (assoc env id xs)))
  )

(deftype Stream [env root]
  clojure.lang.Sequential
  clojure.lang.Seqable
  (seq [this] this)
  clojure.lang.IPersistentCollection
  ;(count [_])
  ;(empty [_])
  ;(equiv [_ o])
  clojure.lang.ISeq
  (first [_]
    (-sfirst root env))
  (next [_]
    (let [xs (-srest root env)]
      (when-not (identical? (.root ^Stream xs) snil)
        xs)))
  (more [_] ; aka "rest"
    (-srest root env))
  ;(cons [_ o])
  )

(defmethod print-method Stream [^Stream o, ^Writer w]
  ;TODO (print-meta o w)
  (.write w "#<Stream ")
  (print-method (.env o) w)
  (.write w " ")
  (print-method (.root o) w)
  (.write w ">")
  )

(deftype SCons [x xs]
  IStream
  (-sfirst [_ env]
    x)
  (-srest [_ env]
    (Stream. env xs))
  )

(defmacro mu [sym & body]
  (let [id (gensym sym)]
    `(let [~sym (SVar. '~id)]
       (Mu. '~id ~@body))))

;;TODO smart constructors & syntax
;;TODO "local clearing" for captured environments
;;TODO pretty printing

(def s0
  (Stream. {} (SCons. 1 snil)))

(def s1
  (Stream. {} (SCons. 1 (mu v (SCons. 2 v)))))

(def s2
  (Stream. {} (mu v (SCons. 1 (SCons. 2 v)))))


(comment

  (first sempty)
  (-srest snil {})
  (rest sempty)
  (next sempty)

  (first s0)
  (next s0)
  (rest s0)

  (first s1)
  (next s1)
  (rest s1)
  (take 10 s1)

  (first s2)
  (next s2)
  (rest s2)
  (take 10 s2)

)
