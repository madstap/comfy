(ns madstap.comfy.core
  "A small collection of functions and macros that (mostly) wouldn't
  be out of place in clojure.core."
  (:refer-clojure :exclude [keep run! group-by])
  #?(:cljs (:require-macros [madstap.comfy.core]))
  (:require
   [clojure.spec.alpha :as s]
   #?(:clj [clojure.core.specs.alpha])))

(def ^:private nilable-map? (some-fn map? nil?))


(s/fdef deep-merge
  :args (s/* (s/nilable map?))
  :ret (s/nilable map?))

(defn deep-merge
  "Like merge, but also merges nested maps under the same key."
  {:added "0.1.0"}
  [& ms]
  (apply merge-with
         (fn [v1 v2]
           (if (and (nilable-map? v1) (nilable-map? v2))
             (deep-merge v1 v2)
             v2))
         ms))


(s/fdef deep-merge-with
  :args (s/cat :f ifn?, :maps (s/* (s/nilable map?)))
  :ret (s/nilable map?))

(defn deep-merge-with
  "Like merge-with, but also merges nested maps under the same key."
  {:added "0.1.0"}
  [f & ms]
  (apply merge-with
         (fn [v1 v2]
           (if (and (nilable-map? v1) (nilable-map? v2))
             (deep-merge-with f v1 v2)
             (f v1 v2)))
         ms))


(s/fdef one? :args (s/cat :x number?), :ret boolean?)

(defn one?
  "Returns true if x is one, else false."
  {:added "0.1.0"}
  [x] (== 1 x))


(s/fdef two? :args (s/cat :x number?), :ret boolean?)

(defn two?
  "Returns true if x is two, else false."
  {:added "0.1.0"}
  [x] (== 2 x))


(s/fdef conj-some
  :args (s/and (s/cat :coll (s/? (s/nilable coll?)) :xs (s/* any?))
               #(if (:xs %) (contains? % :coll) true))
  :ret (s/nilable coll?))

(defn conj-some
  "conj[oin]. Returns a new collection with the xs
  'added' if and only if they are not nil.
  Otherwise behaves like core/conj."
  {:added "0.1.0"}
  ([] (conj))
  ([coll] (conj coll))
  ([coll x]
   (if (some? x) (conj coll x) coll))
  ([coll x & xs]
   (reduce conj-some coll (cons x xs))))


(defmacro fn->
  "The same as #(-> % ~@forms)"
  {:added "0.1.0"}
  [& forms]
  `(fn [x#] (-> x# ~@forms)))


(defmacro fn->>
  "The same as #(->> % ~@forms)"
  {:added "0.1.0"}
  [& forms]
  `(fn [x#] (->> x# ~@forms)))

;; This spec, and the specs that depend on it need the conditional
;; because :clojure.core.specs.alpha is not yet ported to cljs.
#?(:clj
   (s/def ::seq-exprs
     (s/and vector?
            (s/* (s/alt :binding :clojure.core.specs.alpha/binding
                        :let   (s/cat :k #{:let}   :bindings :clojure.core.specs.alpha/bindings)
                        :when  (s/cat :k #{:when}  :expr any?)
                        :while (s/cat :k #{:while} :expr any?)))
            #(contains? (set (map key %)) :binding))))


#?(:clj
   (s/fdef forv
     :args (s/cat :seq-exprs ::seq-exprs, :body any?)))

(defmacro forv
  "Like for, but returns a vector. Not lazy."
  {:style/indent 1, :added "0.1.0"}
  [seq-exprs body-expr]
  `(vec (for ~seq-exprs ~body-expr)))

#?(:clj
   (s/fdef for-map
     :args (s/cat :seq-exprs ::seq-exprs, :key-expr any?, :val-expr any?)))

(defmacro for-map
  "Like for, but takes a key and value expression and returns a map.
  Multiple equal keys are treated as if by repeated assoc. Not lazy."
  {:style/indent 1, :added "0.1.0"}
  [seq-exprs key-expr val-expr]
  `(into {} (for ~seq-exprs [~key-expr ~val-expr])))


#?(:clj
   (s/fdef forcat
     :args (s/cat :seq-exprs ::seq-exprs, :body-expr any?)))

;; The obvious (apply concat (for ,,,)) solution is not lazy,
;; (mapcat doesn't seem to be, either).
;; Returns a vector to make non-lazyness obvious.
;; (Not consisted with mapcat, but you generally don't care whether something
;;  is a sequence or a vector, you do sometimes care if it's lazy or not.)
(defmacro forcat
  "Like for, but presumes that the body-expr evaluates to a seqable thing,
  and returns a vector of every element from each body. Not lazy."
  {:style/indent 1, :added "0.1.2"}
  [seq-exprs body-expr]
  `(into [] cat (for ~seq-exprs ~body-expr)))


(defn flip
  "Takes a function f and arguments args. Returns a function of
  one argument x that is f applied with x as it's first argument
  and args as the rest.

  Almost the opposite of partial, the difference being that the returned
  function takes exactly one argument, the \"thing\" that's being operated upon,
  instead of an arbitrary number of arguments. This is to reduce confusion.
  Meant to be used with assoc, get, conj, etc. that all take the \"thing\"
  as the first argument.

  The name is taken from haskell, although it's not
  exactly equivalent to the haskell version."
  {:added "0.1.1"}
  [f & args]
  (fn [x] (apply f x args)))


;; Already in medley, but repeated here for symmetry with assoc-in-some
(s/fdef assoc-some
  :args (s/cat :m (s/nilable associative?) :kvs (s/+ (s/cat :k any? :v any?)))
  :ret (s/nilable associative?))

(defn assoc-some
  "Associates a value in an associative structure,
  if and only if the value is not nil."
  {:added "0.1.2"}
  ([m k v]
   (if (some? v) (assoc m k v) m))
  ([m k v & kvs]
   (reduce (fn [acc [k v]]
             (assoc-some acc k v))
           (assoc-some m k v)
           (partition 2 kvs))))


(s/fdef assoc-in-some
  :args (s/cat :m (s/nilable associative?) :ks sequential? :v any?)
  :ret (s/nilable associative?))

(defn assoc-in-some
  "Associates a value in a nested associative structure,
  if and only if the value is not nil."
  {:added "0.1.1"}
  [m ks v]
  (if (some? v) (assoc-in m ks v) m))


(s/fdef keep
  :args (s/cat :f ifn? :colls (s/* seqable?))
  :ret (s/or :transducer fn?
             :coll (s/coll-of some?))
  :fn (fn [{:keys [args ret]}]
        (let [[ret-type _] ret]
          (= ret-type (if (:colls args) :coll :transducer)))))

(defn keep
  "Returns a lazy sequence of the non-nil results of (f item). Note,
  this means false return values will be included.  f must be free of
  side-effects.  Returns a transducer when no collection is provided.

  When given multiple collections, will behave as map. (This includes the transducer).

  A drop-in replacement for core/keep."
  {:added "0.1.1"}
  ([f] (comp (map f) (remove nil?)))
  ([f coll] (clojure.core/keep f coll))
  ([f coll & colls]
   (apply sequence (keep f) (cons coll colls))))


(s/fdef run!
  :args (s/cat :proc ifn? :colls (s/+ seqable?))
  :ret nil?)

(defn run!
  "Runs the supplied procedure (via reduce), for purposes of side
  effects, on successive items in the collection. Returns nil.

  In the case of multiple collections, map is used to run the procedure
  (which will be given number-of-colls arguments),
  but run! is still eager and returns nil.

  A drop-in replacement for core/run!."
  {:added "0.1.1"}
  ([proc coll]
   (clojure.core/run! proc coll))
  ([proc coll & colls]
   (dorun (apply map proc (cons coll colls)))))


(s/fdef group-by
  :args (s/cat :f ifn?, :xform (s/? ifn?), :coll seqable?)
  :ret (s/map-of any? vector?))

(defn group-by
  "Returns a map of the elements of coll keyed by the result of
  f on each element. The value at each key will be a vector of the
  corresponding elements, in the order they appeared in coll.

  When given a transducer, the resulting reducing function will be called on
  the vector and each element as it's added.
  For each different key, a new instance of the reducing function will be created,
  with its own state, if any.

  A drop-in replacement for core/group-by."
  {:added "0.1.1"}
  ([f coll] (clojure.core/group-by f coll))
  ([f xform coll]
   ;; res => result (the vector at each key)
   ;; rfs => reducing functions
   (let [[acc rfs] (reduce (fn [[acc rfs] x]
                             (let [k (f x)
                                   res (get acc k [])

                                   ;; These conditionals are for perf reasons,
                                   ;; to not do more work than necessary.
                                   rf-maybe (get rfs k)
                                   existing-rf? (boolean rf-maybe)
                                   rf (if existing-rf? rf-maybe (xform conj))
                                   rfs (if existing-rf? rfs (assoc rfs k rf))]

                               (if (reduced? res)
                                 [acc rfs]
                                 [(assoc acc k (rf res x)) rfs])))
                           [{} {}]
                           coll)]
     (into {} (map (fn [[k res]] [k ((rfs k) (unreduced res))])) acc))))


(s/fdef frequencies-by
  :args (s/cat :f ifn? :coll seqable?)
  :ret map?)

(defn frequencies-by
  "Returns a map of the distinct values of (f item) in coll
  to the number of times they appear."
  [f coll]
  (reduce (fn [acc x]
            (update acc (f x) (fnil inc 0)))
          {}, coll))
