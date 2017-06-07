(ns madstap.comfy.core
  "A small collection of functions and macros that (mostly) wouldn't
  be out of place in clojure.core."
  (:refer-clojure :exclude [keep run!])
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
  "Conj a value into a coll, if and only if the value is not nil."
  {:added "0.1.0"}
  ([] (conj))
  ([coll] (conj coll))
  ([coll x]
   (if (some? x) (conj coll x) coll))
  ([coll x & xs]
   (reduce conj-some coll (cons x xs))))


#?(:clj
   (defmacro fn->
     "The same as #(-> % ~@forms)"
     {:added "0.1.0"}
     [& forms]
     `(fn [x#] (-> x# ~@forms))))


#?(:clj
   (defmacro fn->>
     "The same as #(->> % ~@forms)"
     {:added "0.1.0"}
     [& forms]
     `(fn [x#] (->> x# ~@forms))))

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

#?(:clj
   (defmacro forv
     "Like for, but returns a vector. Not lazy."
     {:style/indent 1, :added "0.1.0"}
     [seq-exprs body-expr]
     `(vec (for ~seq-exprs ~body-expr))))

#?(:clj
   (s/fdef for-map
     :args (s/cat :seq-exprs ::seq-exprs, :key-expr any?, :val-expr any?)))

#?(:clj
   (defmacro for-map
     "Like for, but takes a key and value expression and returns a map.
  Multiple equal keys are treated as if by repeated assoc. Not lazy."
     {:style/indent 1, :added "0.1.0"}
     [seq-exprs key-expr val-expr]
     `(into {} (for ~seq-exprs [~key-expr ~val-expr]))))

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
  "Like core/keep, but can take an arbitrary number of colls.
  Returns a lazy sequence of the non-nil results from the application of f
  to the set of first items in each coll, followed by the set of second items
  in each coll, until any of the colls is exhausted.
  f must accept number-of-colls arguments.
  The transducer returned from calling it with only a function,
  can, if needed, accept multiple arguments (like map)."
  {:added "0.1.1"}
  ([f] (comp (map f) (remove nil?)))
  ([f coll] (clojure.core/keep f coll))
  ([f coll & colls]
   (apply sequence (keep f) (cons coll colls))))

(defn run!
  "Like core/run!, but can take an arbitrary number of colls,
  in which case proc is called with number-of-colls arguments, consisting of
  the set of first items in each coll, followed by the set of second items,
  until one of the colls is exchausted. Returns nil."
  {:added "0.1.1"}
  ([proc coll]
   (clojure.core/run! proc coll))
  ([proc coll & colls]
   (dorun (apply map proc (cons coll colls)))))
