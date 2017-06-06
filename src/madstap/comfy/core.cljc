(ns madstap.comfy.core
  "A small collection of functions and macros that (mostly) wouldn't
  be out of place in clojure.core."
  (:require
   [clojure.spec.alpha :as s]
   #?(:clj [clojure.core.specs.alpha])))

(def ^:private nilable-map? (some-fn map? nil?))


(s/fdef deep-merge
  :args (s/* (s/nilable map?))
  :ret (s/nilable map?))

(defn deep-merge
  "Like merge, but also merges nested maps under the same key."
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
  [x] (== 1 x))


(s/fdef two? :args (s/cat :x number?), :ret boolean?)

(defn two?
  "Returns true if x is two, else false."
  [x] (== 2 x))


(s/fdef conj-some
  :args (s/and (s/cat :coll (s/? (s/nilable coll?)) :xs (s/* any?))
               #(if (:xs %) (contains? % :coll) true))
  :ret (s/nilable coll?))

(defn conj-some
  "Conj a value into a coll, if and only if the value is not nil."
  ([] (conj))
  ([coll] (conj coll))
  ([coll x]
   (if (some? x) (conj coll x) coll))
  ([coll x & xs]
   (reduce conj-some coll (cons x xs))))


#?(:clj
   (defmacro fn->
     "The same as #(-> % ~@forms)"
     [& forms]
     `(fn [x#] (-> x# ~@forms))))


#?(:clj
   (defmacro fn->>
     "The same as #(->> % ~@forms)"
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
     {:style/indent 1}
     [seq-exprs body-expr]
     `(vec (for ~seq-exprs ~body-expr))))

#?(:clj
   (s/fdef for-map
     :args (s/cat :seq-exprs ::seq-exprs, :key-expr any?, :val-expr any?)))

#?(:clj
   (defmacro for-map
     "Like for, but takes a key and value expression and returns a map.
  Multiple equal keys are treated as if by repeated assoc. Not lazy."
     {:style/indent 1}
     [seq-exprs key-expr val-expr]
     `(into {} (for ~seq-exprs [~key-expr ~val-expr]))))
