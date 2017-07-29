(ns madstap.comfy.core
  "A small collection of functions and macros that (mostly) wouldn't
  be out of place in clojure.core."
  (:refer-clojure :exclude [keep run! group-by])
  #?(:cljs (:require-macros [madstap.comfy.core :refer [forv]]))
  (:require
   [clojure.string :as str]
   [madstap.comfy.walk :as walk]
   [clojure.spec.alpha :as s]
   #?(:clj [clojure.edn :as edn])
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
  Multiple equal keys are treated as if by repeated assoc (last one wins).
  Not lazy."
  {:style/indent 1, :added "0.1.0"}
  [seq-exprs key-expr val-expr]
  `(into {} (for ~seq-exprs [~key-expr ~val-expr])))


(defn join-seqs
  "Lazily concatenates a sequence-of-sequences into a flat sequence."
  {:no-doc true}
  [seq-of-seqs]
  (lazy-seq
   (when-let [s (seq seq-of-seqs)]
     (concat (first s) (join-seqs (rest s))))))


#?(:clj
   (s/fdef forcat
     :args (s/cat :seq-exprs ::seq-exprs, :body-expr any?)))

(defmacro forcat
  "Like for, but presumes that the body-expr evaluates to a seqable thing,
  and returns a lazy sequence of every element from each body."
  {:style/indent 1, :added "0.1.2"}
  [seq-exprs body-expr]
  `(join-seqs (for ~seq-exprs ~body-expr)))


#?(:clj
   (s/fdef forcatv
     :args (s/cat :seq-exprs ::seq-exprs, :body-expr any?)))

(defmacro forcatv
  "Like for, but presumes that the body-expr evaluates to a seqable thing,
  and returns a vector of every element from each body. Not lazy."
  {:style/indent 1, :added "0.2.0"}
  [seq-exprs body-expr]
  `(into [] cat (for ~seq-exprs ~body-expr)))


(s/fdef flip
  :args (s/cat :f ifn? :args (s/* any?))
  :ret ifn?)

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
  :fn (fn [{{:keys [colls]} :args, [ret-type _] :ret}]
        (= ret-type (if colls :coll :transducer))))

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
                           [{} {}], coll)]

     (into {} (map (fn [[k res]] [k ((rfs k) (unreduced res))])) acc))))


(s/fdef frequencies-by
  :args (s/cat :f ifn? :coll seqable?)
  :ret (s/map-of any? int?))

(defn frequencies-by
  "Returns a map of the distinct values of (f item) in coll
  to the number of times they appear."
  {:added "0.1.2"}
  [f coll]
  (reduce (fn [acc x]
            (update acc (f x) (fnil inc 0)))
          {}, coll))


(defn strip-leading-zeroes
  {:no-doc true}
  [s]
  (let [negative? (str/starts-with? s "-")]
    (str (when negative? \-) (last (re-find #"(0+)?(\d+)" s)))))

(s/fdef str->int
  :args (s/cat :s (s/nilable string?))
  :ret (s/nilable int?))

(defn str->int
  "Parses a string to an integer. Returns nil if the string has the
  wrong format or is nil. Leading zeroes are ignored."
  {:addded "0.2.1"}
  [s]
  (when (and s (re-find #"^[-|+]?\d+$" s))
    #?(:clj (edn/read-string (strip-leading-zeroes s))
       :cljs (js/parseInt s 10))))


(s/fdef str->dec
  :args (s/cat :s (s/nilable string?))
  :ret (s/nilable double?))

(defn str->dec
  "Parses a decimal number from a string. Returns nil if the string
  has the wrong format or is nil. Leading zeroes are ignored."
  {:added "0.2.3"}
  [s]
  (when (and s (re-find #"^[-|+]?(\d+)?\.?(\d+)?$" s) (not= s "."))
    #?(:clj (Double/parseDouble s)
       :cljs (js/parseFloat s))))


(defmacro <<->
  "Turns a thread-last macro into a thread first one.

  (->> (range 3)
       (mapv inc)
       (<<->
        (conj :foo))) ;=> [1 2 3 :foo]"
  {:added "0.2.3"}
  [& forms]
  `(-> ~(last forms) ~@(butlast forms)))


;;;; Walk-reduce
;; Reduce and transduce versions of the functions in clojure.walk

(s/fdef prewalk-reduce
  :args (s/cat :rf ifn?, :init (s/? any?), :form any?))

(defn prewalk-reduce
  "Prewalk reduce.
  Performs a depth-first, pre-order traversal of form, calling rf on
  init and each sub-form.
  Will use (rf) as init value if not supplied one. (Like transduce, unlike reduce.)"
  {:added "1.0.0"}
  ([rf form]
   (prewalk-reduce rf (rf) form))
  ([rf init form]
   (letfn [(step [acc x]
             (if (seqable? x)
               (reduce step (rf acc x) x)
               (rf acc x)))]
     (step init form))))


(s/fdef postwalk-reduce
  :args (s/cat :rf ifn?, :init (s/? any?), :form any?))

(defn postwalk-reduce
  "Prewalk reduce.
  Performs a depth-first, pre-order traversal of form calling rf on
  init and each sub-form.
  Will use (rf) as init walue it not supplied one. (Like transduce, unlike reduce.)"
  {:added "1.0.0"}
  ([rf form]
   (postwalk-reduce rf (rf) form))
  ([rf init form]
   (letfn [(step [acc x]
             (if (seqable? x)
               (rf (reduce step acc x) x)
               (rf acc x)))]
     (step init form))))


(s/fdef prewalk-transduce
  :args (s/cat :xform ifn?, :rf ifn?, :init any?, :form any?))

(defn prewalk-transduce
  "Prewalk transduce.
  Traverses form in depth-first, pre-order, behaving otherwise like transduce."
  {:added "0.3.0"}
  ([xform rf form]
   (prewalk-transduce xform rf (rf) form))
  ([xform rf init form]
   (let [f (xform rf)
         res (prewalk-reduce f init form)]
     (f res))))


(s/fdef postwalk-transduce
  :args (s/cat :xform ifn?, :rf ifn?, :init any?, :form any?))

(defn postwalk-transduce
  "Postwalk transduce.
  Traverses form in depth-first, post-order, behaving otherwise like transduce."
  {:added "0.3.0"}
  ([xform rf form]
   (postwalk-transduce xform rf (rf) form))
  ([xform rf init form]
   (let [f (xform rf)
         res (postwalk-reduce f init form)]
     (f res))))


#?(:clj
   (s/fdef syms-in-binding
     :args (s/cat :b :clojure.core.specs.alpha/binding-form)
     :ret (s/coll-of simple-symbol? :kind set?)))

(defn syms-in-binding
  "Returns the set of symbols (and keywords that act as symbols)
  in a binding form."
  {:no-doc true
   :added "0.2.3"}
  [b]
  (let [simple-symbol (comp symbol name)]
    (if (symbol? b)
      [b]
      (prewalk-reduce (fn [acc x]
                        (cond (symbol? x)
                              (conj acc (simple-symbol x))

                              ;;; Special-case:
                              ;;   Keywords act like symbols in {:keys [:foo]}
                              (and #?(:clj (map-entry? x)
                                      :cljs (some-fn vector? #(two? (count %))))
                                   (= :keys (first x)))
                              (into acc
                                    (comp (filter keyword?) (map simple-symbol))
                                    (second x))

                              :else acc))
                       #{}, b))))


#?(:clj
   (s/fdef defs
     :args (s/cat :binding :clojure.core.specs.alpha/binding-form, :body any?)))

(defmacro defs
  "defs(tructure)
   Like def, but can take a binding form instead of a symbol to
   destructure the results of the body.
   Doesn't support docstrings or other metadata."
  {:added "0.2.3"}
  [binding body]
  `(let [~binding ~body]
     ~(forv [sym (syms-in-binding binding)]
        `(def ~sym ~sym))))
