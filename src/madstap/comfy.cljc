(ns madstap.comfy
  "A small collection of functions and macros that (mostly) wouldn't
  be out of place in clojure.core."
  (:refer-clojure :exclude [keep run! group-by for])
  #?(:cljs (:require-macros [madstap.comfy :refer [for]]))
  (:require
   [clojure.string :as str]
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


;; This spec, and the specs that depend on it need the conditional
;; because :clojure.core.specs.alpha is not yet ported to cljs.
#?(:clj
   (s/def ::seq-exprs
     (s/and vector?
            (s/* (s/alt :binding :clojure.core.specs.alpha/binding
                        :let   (s/cat :k #{:let}   :bindings :clojure.core.specs.alpha/bindings)
                        :when  (s/cat :k #{:when}  :expr any?)
                        :while (s/cat :k #{:while} :expr any?)
                        :into  (s/cat :k #{:into}  :coll any?)))
            #(= :binding (ffirst %))
            #(>= 1 (count (keep (comp #{:into} key) %))))))

(defn- into-coll
  {:no-doc true}
  [seq-exprs]
  (->> seq-exprs (partition 2) (filter #(= :into (first %))) first second))

(defn- remove-into-coll
  {:no-doc true}
  [seq-exprs]
  (->> seq-exprs (partition 2) (remove #(= :into (first %))) (apply concat) vec))

(def ^{:no-doc true, :private true}
  parse-seq-exprs (juxt into-coll remove-into-coll))

(defn join-seqs
  "Lazily concatenates a sequence-of-sequences into a flat sequence."
  {:no-doc true}
  [seq-of-seqs]
  (lazy-seq
   (when-let [s (seq seq-of-seqs)]
     (concat (first s) (join-seqs (rest s))))))


#?(:clj
   (s/fdef for
     :args (s/cat :seq-exprs ::seq-exprs, :expr any?)))

(defmacro for
  "Like core/for, but adds :into as a supported modifier.

  :into specifies a collection which each element from the sequence
  will be added to using conj. It makes for eager instead of lazy
  and can only be specified once.

  A drop-in replacement for core/for."
  {:style/indent 1, :added "1.0.1"}
  [seq-exprs expr]
  (let [[coll seq-ex] (parse-seq-exprs seq-exprs)]
    (if coll
      `(into ~coll (clojure.core/for ~seq-ex ~expr))
      `(clojure.core/for ~seq-ex ~expr))))


#?(:clj
   (s/fdef forcat
     :args (s/cat :seq-exprs ::seq-exprs, :expr any?)))

(defmacro forcat
  "A for variant that presumes the body evaluates to a seqable thing.
  Returns a lazy sequence of all elements in each body.

  Like comfy/for, accepts the :into modifier. (Which will make it non-lazy.)"
  {:style/indent 1, :added "0.1.2"}
  [seq-exprs expr]
  (let [[coll seq-ex] (parse-seq-exprs seq-exprs)]
    (if coll
      `(into ~coll cat (clojure.core/for ~seq-ex ~expr))
      `(join-seqs (clojure.core/for ~seq-ex ~expr)))))


#?(:clj
   (s/fdef forv
     :args (s/cat :seq-exprs ::seq-exprs, :body any?)))

(defmacro forv
  "DEPRECATED: Use (comfy/for [,,, :into []] ,,,) instead.

  Like for, but returns a vector. Not lazy."
  {:style/indent 1, :added "0.1.0", :deprecated "1.0.1"}
  [seq-exprs body-expr]
  `(vec (for ~seq-exprs ~body-expr)))


#?(:clj
   (s/fdef for-map
     :args (s/cat :seq-exprs ::seq-exprs, :key-expr any?, :val-expr any?)))

(defmacro for-map
  "DEPRECATED: Use (comfy/for [,,, :into {}] [k-expr v-expr]) instead.

  Like for, but takes a key and value expression and returns a map.
  Multiple equal keys are treated as if by repeated assoc (last one wins).
  Not lazy."
  {:style/indent 1, :added "0.1.0", :deprecated "1.0.1"}
  [seq-exprs key-expr val-expr]
  `(into {} (for ~seq-exprs [~key-expr ~val-expr])))

#?(:clj
   (s/fdef forcatv
     :args (s/cat :seq-exprs ::seq-exprs, :body-expr any?)))

(defmacro forcatv
  "DEPRECATED: Use (comfy/forcat [,,, :into []] ,,,) instead.

  Like for, but presumes that the body-expr evaluates to a seqable thing,
  and returns a vector of every element from each body. Not lazy."
  {:style/indent 1, :added "0.2.0", :deprecated "1.0.1"}
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


(s/fdef run!
  :args (s/cat :proc ifn? :colls (s/+ seqable?))
  :ret nil?)

(defn run!
  "Runs the supplied procedure, for purposes of side
  effects, on successive items in the collection.

  Returns nil. Not lazy.

  When given multiple collections,
  proc will be called with number-of-colls arguments.
  Will go on for the length of the shortest collection.

  A drop-in replacement for core/run!."
  {:added "0.1.1"}
  ([proc coll]
   (clojure.core/run! proc coll))
  ([proc coll & colls]
   (dorun (apply map proc (cons coll colls)))))


(s/fdef group-by
  :args (s/cat :f ifn?
               :xform (s/? ifn?)
               :rf (s/alt :rf (s/cat :rf ifn?
                                     :init (s/? any?))
                          :no-rf (s/cat))
               :coll seqable?)
  :ret map?)

(defn group-by
  "Returns a map of the elements of coll keyed by the result of
  f on each element. The value at each key will be a vector of the
  corresponding elements, in the order they appeared in coll.

  When given a transducer, the resulting reducing function will be called on
  the vector and each element as it's added.
  For each different key, a new instance of the reducing function will be created,
  with its own state, if any.

  Eagerly evaluates all of coll, even if all results are reduced.

  A drop-in replacement for core/group-by."
  {:added "0.1.1"}
  ([f coll] (clojure.core/group-by f coll))
  ([f xform coll]
   (group-by f xform conj coll))
  ([f xform rf coll]
   (group-by f xform rf (rf) coll))
  ([f xform rf init coll]
   ;; rf => the untransduced reducing function
   ;; rf* => the transduced rf (for each key)
   ;; res => result (for each key)
   ;; rfs => reducing functions
   (let [[acc rfs] (reduce (fn [[acc rfs] x]
                             (let [k (f x)
                                   res (get acc k init)

                                   ;; These conditionals are for perf reasons,
                                   ;; to not do more work than necessary.
                                   rf-maybe (get rfs k)
                                   existing-rf? (boolean rf-maybe)
                                   rf* (if existing-rf? rf-maybe (xform rf))
                                   rfs (if existing-rf? rfs (assoc rfs k rf*))]

                               (if (reduced? res)
                                 [acc rfs]
                                 [(assoc acc k (rf* res x)) rfs])))
                           [{} {}], coll)]
     (into {}
           (map (fn [[k res]]
                  [k ((rfs k) (unreduced res))]))
           acc))))


(s/fdef frequencies-by
  :args (s/cat :f ifn? :coll seqable?)
  :ret (s/map-of any? pos-int?))

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

;; The reduce versions require the caller to supply an init value for multiple reasons.
;; * Using the first value from form would complicate the code significantly,
;;   be different between pre and post, and is not really very useful as the types are heterogenous.
;; * Using (rf) is not very useful as in most cases you would write an inline anon fn.
;; * I can always add this later if I feel like it,
;;   but I couldn't remove it or change it without breaking code.
;;   (The "kick the can down the road" methodology of software development.)

;; Can probably be optimized some, the current version is the simplest thing that works.
(defn reduce*
  "Version of reduce that, if reduced, returns the value still wrapped in reduced.
  Needed to pass a reduced value upwards through nested collections
  in pre- and postwalk reduce/transduce."
  {:no-doc true}
  [rf init coll]
  (if (empty? coll)
    init
    (let [acc (rf init (first coll))]
      (if (reduced? acc)
        acc
        (recur rf acc (rest coll))))))


(s/fdef prewalk-reduce
  :args (s/cat :rf ifn?, :init any?, :form any?))

(defn prewalk-reduce
  "Prewalk reduce.
  Performs a depth-first, pre-order traversal of form, calling rf on
  init and each sub-form. Unlike reduce, an init value is required."
  {:added "1.0.0"}
  [rf init form]
  (letfn [(step [acc x]
            (if (coll? x)
              (let [acc' (rf acc x)]
                (if (reduced? acc')
                  acc'
                  (reduce* step acc' x)))
              (rf acc x)))]
    (unreduced (step init form))))


(s/fdef postwalk-reduce
  :args (s/cat :rf ifn?, :init any?, :form any?))

(defn postwalk-reduce
  "Postwalk reduce.
  Performs a depth-first, post-order traversal of form calling rf on
  init and each sub-form. Unlike reduce, an init value is required."
  {:added "1.0.0"}
  [rf init form]
  (letfn [(step [acc x]
            (if (coll? x)
              (let [acc' (reduce* step acc x)]
                (if (reduced? acc')
                  acc'
                  (rf acc' x)))
              (rf acc x)))]
     (unreduced (step init form))))


(s/fdef prewalk-transduce
  :args (s/cat :xform ifn?, :rf ifn?, :init (s/? any?), :form any?))

(defn prewalk-transduce
  "Prewalk transduce.
  Traverses form depth-first, pre-order, behaving otherwise like transduce."
  {:added "1.0.0"}
  ([xform rf form]
   (prewalk-transduce xform rf (rf) form))
  ([xform rf init form]
   (let [f (xform rf)
         res (prewalk-reduce f init form)]
     (f res))))


(s/fdef postwalk-transduce
  :args (s/cat :xform ifn?, :rf ifn?, :init (s/? any?), :form any?))

(defn postwalk-transduce
  "Postwalk transduce.
  Traverses form depth-first, post-order, behaving otherwise like transduce."
  {:added "1.0.0"}
  ([xform rf form]
   (postwalk-transduce xform rf (rf) form))
  ([xform rf init form]
   (let [f (xform rf)
         res (postwalk-reduce f init form)]
     (f res))))


#?(:clj
   (s/fdef syms-in-binding
     :args (s/cat :b :clojure.core.specs.alpha/binding-form)
     :ret (s/coll-of simple-symbol?)))

(defn syms-in-binding
  "Returns the symbols (and keywords that act as symbols) in a binding form."
  {:no-doc true
   :added "0.2.3"}
  [b]
  (letfn [(simple-symbol [sym]
            (with-meta (symbol (name sym)) (meta sym)))]
    (prewalk-reduce (fn [acc x]
                      (cond (symbol? x)
                            (conj acc (simple-symbol x))

                            ;;; Special-case:
                            ;;   Keywords act like symbols in {:keys [:foo :bar/baz]}
                            (and #?(:clj (map-entry? x)
                                    :cljs (and (vector? x) (two? (count x))))
                                 (= :keys (first x)))
                            (into acc
                                  (comp (filter keyword?) (map simple-symbol))
                                  (second x))

                            :else acc))
                    [], b)))


#?(:clj
   (s/fdef defs
     :args (s/cat :binding :clojure.core.specs.alpha/binding-form, :body any?)
     :ret vector?))

(defmacro defs
  "defs(tructure)
  Like def, but can take a binding form instead of a symbol to
  destructure the results of the body, creating vars instead of locals.
  Returns a vector of the vars defined.
  Any metadata added to a symbol becomes metadata on the var."
  {:added "0.2.3"
   :style/indent 1}
  [binding body]
  `(let [~binding ~body]
     ~(for [sym (syms-in-binding binding), :into []]
        `(def ~sym ~sym))))


(s/fdef append
  :args (s/cat :coll seqable? :xs (s/* any?))
  :ret seq?)

(defn append
  "Returns a lazy seq of the elements of coll followed by xs."
  {:added "1.0.0"}
  [coll & xs]
  (concat coll xs))


(s/fdef append-some
  :args (s/cat :coll seqable? :xs (s/* any?))
  :ret seq?)

(defn append-some
  "Returns a lazy seq of the elements of coll followed by the non-nil xs."
  {:added "1.0.0"}
  [coll & xs]
  (concat coll (remove nil? xs)))


(s/fdef take-while-distinct-by
  :args (s/cat :f ifn? :coll (s/? seqable?))
  :ret (s/or :transducer ifn?, :coll seq?))

(defn take-while-distinct-by
  "Takes items until the f of an item repeats (not inclusive)."
  {:added "1.0.0"}
  ([f]
   (fn [rf]
     (let [seen (volatile! #{})]
       (fn
         ([] (rf))
         ([res] (rf res))
         ([res x]
          (let [y (f x)]
            (if (contains? @seen y)
              (ensure-reduced res)
              (do (vswap! seen conj y)
                  (rf res x)))))))))
  ([f coll]
   (let [seen (volatile! #{})]
     (letfn [(step [coll]
               (lazy-seq
                (when-let [[x & xs] (seq coll)]
                  (let [y (f x)]
                    (when-not (contains? @seen y)
                      (vswap! seen conj y)
                      (cons x (step xs)))))))]
       (step coll)))))


(s/fdef take-while-distinct
  :args (s/cat :coll (s/? seqable?))
  :ret (s/or :transducer ifn?, :coll (s/and seq? (partial apply distinct?))))

(def ^{:arglists '([] [coll])
       :added "1.0.0"
       :doc "Take items until an item repeats (not inclusive)."}
  take-while-distinct (partial take-while-distinct-by identity))


(defn sentinel
  "Returns something that is only equal to itself."
  {:added "1.0.3"}
  [] #?(:clj (Object.), :cljs (js-obj)))


(defn sentinels
  "Returns an infinite sequence of distinct sentinels."
  {:added "1.0.3"}
  [] (repeatedly sentinel))


(s/fdef map-all
  :args (s/cat :f ifn?, :colls (s/+ seqable?))
  :ret seq?)

(defn map-all
  "ike map, but keeps going until the end of the longest collection,
  substituting nil when a collection is exhausted."
  {:added "1.0.0"
   :arglists '([f coll & colls])}
  [f & colls]
  (let [[none stop] (sentinels)]
    (->> (map #(concat % (repeat none)) colls)
         (apply map (fn [& args]
                      (if (every? #{none} args)
                        stop
                        (apply f (replace {none nil} args)))))
         (take-while (complement #{stop})))))
