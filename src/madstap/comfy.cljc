(ns madstap.comfy
  "A small collection of functions and macros that (mostly) wouldn't
  be out of place in clojure.core."
  (:refer-clojure :exclude [keep run! group-by for])
  #?(:cljs (:require-macros [madstap.comfy :refer [for forv]]))
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
           (if (every? nilable-map? [v1 v2])
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
           (if (every? nilable-map? [v1 v2])
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

;;;;;;;;;;;;;;;;;;;;;
;; Variations on for

;; This spec, and the specs that depend on it need the conditional
;; because :clojure.core.specs.alpha is not yet ported to cljs.
#?(:clj
   (s/def ::seq-exprs
     (s/and vector?
            (s/* (s/alt :binding :clojure.core.specs.alpha/binding
                        :let       (s/cat :k #{:let}
                                          :bindings :clojure.core.specs.alpha/bindings)
                        :when-let  (s/cat :k #{:when-let}
                                          :bindings :clojure.core.specs.alpha/bindings)
                        :while-let (s/cat :k #{:while-let}
                                          :bindings :clojure.core.specs.alpha/bindings)
                        :do        (s/cat :k #{:do}        :expr any?)
                        :when      (s/cat :k #{:when}      :expr any?)
                        :when-not  (s/cat :k #{:when-not}  :expr any?)
                        :while     (s/cat :k #{:while}     :expr any?)
                        :while-not (s/cat :k #{:while-not} :expr any?)
                        :into      (s/cat :k #{:into}      :coll any?)))
            #(= :binding (ffirst %))
            #(>= 1 (count (keep (comp #{:into} key) %))))))

(defn join-seqs
  "Lazily concatenates a sequence-of-sequences into a flat sequence."
  {:no-doc true}
  [seq-of-seqs]
  (lazy-seq
   (when-let [s (seq seq-of-seqs)]
     (concat (first s) (join-seqs (rest s))))))

(defn- into-coll
  {:no-doc true}
  [seq-exprs]
  (->> seq-exprs (partition 2) (filter #(= :into (first %))) first second))

(defn- expand-*-let
  {:no-doc true}
  [kw bindings]
  (mapcat (fn [[binding expr]]
            `[:let [temp# ~expr] ~kw temp# :let [~binding temp#]])
          (partition 2 bindings)))

(defn- parse-expr
  {:no-doc true}
  [[k v :as seq-expr]]
  (case k
    :into nil
    :do [:let [(gensym) v]]
    :when-not  [:when  `(not ~v)]
    :while-not [:while `(not ~v)]
    :when-let (expand-*-let :when v)
    :while-let (expand-*-let :while v)
    seq-expr))

(defn- parse-exprs
  {:no-doc true}
  [exprs]
  (into [] (mapcat parse-expr) (partition 2 exprs)))

(def ^:no-doc ^:private parse-seq-exprs
  (juxt into-coll parse-exprs))

#?(:clj
   (s/fdef for
     :args (s/cat :seq-exprs ::seq-exprs, :expr any?)))

(defmacro for
  "Like core/for, but adds :do, :when-let and :while-let modifiers.

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
  Returns a lazy sequence of all elements in each body."
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

(s/fdef run!
  :args (s/cat :proc ifn? :colls (s/+ seqable?))
  :ret nil?)

(defn run!
  "Runs the supplied procedure, for purposes of side
  effects, on successive items in the collection.

  Returns nil. Not lazy.

  Arity one returns a mapping transducer that runs proc
  on each element, while having no other effect
  (ie, returns the element unchanged).

  When given multiple collections,
  proc will be called with number-of-colls arguments.
  Will go on for the length of the shortest collection.

  A drop-in replacement for core/run!."
  {:added "0.1.1"}
  ([proc]
   (map #(doto % proc)))
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
  ([f coll]
   (clojure.core/group-by f coll)
   #_(group-by f identity coll))
  ([f xform coll]
   (group-by f xform conj coll))
  ([f xform rf coll]
   (group-by f xform rf (rf) coll))
  ([f xform rf init coll]
   ;; rf => the untransduced reducing function
   ;; rf* => the transduced rf (for each key)
   ;; res => result (for each key)
   ;; rfs => reducing functions
   ;; acc => the map that's built up and returned at the end
   (let [[acc rfs] (reduce
                    (fn [[acc rfs] x]
                      (let [k (f x)
                            res (get acc k init)

                            ;; These conditionals are for perf reasons.
                            ;; I could just do
                            ;; (get rfs k (xform rf)) and (assoc rfs k rf*)
                            ;; but that's apparently way slower
                            ;; (based on very limited benchmarking at the repl)
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

;; Needed to pass a reduced value upwards through nested collections.
(defn preserving-reduce
  "Version of reduce that, if reduced, returns the value still wrapped in reduced."
  {:no-doc true}
  [rf init coll]
  (reduce (#'clojure.core/preserving-reduced rf) init coll))

(s/fdef prewalk-reduce
  :args (s/cat :rf ifn?, :init (s/? any?), :form any?))

(defn prewalk-reduce
  "Prewalk reduce.
  Performs a depth-first, pre-order traversal of form, calling rf on
  init and each sub-form. If init is not supplied (rf) will be used."
  {:added "1.0.0"}
  ([rf form]
   (prewalk-reduce rf (rf) form))
  ([rf init form]
   (letfn [(step [acc x]
             (if (coll? x)
               (let [acc' (rf acc x)]
                 (if (reduced? acc')
                   acc'
                   (preserving-reduce step acc' x)))
               (rf acc x)))]
     (unreduced (step init form)))))

(s/fdef postwalk-reduce
  :args (s/cat :rf ifn?, :init (s/? any?), :form any?))

(defn postwalk-reduce
  "Postwalk reduce.
  Performs a depth-first, post-order traversal of form calling rf on
  init and each sub-form. If init is not supplied (rf) will be used."
  {:added "1.0.0"}
  ([rf form]
   (postwalk-reduce rf (rf) form))
  ([rf init form]
   (letfn [(step [acc x]
             (if (coll? x)
               (let [acc' (preserving-reduce step acc x)]
                 (if (reduced? acc')
                   acc'
                   (rf acc' x)))
               (rf acc x)))]
     (unreduced (step init form)))))

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
            (with-meta (symbol (name sym)) (merge (meta b) (meta sym))))
          (map-entry?* [x]
            #?(:clj (map-entry? x), :cljs (and (vector? x) (two? (count x)))))]
    (prewalk-reduce
     (fn [acc x]
       (cond (and (symbol? x) (not= '& x))
             (conj acc (simple-symbol x))

             ;; Special-case: Keywords act like symbols in {:keys [:foo :bar/baz]}
             (and (map-entry?* x) (= :keys (first x)))
             (into acc (comp (filter keyword?)
                             (map simple-symbol)) (second x))

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
     ~(forv [sym (syms-in-binding binding)]
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
   (letfn [(step [seen coll]
             (lazy-seq
              (when-let [[x & xs] (seq coll)]
                (let [y (f x)]
                  (when-not (contains? seen y)
                    (cons x (step (conj seen y) xs)))))))]
     (step #{} coll))))

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

(s/fdef cond-doto
  :args (s/cat :expr any?, :clauses (s/* (s/cat :test any?, :form any?))))

(defmacro cond-doto
  "Takes an expression and test/form pairs. Evaluates expr and for
  each truthy test evaluates the corresponding form as if in a doto,
  with expr at the front of the arguments. Returns the value of expr."
  {:added "1.0.5"
   :style/indent 1}
  [expr & clauses]
  (let [x (gensym)]
    `(let [~x ~expr]
       ~@(for [[test step] (partition 2 clauses)]
           `(when ~test (doto ~x ~step)))
       ~x)))

(s/fdef invoke
  :args (s/cat :f ifn?, :args (s/* any?)))

(defn invoke
  "Invokes the function f with args, if any."
  [f & args]
  (apply f args))

(s/fdef comp
  :args (s/* (s/nilable ifn?))
  :ret fn?)

(defn comp
  "Like core/comp, but ignores nil values."
  [& fs]
  (apply clojure.core/comp (remove nil? fs)))

(defn transduce-map [m xform]
  (into (empty m) xform m))

(defn map-vals [m f & args]
  (transduce-map m (map (fn [[k v]]
                          [k (apply f v args)]))))

(defn map-keys [m f & args]
  (transduce-map m (map (fn [[k v]]
                          [(apply f k args) v]))))

(defn filter-keys [m pred & args]
  (transduce-map m (filter (fn [[k _]]
                             (apply pred k args)))))

(defn filter-vals [m pred & args]
  (transduce-map m (filter (fn [[_ v]]
                             (apply pred v args)))))
