(ns ^:no-doc madstap.comfy.alpha
  #?(:cljs (:require-macros [madstap.comfy.alpha]))
  (:require
   [madstap.comfy :as comfy]
   [clojure.spec.alpha :as s]
   [clojure.test :refer [deftest is are testing]]))

(s/fdef remove-first
  :args (s/cat :pred ifn?, :coll (s/? seqable?)))

(defn remove-first
  "Removes the first element of coll where (pred elem).
  Returns a transducer when called without a collection."
  ([pred]
   (fn [rf]
     (let [still-looking? (volatile! true)]
       (fn
         ([] (rf))
         ([res] (rf res))
         ([res x]
          (if (and @still-looking? (pred x))
            (do (vswap! still-looking? not)
                res)
            (rf res x)))))))
  ([pred coll]
   (sequence (remove-first pred) coll)))

(deftest remove-first-test
  (testing "sequence"
    (is (= '(0 1 3 4) (remove-first #{2 3} (range 5)))))
  (testing "is lazy"
    (is (= '(0 1 3) (take 3 (remove-first #{2 3} (range))))))
  (testing "transducer"
    (is (= [0 1 3 4] (transduce (remove-first #{2 3}) conj (range 5))))))

(comment

  (require '[clojure.spec.test.alpha :refer [instrument]])

  (do (instrument) (clojure.test/run-tests))

  )

;; Couldn't get this to work in clojurescript
;; Instead of .hasRoot cljs uses exists?, but it all becomes confusing
;; because of the fact that cljs macros execute in clj.
;; Maybe macrovich would help with this?
;; Also, what about bootstrapped cljs?
#?(:clj
   (defmacro defsonce
     "defs(tructure) once
     Works like defs iff any one of the vars does not yet have a root value.
     If all of them has a root value, body is unevaluated."
     {:added ""
      :style/indent 1}
     [binding body]
     `(let [vs# ~(comfy/forv [sym (comfy/syms-in-binding binding)]
                   `(def ~sym))]
        (when-not (every? #(.hasRoot %) vs#)
          (comfy/defs ~binding ~body)))))

(s/fdef pred-fn
  :args (s/cat :pred ifn? :f ifn? :g (s/? ifn?))
  :ret fn?)

;; TODO: Needs a better name.
(defn pred-fn
  "Returns a function that applies f to it's argument
  if (pred arg), else applies g. g defaults to identity."
  ([pred f]
   (pred-fn pred f identity))
  ([pred f g]
   (fn [x]
     (if (pred x) (f x) (g x)))))

(defn namespace-kw [ns k]
  (keyword (name ns) (name k)))

(defn map-keys [f m]
  (zipmap (map f (keys m)) (vals m)))

(defn namespace-map [ns m]
  (map-keys (pred-fn simple-keyword? (partial namespace-kw ns)) m))

(s/fdef sym-map
  :args (s/cat :ns (s/? string?)
               :m (s/* (s/alt :sym (s/& (s/cat :sym symbol?) (s/conformer #(:sym %)))
                              :pair (s/cat :k (complement symbol?) :v any?)))))

;; Needs a better name and some hammock time.
(defmacro sym-map
  "Convenience macro for creating maps with keys that are the same as local names.

  (let [foo 42 bar 10]
    (sym-map foo bar)) ;=> {:foo 42 :bar 10}

  If the first argument is a string, it will be used as the namespace
  for any simple keyword in the map.

  (let [foo 42 bar 10]
    (sym-map \"quux\" foo bar)) ;=> #:quux{:foo 42 :bar 10}

  Also accepts pairs of keys and values. The keys can be anything but compile
  time symbols.

  (let [foo 42 bar 10]
    (sym-map foo bar :baz (+ foo bar))) ;=> {:foo 42 :bar 10 :baz 52}

  Will ignore namespaces in symbols."
  [& args]
  (let [{:keys [ns m]} (s/conform (:args (s/get-spec `sym-map)) args)
        {:keys [sym pair]} (comfy/group-by key (map val) m)
        m (gensym "m")]
    `(let [~m (hash-map ~@(mapcat (juxt (comp keyword name) identity) sym)
                        ~@(mapcat (juxt :k :v) pair))]
       ~(if ns `(namespace-map ~ns ~m) m))))

(s/fdef insert-at
  :args (s/and (s/cat :v vector?, :idx nat-int?, :x any?)
               (fn [{:keys [idx v]}]
                 (<= idx (count v))))
  :ret vector?)

(defn insert-at [v idx x]
  (vec (concat (subvec v 0 idx) [x] (subvec v idx))))

#?(:clj
   (s/fdef compile-time-slurp
     :args (s/cat :path string?)))

#?(:clj
   (defmacro compile-time-slurp
     "Slurp something at compile-time. It will be as if the contents at path
     were a literal string in the source code.
     Not available in self hosted clojurescript."
     [path]
     (slurp path)))
