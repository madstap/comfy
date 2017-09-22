(ns ^:no-doc madstap.comfy.alpha
  (:require
   [madstap.comfy :as comfy]
   [clojure.spec.alpha :as s]
   [clojure.test :refer [deftest is are testing]]
   #?(:clj [clojure.core.specs.alpha])))

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
