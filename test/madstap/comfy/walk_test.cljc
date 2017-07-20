(ns madstap.comfy.walk-test
  (:require [madstap.comfy.walk :refer [pre-reduce pre-transduce
                                        post-reduce post-transduce]]
            [clojure.test :refer [deftest testing is are run-all-tests]]
            [clojure.walk :as core.walk]))

;; Alternative implementation using clojure.walk

(defn walk-reduce-factory*
  {:no-doc true}
  [walker]
  (fn walk-reduce
    ([rf coll]
     (walk-reduce rf (rf) coll))
    ([rf init coll]
     (let [*acc (atom [])]
       (walker (fn [x] (swap! *acc conj x) x) coll)
       (reduce rf init @*acc)))))

(defn walk-transduce-factory*
  {:no-doc true}
  [walker]
  (let [walk-reducer (walk-reduce-factory* walker)]
    (fn walk-transduce
      ([xform rf coll]
       (walk-transduce xform rf (rf) coll))
      ([xform rf init coll]
       (let [f (xform rf)
             ret (walk-reducer f init coll)]
         (f ret))))))

(def post-reduce* (walk-reduce-factory* core.walk/postwalk))

(def post-transduce* (walk-transduce-factory* core.walk/postwalk))

(def pre-reduce* (walk-reduce-factory* core.walk/prewalk))

(def pre-transduce* (walk-transduce-factory* core.walk/prewalk))

(deftest example-based-comparison-to-core-walk
  (are [form] (= (post-reduce conj [] form)
                 (post-reduce* conj [] form)
                 (post-transduce identity conj [] form)
                 (post-transduce* identity conj [] form))
    [1 2 3]
    1
    []
    [1 2 [3 4]]
    [1 [2 3 [4] 5 [6]] 7 [8 [[9 10] 11]]]
    [:foo :bar {'(1 [2 3] 4) :bar}]))

(comment

  (run-all-tests #"walk")


  )
