(ns madstap.comfy.walk-test
  (:require
   [madstap.comfy :as comfy]
   [clojure.test :refer [deftest testing is are run-all-tests]]
   [clojure.walk :as core.walk]))

;; Alternative implementation using clojure.walk

(defn walk-reduce-factory*
  {:no-doc true}
  [walker]
  (fn walk-reduce [rf init coll]
    (let [*acc (atom [])]
      (walker (fn [x] (swap! *acc conj x) x) coll)
      (reduce rf init @*acc))))

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
  (are [form] (= (comfy/postwalk-reduce conj [] form)
                 (post-reduce* conj [] form)
                 (comfy/postwalk-transduce identity conj [] form)
                 (post-transduce* identity conj [] form))
    [1 2 3]
    1
    []
    #{1 2 3}
    "foo"
    '(foo :foo "foo")
    [1 2 [3 4]]
    [1 [2 3 [4] 5 [6]] 7 [8 [[9 10] 11]]]
    [:foo :bar {'(1 [2 3] 4) :bar}]))

(deftest walkers-respect-reduced-test
  (is (= :foo
         (comfy/prewalk-reduce #(reduced %2) nil :foo)
         (comfy/postwalk-reduce #(reduced %2) nil :foo)))
  (is (= [1 2 3] (comfy/prewalk-reduce #(reduced %2) nil [1 2 3])))
  (is (= 1 (comfy/postwalk-reduce #(reduced %2) nil [1 2 3])))

  (testing "reduced is passed upwards from nested collections"
    (is (= [1 [1]]
           (comfy/postwalk-reduce (fn [acc x]
                                    (if (coll? x)
                                      (reduced (conj acc x))
                                      (conj acc x)))
                                  []
                                  [[[1]] 2 3])))

    (is (= [[[[1]]] [[1]] [1]]
           (comfy/prewalk-reduce
            (fn [acc x]
              ;; The call to first here will throw if the
              ;; reduction fails to respect reduced.
              (if (not (coll? (first x)))
                (reduced (conj acc x))
                (conj acc x)))
            []
            [[[1]]])))))


(comment

  (run-all-tests #"walk")

  )
