(ns madstap.comfy.core-test
  (:require
   [madstap.comfy.core :as comfy]
   [clojure.test :refer [deftest is are testing]]))

(deftest one?-and-two?-tests
  (is (comfy/one? 1))
  (is (comfy/one? 1.0))
  (is (false? (comfy/one? 0)))
  (is (comfy/two? 2))
  (is (comfy/two? 2.0))
  (is (false? (comfy/two? 1))))

(deftest conj-some-test
  (is (= [] (comfy/conj-some)))
  (is (nil? (comfy/conj-some nil)))
  (is (= () (comfy/conj-some ())))
  (is (= '(:foo) (comfy/conj-some nil :foo)))
  (is (= [1 2 3] (comfy/conj-some [] 1 nil 2 nil 3 nil))))

(deftest fn->-test
  (is (= :foo ((comfy/fn->) :foo)))
  (is (= :foobar ((comfy/fn-> (str "bar") keyword) "foo"))))

(deftest fn->>-test
  (is (= :foo ((comfy/fn->>) :foo)))
  (is (= '(1 2 3) ((comfy/fn->> (map inc)) (range 3)))))

(deftest forv-test
  (is (= [1 2 3] (comfy/forv [i (range 3)] (inc i)))))

(deftest for-map-test
  (is (= {0 "0" 1 "1"}
         (comfy/for-map [i (range 2)]
           i (str i)))))

(deftest deep-merge-test
  (is (nil? (comfy/deep-merge nil nil)))
  (is (= {}
         (comfy/deep-merge nil {})
         (comfy/deep-merge {} nil)))
  (is (= {:foo 1}
         (comfy/deep-merge {:foo 1} nil)))
  (is (= {:foo {:bar 1}}
         (comfy/deep-merge {:foo {:bar 1}} {:foo nil})))
  (is (= {:foo {:bar 1 :baz 2}}
         (comfy/deep-merge {:foo {:bar 1}} {:foo {:baz 2}}))))

(defn throw-fn [& args]
  (throw (ex-info "This fn shouldn't be called, but it was." {:args args})))

(deftest deep-merge-with-test
  (is (nil? (comfy/deep-merge-with throw-fn nil nil)))
  (is (= {} (comfy/deep-merge-with throw-fn {} nil)))
  (is (= {:foo 1} (comfy/deep-merge-with throw-fn {:foo 1} nil)))
  (is (= {:foo 2}
         (comfy/deep-merge-with + {:foo 1} {:foo 1})))
  (is (= {:foo {:bar 2 :baz 2}}
         (comfy/deep-merge-with + {:foo {:bar 1}} {:foo {:bar 1 :baz 2}})))
  (is (= {:foo {:bar 2 :baz 2}}
         (comfy/deep-merge-with + {:foo {:bar 1}} {:foo {:bar 1 :baz 2}} {:foo nil}))))

(deftest flip-test
  (is (= {:foo 1} ((comfy/flip assoc :foo 1) {}))))

(deftest assoc-in-some-test
  (is (= {:foo {}} (comfy/assoc-in-some {:foo {}} [:foo :bar] nil)))
  (is (= {:foo {:bar 42}} (comfy/assoc-in-some {} [:foo :bar] 42))))

(deftest keep-test
  (is (= [:foo :bar]
         (comfy/keep (fn [x y z]
                       (when (= x y z) x))
                     [1 :foo 3 :bar :quux]
                     [2 :foo 2 :bar :quux]
                     [5 :foo 3 :bar])))
  (is (= [0 1] (sequence (comfy/keep (fn [x y] x)) [0 nil 1] (range 3)))))

(deftest run!-test
  (is (= [[0 0] [1 1] [2 2]]
         (let [a (atom [])]
           (comfy/run! (fn [x y]
                         (swap! a conj [x y]))
                       (range)
                       (range 3))
           @a)))
  (is (nil? (comfy/run! (fn [x y] x) (range 3) (range 3)))))

(deftest group-by-test
  (is (= {false [[0 2] [4]], true [[1 3] [5]]}
         (comfy/group-by odd? (comp (take 3) (partition-all 2)) (range 100)))))

(deftest frequencies-by-test
  (is (= {} (comfy/frequencies-by throw-fn [])))
  (is (= {false 5, true 4} (comfy/frequencies-by odd? (range 9)))))

(deftest forcat-test
  (is (= '(0 0 1 0 1 2) (comfy/forcat [i [1 2 3]] (range i)))))

(deftest forcatv-test
  (is (= [0 0 1 0 1 2] (comfy/forcatv [i [1 2 3]] (range i)))))
