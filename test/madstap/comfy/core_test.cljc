(ns madstap.comfy.core-test
  (:require
   [madstap.comfy.core :as sut]
   #?(:clj [clojure.test :refer [deftest is are testing]]
      ;; Is this needed?
      :cljs [cljs.test :refer-macros [deftest is are testing]])))

(deftest one?-and-two?-tests
  (is (sut/one? 1))
  (is (sut/one? 1.0))
  (is (false? (sut/one? 0)))
  (is (sut/two? 2))
  (is (sut/two? 2.0))
  (is (false? (sut/two? 1))))

(deftest conj-some-test
  (is (= [] (sut/conj-some)))
  (is (nil? (sut/conj-some nil)))
  (is (= () (sut/conj-some ())))
  (is (= '(:foo) (sut/conj-some nil :foo)))
  (is (= [1 2 3] (sut/conj-some [] 1 nil 2 nil 3 nil))))

(deftest fn->-test
  (is (= :foo ((sut/fn->) :foo)))
  (is (= :foobar ((sut/fn-> (str "bar") keyword) "foo"))))

(deftest fn->>-test
  (is (= :foo ((sut/fn->>) :foo)))
  (is (= '(1 2 3) ((sut/fn->> (map inc)) (range 3)))))

(deftest forv-test
  (is (= [1 2 3] (sut/forv [i (range 3)] (inc i)))))

(deftest for-map-test
  (is (= {0 "0" 1 "1"}
         (sut/for-map [i (range 2)]
           i (str i)))))

(deftest deep-merge-test
  (is (nil? (sut/deep-merge nil nil)))
  (is (= {}
         (sut/deep-merge nil {})
         (sut/deep-merge {} nil)))
  (is (= {:foo 1}
         (sut/deep-merge {:foo 1} nil)))
  (is (= {:foo {:bar 1}}
         (sut/deep-merge {:foo {:bar 1}} {:foo nil})))
  (is (= {:foo {:bar 1 :baz 2}}
         (sut/deep-merge {:foo {:bar 1}} {:foo {:baz 2}}))))

(defn throw-fn [& args]
  (throw (ex-info "This fn shouldn't be called, but it was." {:args args})))

(deftest deep-merge-with-test
  (is (nil? (sut/deep-merge-with throw-fn nil nil)))
  (is (= {} (sut/deep-merge-with throw-fn {} nil)))
  (is (= {:foo 1} (sut/deep-merge-with throw-fn {:foo 1} nil)))
  (is (= {:foo 2}
         (sut/deep-merge-with + {:foo 1} {:foo 1})))
  (is (= {:foo {:bar 2 :baz 2}}
         (sut/deep-merge-with + {:foo {:bar 1}} {:foo {:bar 1 :baz 2}})))
  (is (= {:foo {:bar 2 :baz 2}}
         (sut/deep-merge-with + {:foo {:bar 1}} {:foo {:bar 1 :baz 2}} {:foo nil}))))
