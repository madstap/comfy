(ns madstap.comfy.walk
  (:require
   [clojure.spec.alpha :as s]))

(s/fdef pre-reduce
  :args (s/cat :rf ifn? :init (s/? any?) :form any?))

(defn pre-reduce
  "Prewalk reduce.
  Performs a depth-first, pre-order traversal of form calling rf on
  init and each sub-form.
  Will use (rf) as init walue it not supplied one. (Like transduce, unlike reduce.)"
  ([rf form]
   (pre-reduce rf (rf) form))
  ([rf init form]
   (letfn [(step [acc x]
             (if (seqable? x)
               (reduce step (rf acc x) x)
               (rf acc x)))]
     (step init form))))


(s/fdef pre-transduce
  :args (s/cat :xform ifn?, :rf ifn?, :init any?, :form any?))

(defn pre-transduce
  "Prewalk transduce.
  Traverses form in depth-first, pre-order, behaving otherwise like transduce."
  ([xform rf form]
   (pre-transduce xform rf (rf) form))
  ([xform rf init form]
   (let [f (xform rf)
         res (pre-reduce f init form)]
     (f res))))


(s/fdef post-reduce
  :args (s/cat :rf ifn? :init (s/? any?) :form any?))

(defn post-reduce
  "Prewalk reduce.
  Performs a depth-first, pre-order traversal of form calling rf on
  init and each sub-form.
  Will use (rf) as init walue it not supplied one. (Like transduce, unlike reduce.)"
  ([rf form]
   (post-reduce rf (rf) form))
  ([rf init form]
   (letfn [(step [acc x]
             (if (seqable? x)
               (rf (reduce step acc x) x)
               (rf acc x)))]
     (step init form))))


(s/fdef post-transduce
  :args (s/cat :xform ifn?, :rf ifn?, :init any?, :form any?))

(defn post-transduce
  "Postwalk transduce.
  Traverses form in depth-first, post-order, behaving otherwise like transduce."
  ([xform rf form]
   (post-transduce xform rf (rf) form))
  ([xform rf init form]
   (let [f (xform rf)
         res (post-reduce f init form)]
     (f res))))
