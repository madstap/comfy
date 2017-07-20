(ns madstap.comfy.test-runner
  (:require
   [doo.runner :refer-macros [doo-tests]]
   [madstap.comfy.core-test]
   [madstap.comfy.walk-test]))

(doo-tests 'madstap.comfy.core-test 'madstap.comfy.walk-test)
