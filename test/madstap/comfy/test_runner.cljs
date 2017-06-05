(ns madstap.comfy.test-runner
  (:require
   [doo.runner :refer-macros [doo-tests]]
   [madstap.comfy.core-test]))

(doo-tests 'madstap.comfy.core-test)
