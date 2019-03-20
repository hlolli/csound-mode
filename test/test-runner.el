;;; -*- lexical-binding: t -*-
(require 'with-simulated-input)
(require 'cl-lib)
(require 'buttercup)

(describe "`with-simulated-input'"
          (it "should work for basic string input"
              (expect
               (with-simulated-input "hello RET"
                                     (read-string "Enter a string: "))
               :to-equal "hello")))
