;;; package --- Summary
;;; Commentary:
;;
;; Unit tests of buffer-manager-test.el
;;
;;; Code:

(require 'ert)
(require 'dash)
(require 'compile-flex)

(ert-deftest test-manager-instance ()
  "Test successful evaluation of compile-flex"
  (let ((this the-flex-compile-manager))
    (should (eq t (eieio-object-p this)))
    (should (eq 'flex-compile-manager (eieio-object-class this)))))
