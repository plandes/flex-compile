;;; package --- Summary
;;; Commentary:
;;
;; Unit tests of flex-compile.el
;;
;;; Code:

(require 'ert)
(require 'dash)
(require 'flex-compile)
(require 'flex-compile-make)

(ert-deftest test-manager-instance ()
  "Test successful evaluation of flex-compile"
  (let ((this flex-compile-manage-inst))
    (should (eq t (eieio-object-p this)))
    (should (eq 'flex-compile-manager (eieio-object-class this)))))

(ert-deftest test-compiler-registration ()
  "Test registration of compilers."
  (should (equal '("choice-program" "clojure" "comint" "command" "disable"
		   "ess" "make" "org-export" "python" "script" "xml-validate")
		 (->> (slot-value flex-compile-manage-inst 'entries)
		      (-map 'config-entry-name)
		      (funcall #'(lambda (elt) (sort elt 'string<)))))))

(provide 'flex-compile-test)

;;; flex-compile-test ends here
