;;;; testsuite.lisp — Testsuite to Make Common Lisp Program

;;;; Melusina Actions (https://github.com/melusina-org/make-common-lisp-program)
;;;; This file is part of Melusina Actions.
;;;;
;;;; Copyright © 2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(require '#:asdf)
(require '#:uiop)
(require '#:org.melusina.confidence)
(require '#:org.melusina.github-action.make-common-lisp-program)

(defpackage #:org.melusina.github-action.make-common-lisp-program/testsuite
  (:use #:common-lisp)
  (:local-nicknames
   (#:confidence #:org.melusina.confidence))
  (:import-from #:org.melusina.confidence
   #:define-testcase
   #:define-assertion
   #:assert-condition
   #:assert=
   #:assert-t
   #:assert-t*
   #:assert-nil
   #:assert-eq
   #:assert-equal
   #:assert-set-equal
   #:assert-string=
   #:assert-type)  
  (:export
   #:unit-tests))

(in-package #:org.melusina.github-action.make-common-lisp-program/testsuite)

(define-testcase unit-tests ()
  (assert-t t))

(define-testcase all-tests ()
  (unit-tests))

;;;; End of file `action.lisp'
