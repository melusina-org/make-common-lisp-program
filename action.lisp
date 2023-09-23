;;;; action.lisp — Make Common Lisp Program

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

(defpackage #:org.melusina.github-action.make-common-lisp-program
  (:use #:common-lisp)
  (:export
   #:configure
   ))

(in-package #:org.melusina.github-action.make-common-lisp-program)

(defun getenv (variable-name &optional default-value)
  (or (uiop:getenv variable-name)
      default-value
      (flet ((read-value ()
	       (format *trace-output* "~&Enter the value to instead of ~A: " variable-name)
	       (list (read-line))))
	(restart-case (error "The environment variable ~A is not defined." variable-name)
	  (use-value (value)
	    :report "Specify a value to  use instead of the missing environment variable."
	    :interactive read-value
	    value)))))

(defparameter *implementation*
  (getenv "LISP_IMPLEMENTATION" "sbcl"))

(defparameter *system*
  (getenv "LISP_SYSTEM"))

(defparameter *entrypoint*
  (getenv "LISP_ENTRYPOINT"))

(defparameter *program*
  (flet ((make-program-name ()
	   (concatenate
	    'string
	    *system* "-" *entrypoint*
	    (when (uiop:os-windows-p)
	      ".exe"))))
    (getenv "LISP_PROGRAM" (make-program-name))))

(defun write-detail (&key name key value)
  "Write detail NAME with VALUE.
Additionally, when running on GitHub Actions, the key is written
to job output."
  (format t "~&~A: ~A~%" name value)
  (when (uiop:getenv "GITHUB_OUTPUT")
    (with-open-file (output (uiop:getenv "GITHUB_OUTPUT")
			    :direction :output
			    :if-exists :append :if-does-not-exist :create)
      (format output "~&~A=~A~%" key value))))

(defun write-make-program-details ()
  "Write details about the current Common Lisp Implementation."
  (loop :for detail
	:in
	(list
	 (list
	  :name "Implementation"
	  :key "implementation"
	  :value *implementation*)
	 (list
	  :name "System"
	  :key "system"
	  :value *system*)
	 (list
	  :name "Entrypoint"
	  :key "entrypoint"
	  :value *entrypoint*)
	 (list
	  :name "Program"
	  :key "program"
	  :value *program*))
	:do (apply #'write-detail detail)))

(defun configure ()
  "Perform configuration step."
  (write-make-program-details))

;;;; End of file `action.lisp'
