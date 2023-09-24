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
  (:local-nicknames
   (#:actions #:org.melusina.github-actions))
  (:export
   #:configure
   ))

(in-package #:org.melusina.github-action.make-common-lisp-program)

(defun getenv (variable-name)
  (let ((value
	  (uiop:getenv variable-name)))
    (unless (string-equal value ":NOT-SET")
      value)))

(defparameter *implementation*
  (or (uiop:getenv "LISP_IMPLEMENTATION")
      "sbcl"))

(defparameter *system*
  (uiop:getenv "LISP_SYSTEM"))

(defparameter *entrypoint*
  (uiop:getenv "LISP_ENTRYPOINT"))

(defparameter *program*
  (flet ((make-program-name ()
	   (when (and *system* *entrypoint*)
	     (concatenate
	      'string
	      *system* "-" *entrypoint*
	      (when (uiop:os-windows-p)
		".exe")))))
    (or (uiop:getenv "LISP_PROGRAM") (make-program-name))))

(defun write-make-program-configuration ()
  "Write details about the current Common Lisp Implementation."
  (loop :for detail
	:in
	(list
	 (list
	  :key "implementation"
	  :value *implementation*)
	 (list
	  :key "system"
	  :value *system*)
	 (list
	  :key "entrypoint"
	  :value *entrypoint*)
	 (list
	  :key "program"
	  :value *program*))
	:do (apply #'actions:set-output detail)))

(defun configure ()
  "Perform configuration step."
  (write-make-program-configuration))

;;;; End of file `action.lisp'
