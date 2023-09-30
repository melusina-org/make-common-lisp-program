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

(defparameter *implementation*
  (or (uiop:getenv "LISP_IMPLEMENTATION")
      "sbcl"))

(defparameter *system*
  (uiop:getenv "LISP_SYSTEM"))

(defun build-pathname (&optional (system *system*))
  (let ((pathname
	  (slot-value (asdf:find-system system t) 'asdf/system:build-pathname)))
    (flet ((ensure-build-pathname-is-set ()
	     (unless pathname
	       (error "Build system ~A does not define a BUILD-PATHNAME." system)))
	   (finalize-pathname ()
	     (let ((program-pathname
		     (if (uiop:os-windows-p)
			 (concatenate 'string pathname ".exe")
			 pathname)))
	       (merge-pathnames
		program-pathname
		(asdf:system-source-directory system)))))
      (ensure-build-pathname-is-set)
      (finalize-pathname))))

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
	  :key "build-pathname"
	  :value (build-pathname)))
	:do (apply #'actions:set-output detail)))

(defun configure ()
  "Perform configuration step."
  (write-make-program-configuration))

;;;; End of file `action.lisp'
