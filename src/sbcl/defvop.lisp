;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-

;;; Copyright (C) 2024, Dmitry Ignatiev <lovesan.ru at gmail.com>

;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:

;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.

;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

(in-package #:nnp)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (shadowing-import '(sb-vm::ea))

  (defun defvop-args->vop-types (args)
    (loop :for (name type . rest) :in args
          :collect (optype-vop-type type)))

  (defun defvop-args-has-vec-p (args)
    (dolist (arg args)
      (destructuring-bind (name type &rest rest) arg
        (declare (ignore name rest))
        (let ((real-type (optype-real-type type)))
          (when (and (consp real-type)
                     (eql (first real-type) 'simple-array))
            (return arg))))))

  (defun defvop-args->vop-args (args)
    (loop :with vec-arg = (defvop-args-has-vec-p args)
          :for (name type . rest) :in args
          :if (and vec-arg (eql type 'index))
            :collect `(,name :scs ,(index-scs (second vec-arg)) ,@rest)
          :else :if (not (optype-immediate-p type))
                  :collect `(,name :scs ,(optype-scs type) ,@rest)))

  (defun defvop-args->vop-info (args)
    (loop :for (name type . rest) :in args
          :when (optype-immediate-p type)
            :collect name))

  (defun defvop-temp->vop-temp (temp)
    (loop :for (name type . rest) :in temp
          :collect `(:temporary (:sc ,(first (optype-scs type)) ,@rest) ,name)))

  (defun defvop-args->defknown-args (args)
    (loop :for (name type . rest) :in args
          :collect type))

  (defun defvop-results->defknown-result (results)
    `(values ,@(mapcar #'second results) &optional))

  (defun defvop-results->vop-results (results)
    (loop :for (name type . rest) :in results
          :if (optype-immediate-p type)
            :do (error "Immediates are not allowed in results: ~s" results)
          :else
            :collect `(,name :scs ,(optype-scs type) ,@rest)))

  (defun defvop-args-has-immediates-p (args)
    (loop :for (name type . rest) :in args
          :when (optype-immediate-p type)
            :return t))

  (defun defvop-args->wrapper (name args)
    (loop :for (arg-name arg-type . rest) :in args
          :collect arg-name :into arg-names
          :collect `(declare (type ,arg-type ,arg-name)) :into decls
          :collect `(,arg-name ,arg-type) :into primargs
          :finally (return `(defun ,name ,arg-names
                              ,@decls
                              (with-primitive-arguments ,primargs
                                (,name ,@arg-names)))))))

(defmacro defvop (name-and-options (&rest args) (&rest results) (&rest temp)
                  &body generator)
  (destructuring-bind (name &key (pure t)
                                 (foldable pure)
                                 (always-translatable t)
                                 ;; wrapper is required for constant folding
                                 (wrapper foldable)
                                 (translate nil translatep)
                                 (cost 1))
      (ensure-list name-and-options)
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         ,@(unless translatep
             `((defknown ,name ,(defvop-args->defknown-args args)
                   ,(defvop-results->defknown-result results)
                   (,@(when always-translatable '(sb-c:always-translatable))
                    ,@(when pure '(flushable movable))
                    ,@(when foldable '(foldable)))
                 :overwrite-fndb-silently t)))
         (define-vop (,name)
           (:translate ,(if translatep translate name))
           (:args ,@(defvop-args->vop-args args))
           (:arg-types ,@(defvop-args->vop-types args))
           ,@(when (defvop-args-has-immediates-p args)
               `((:info ,@(defvop-args->vop-info args))))
           (:results ,@(defvop-results->vop-results results))
           (:result-types ,@(defvop-args->vop-types results))
           ,@(defvop-temp->vop-temp temp)
           (:policy :fast-safe)
           (:generator ,cost ,@generator))
         ,@(when wrapper
             `(,(defvop-args->wrapper name args))))
       ',name)))

;;; vim: ft=lisp et
