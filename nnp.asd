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

(defsystem #:nnp
  :version "0.0.1"
  :description "Not NumPy"
  :author "Dmitry Ignatiev <lovesan.ru at gmail.com>"
  :maintainer "Dmitry Ignatiev <lovesan.ru at gmail.com>"
  :licence "MIT"
  :defsystem-depends-on (#:trivial-features)
  :depends-on (#:uiop
               #:trivial-features
               #:global-vars
               #:alexandria)
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:module "sbcl"
                              :if-feature (:and :sbcl :x86-64)
                              :serial t
                              :components ((:file "utils")
                                           (:file "types")
                                           (:file "defvop")
                                           (:file "constructors")
                                           (:file "load-store")
                                           (:file "arith")
                                           (:file "math")
                                           (:file "matrix")))))))

;;; vim: ft=lisp et
