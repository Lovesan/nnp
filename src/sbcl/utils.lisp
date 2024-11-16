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
  (defun symbolicate! (&rest args)
    (apply #'symbolicate (append args (list "!"))))

  (defun symbolicate% (&rest args)
    (apply #'symbolicate "%" args))

  (defun check-bounds-p (environment)
    (sb-c:policy environment (plusp sb-c::insert-array-bounds-checks)))

  (defun error-wrong-number-of-subscripts (array subscript-count)
    (error "Wrong number of subscripts ~s for an array of rank ~s"
           subscript-count
           (array-rank array)))

  (defun error-invalid-subscript (subscript axis limit)
    (error "Invalid subscript ~s for axis ~s, should be non-negative integer below ~s"
           subscript axis limit))

  (defun array-row-major-simd-index (array simd-width &rest subscripts)
    (declare (dynamic-extent subscripts)
             (type array array)
             (type (unsigned-byte 6) simd-width))
    (let ((rank (array-rank array))
          (len (length subscripts)))
      (unless (= len rank)
        (error-wrong-number-of-subscripts array subscripts))
      (let ((index 0)
            (stride 1))
        (declare (type fixnum index stride))
        (loop :for axis :from (1- rank) :downto 0
              :for width = simd-width :then 1
              :for subscript = (the fixnum (nth axis subscripts))
              :for dimension = (array-dimension array axis) :do
                (unless (and (>= subscript 0)
                             (<= (* subscript width) (- dimension width)))
                  (error-invalid-subscript subscript
                                           axis
                                           (floor dimension width)))
                (incf index (the fixnum (* width (the fixnum (* stride subscript)))))
                (setf stride (the fixnum (* stride dimension))))
        index))))

(defmacro definline (name (&rest args) &body body)
  `(progn (declaim (inline ,name))
          (defun ,name ,args ,@body)))

(defmacro with-row-major-simd-index ((index array simd-width &rest subscripts)
                                     &body body &environment env)
  (check-type index symbol)
  (check-type array symbol)
  (check-type simd-width (unsigned-byte 6))
  (dolist (subscript subscripts)
    (check-type subscript symbol))
  (with-gensyms (rank)
    (let* ((len (length subscripts))
           (rank-binding `(,rank (array-rank ,array)))
           (dimension-bindings (loop :for axis :below len
                                     :collect `(,(gensym (string '#:dimension))
                                                (array-dimension ,array ,axis))))
           (dimensions (mapcar #'first dimension-bindings))
           (stride-bindings (loop :for axis :from (1- len) :downto 1
                                  :for old-stride = nil :then new-stride
                                  :for new-stride = (gensym (string '#:stride))
                                  :for stride-binding = `(,new-stride
                                                          ,(nth axis dimensions))
                                    :then `(,new-stride (the index
                                                             (* ,(nth axis dimensions)
                                                                ,old-stride)))
                                  :collect stride-binding))
           (strides (nreverse (mapcar #'first stride-bindings)))
           (index-form
             `(the index (+ ,@(loop :for stride :in strides
                                    :for subscript :in subscripts
                                    :collect `(the index (* ,stride ,subscript)))
                            (the index
                                 (* ,simd-width ,(car (last subscripts))))))))
      `(let (,rank-binding)
         (unless (= ,rank ,len)
           (error-wrong-number-of-subscripts ,array ,len))
         (let ,dimension-bindings
           (declare (ignorable ,@dimensions))
           ,@(when (check-bounds-p env)
               (loop :for axis :downfrom (1- len) :to 0
                     :for dimension :in (reverse dimensions)
                     :for subscript :in (reverse subscripts)
                     :for width = simd-width :then 1
                     :collect `(unless (and (>= ,subscript 0)
                                            (<= (* ,subscript ,width)
                                                (- ,dimension ,width)))
                                 (error-invalid-subscript ,subscript
                                                          ,axis
                                                          (floor ,dimension ,width)))))
           (let* (,@stride-bindings
                  (,index ,index-form))
             ,@body))))))

;;; vim: ft=lisp et
