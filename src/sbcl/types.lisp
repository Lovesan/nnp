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
  (defconstant +low-index-size+ 29)

  (define-global-var -optypes- (make-hash-table :test 'eq))
  (declaim (type hash-table -optypes-))

  (defstruct (optype (:conc-name %optype-))
    (name (error "Name required") :type symbol)
    (real-type (error "Real type required") :type t)
    (scs (error "Storage class not supplied") :type list)
    (vop-type nil :type t)
    (vector-type nil :type symbol)
    (width (error "Type width required") :type (unsigned-byte 10))
    (simd-width 1 :type (unsigned-byte 10))
    element-type
    immediate-p)

  (defun find-optype (name)
    (declare (type symbol name))
    (let ((type (gethash name -optypes-)))
      (unless type
        (error "Unable to find operation type ~s" name))
      (the optype type)))

  (defun optype-real-type (name)
    (%optype-real-type (find-optype name)))

  (defun optype-scs (name)
    (%optype-scs (find-optype name)))

  (defun optype-vop-type (name)
    (or (%optype-vop-type (find-optype name))
        (error "Type ~s has not VOP type" name)))

  (defun optype-width (name)
    (%optype-width (find-optype name)))

  (defun optype-simd-width (name)
    (%optype-simd-width (find-optype name)))

  (defun optype-vector-type (name)
    (or (%optype-vector-type (find-optype name))
        (error "No vector type for ~s" name)))

  (defun optype-element-type (name)
    (%optype-element-type (find-optype name)))

  (defun optype-immediate-p (name)
    (%optype-immediate-p (find-optype name)))

  (defun optype-immediate-low-high (name)
    (let ((type (find-optype name)))
      (assert (%optype-immediate-p type))
      (values 0 (1- (expt 2 (%optype-width type))))))

  (defun optype-index-scale (name index-tn)
    (let* ((element-type (optype-element-type (optype-vector-type name)))
           (width-in-bytes (floor (optype-width element-type) 8)))
      (if (>= width-in-bytes (ash 1 sb-vm:n-fixnum-tag-bits))
        (sb-vm::index-scale width-in-bytes index-tn)
        width-in-bytes)))

  (defun optype-index-scale-form (name index-var)
    (let* ((element-type (optype-element-type (optype-vector-type name)))
           (width-in-bytes (floor (optype-width element-type) 8)))
      (if (>= width-in-bytes (ash 1 sb-vm:n-fixnum-tag-bits))
        `(sb-vm::index-scale ,width-in-bytes ,index-var)
        width-in-bytes)))

  (defun index-scs (vec-type)
    (let* ((element-type (optype-element-type vec-type))
           (width-in-bytes (floor (optype-width element-type) 8)))
      (if (>= width-in-bytes (ash 1 sb-vm:n-fixnum-tag-bits))
        `(sb-vm::any-reg sb-vm::signed-reg sb-vm::unsigned-reg)
        `(sb-vm::signed-reg sb-vm::unsigned-reg))))

  (defun optypes-128 ()
    (loop :for v :being :the :hash-value :in -optypes-
          :when (and (= 128 (%optype-width v))
                     (not (eq (%optype-name v) 'p128)))
            :collect (%optype-name v)))

  (defun optypes-256 ()
    (loop :for v :being :the :hash-value :in -optypes-
          :when (and (= 256 (%optype-width v))
                     (not (eq (%optype-name v) 'p256)))
            :collect (%optype-name v))))

(defmacro defoptype (name real-type
                     &key (width (error "Type width required"))
                          (simd-width 1)
                          (scs (error "Storage classes not supplied"))
                          (vop-type (error "VOP type required"))
                          vector-type
                          (element-type name)
                          immediate
                          (deftype t)
                          (predicate (if (find #\- (symbol-name name))
                                       (symbolicate name '#:-p)
                                       (symbolicate name '#:p))))
  `(progn
     ,@(when deftype
         `((deftype ,name () ',real-type)
           (definline ,predicate (object)
             (typep object ',name))))
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (gethash ',name -optypes-)
             (make-optype :name ',name
                          :real-type ',real-type
                          :width ,width
                          :simd-width ,simd-width
                          :scs ',(ensure-list scs)
                          :vop-type ',vop-type
                          :vector-type ',vector-type
                          :element-type ',element-type
                          :immediate-p ,immediate))
       ',name)))

(macrolet ((def ()
             `(progn
                ,@(loop :for i :from 1 :to 8 :collect
                        `(defoptype ,(symbolicate '#:imm (write-to-string i))
                             (unsigned-byte ,i)
                           :width ,i
                           :scs sb-vm::descriptor-reg
                           :vop-type (:constant (unsigned-byte ,i))
                           :immediate t)))))
  (def))

(defoptype single single-float
  :width 32
  :scs sb-vm::single-reg
  :vop-type single-float
  :vector-type float-vector)

(defoptype double double-float
  :width 64
  :scs sb-vm::double-reg
  :vop-type double-float
  :vector-type double-vector)

(defoptype sbyte (signed-byte 8)
  :width 8
  :scs sb-vm::signed-reg
  :vop-type sb-vm::signed-num
  :vector-type sbyte-vector)

(defoptype ubyte (unsigned-byte 8)
  :width 8
  :scs sb-vm::unsigned-reg
  :vop-type sb-vm::unsigned-num
  :vector-type ubyte-vector)

(defoptype short (signed-byte 16)
  :width 16
  :scs sb-vm::signed-reg
  :vop-type sb-vm::signed-num
  :vector-type short-vector)

(defoptype ushort (unsigned-byte 16)
  :width 16
  :scs sb-vm::unsigned-reg
  :vop-type sb-vm::unsigned-num
  :vector-type ushort-vector)

(defoptype int (signed-byte 32)
  :width 32
  :scs sb-vm::signed-reg
  :vop-type sb-vm::signed-num
  :vector-type int-vector)

(defoptype uint (unsigned-byte 32)
  :width 32
  :scs sb-vm::unsigned-reg
  :vop-type sb-vm::unsigned-num
  :vector-type uint-vector)

(defoptype long (signed-byte 64)
  :width 64
  :scs sb-vm::signed-reg
  :vop-type sb-vm::signed-num
  :vector-type long-vector)

(defoptype ulong (unsigned-byte 64)
  :width 64
  :scs sb-vm::unsigned-reg
  :vop-type sb-vm::unsigned-num
  :vector-type ulong-vector)

(defoptype index (integer 0 #.array-total-size-limit)
  :width (floor (log array-total-size-limit 2))
  :scs (sb-vm::any-reg sb-vm::unsigned-reg sb-vm::signed-reg)
  :vop-type sb-vm::positive-fixnum
  :vector-type index-vector)

;; immediate
(defoptype low-index (unsigned-byte #.+low-index-size+)
  :width +low-index-size+
  :scs sb-vm::descriptor-reg
  :vop-type (:constant (unsigned-byte #.+low-index-size+))
  :immediate t)

(defoptype fixnum fixnum
  :width sb-vm:n-fixnum-bits
  :scs (sb-vm::any-reg sb-vm::signed-reg)
  :vop-type fixnum
  :vector-type fixnum-vector
  :deftype nil)

(defoptype positive-fixnum (integer 1 #.most-positive-fixnum)
  :width sb-vm:n-fixnum-bits
  :scs (sb-vm::any-reg sb-vm::unsigned-reg sb-vm::signed-reg)
  :vop-type sb-vm::positive-fixnum
  :vector-type index-vector)

(defoptype intptr (signed-byte #.sb-vm:n-word-bits)
  :width sb-vm:n-word-bits
  :scs sb-vm::signed-reg
  :vop-type sb-vm::signed-num
  :vector-type intptr-vector)

(defoptype uintptr (unsigned-byte #.sb-vm:n-word-bits)
  :width sb-vm:n-word-bits
  :scs sb-vm::unsigned-reg
  :vop-type sb-vm::unsigned-num
  :vector-type uintptr-vector)

(defoptype pointer sb-sys:system-area-pointer
  :width sb-vm:n-word-bits
  :scs sb-vm::sap-reg
  :vop-type sb-sys:system-area-pointer)

(defoptype float-vector (simple-array single (*))
  :width sb-vm:n-word-bits
  :scs sb-vm::descriptor-reg
  :vop-type sb-vm::simple-array-single-float
  :element-type single)

(defoptype double-vector (simple-array double (*))
  :width sb-vm:n-word-bits
  :scs sb-vm::descriptor-reg
  :vop-type sb-vm::simple-array-double-float
  :element-type double)

(defoptype sbyte-vector (simple-array sbyte (*))
  :width sb-vm:n-word-bits
  :scs sb-vm::descriptor-reg
  :vop-type sb-vm::simple-array-signed-byte-8
  :element-type sbyte)

(defoptype ubyte-vector (simple-array ubyte (*))
  :width sb-vm:n-word-bits
  :scs sb-vm::descriptor-reg
  :vop-type sb-vm::simple-array-unsigned-byte-8
  :element-type ubyte)

(defoptype short-vector (simple-array short (*))
  :width sb-vm:n-word-bits
  :scs sb-vm::descriptor-reg
  :vop-type sb-vm::simple-array-signed-byte-16
  :element-type short)

(defoptype ushort-vector (simple-array ushort (*))
  :width sb-vm:n-word-bits
  :scs sb-vm::descriptor-reg
  :vop-type sb-vm::simple-array-unsigned-byte-16
  :element-type ushort)

(defoptype int-vector (simple-array int (*))
  :width sb-vm:n-word-bits
  :scs sb-vm::descriptor-reg
  :vop-type sb-vm::simple-array-signed-byte-32
  :element-type int)

(defoptype uint-vector (simple-array uint (*))
  :width sb-vm:n-word-bits
  :scs sb-vm::descriptor-reg
  :vop-type sb-vm::simple-array-unsigned-byte-32
  :element-type uint)

(defoptype long-vector (simple-array long (*))
  :width sb-vm:n-word-bits
  :scs sb-vm::descriptor-reg
  :vop-type sb-vm::simple-array-signed-byte-64
  :element-type long)

(defoptype ulong-vector (simple-array ulong (*))
  :width sb-vm:n-word-bits
  :scs sb-vm::descriptor-reg
  :vop-type sb-vm::simple-array-unsigned-byte-64
  :element-type ulong)

(defoptype index-vector (simple-array index (*))
  :width sb-vm:n-word-bits
  :scs sb-vm::descriptor-reg
  :vop-type sb-vm::simple-array-unsigned-fixnum
  :element-type index)

(defoptype fixnum-vector (simple-array fixnum (*))
  :width sb-vm:n-word-bits
  :scs sb-vm::descriptor-reg
  :vop-type sb-vm::simple-array-fixnum
  :element-type fixnum)

(defoptype intptr-vector (simple-array intptr (*))
  :width sb-vm:n-word-bits
  :scs sb-vm::descriptor-reg
  :vop-type #.(intern (format nil "~a~s"
                              '#:simple-array-signed-byte-
                              sb-vm:n-word-bits)
                      '#:sb-vm)
  :element-type intptr)

(defoptype uintptr-vector (simple-array uintptr (*))
  :width sb-vm:n-word-bits
  :scs sb-vm::descriptor-reg
  :vop-type #.(intern (format nil "~a~s"
                              '#:simple-array-unsigned-byte-
                              sb-vm:n-word-bits)
                      '#:sb-vm)
  :element-type uintptr)

(defoptype float4 (simd-pack single)
  :width 128
  :simd-width 4
  :scs sb-vm::single-sse-reg
  :vop-type sb-vm::simd-pack-single
  :vector-type float-vector
  :element-type single)

(defoptype float8 (simd-pack-256 single)
  :width 256
  :simd-width 8
  :scs sb-vm::single-avx2-reg
  :vop-type sb-vm::simd-pack-256-single
  :vector-type float-vector
  :element-type single)

(defoptype double2 (simd-pack double)
  :width 128
  :simd-width 2
  :scs sb-vm::double-sse-reg
  :vop-type sb-vm::simd-pack-double
  :vector-type double-vector
  :element-type double)

(defoptype double4 (simd-pack-256 double)
  :width 256
  :simd-width 4
  :scs sb-vm::double-avx2-reg
  :vop-type sb-vm::simd-pack-256-double
  :vector-type double-vector
  :element-type double)

(defoptype sbyte16 (simd-pack sbyte)
  :width 128
  :simd-width 16
  :scs sb-vm::int-sse-reg
  :vop-type sb-vm::simd-pack-sb8
  :vector-type sbyte-vector
  :element-type sbyte)

(defoptype sbyte32 (simd-pack-256 sbyte)
  :width 256
  :simd-width 32
  :scs sb-vm::int-avx2-reg
  :vop-type sb-vm::simd-pack-256-sb8
  :vector-type sbyte-vector
  :element-type sbyte)

(defoptype ubyte16 (simd-pack ubyte)
  :width 128
  :simd-width 16
  :scs sb-vm::int-sse-reg
  :vop-type sb-vm::simd-pack-ub8
  :vector-type ubyte-vector
  :element-type ubyte)

(defoptype ubyte32 (simd-pack-256 ubyte)
  :width 256
  :simd-width 32
  :scs sb-vm::int-avx2-reg
  :vop-type sb-vm::simd-pack-256-ub8
  :vector-type ubyte-vector
  :element-type ubyte)

(defoptype short8 (simd-pack short)
  :width 128
  :simd-width 8
  :scs sb-vm::int-sse-reg
  :vop-type sb-vm::simd-pack-sb16
  :vector-type short-vector
  :element-type short)

(defoptype short16 (simd-pack-256 short)
  :width 256
  :simd-width 16
  :scs sb-vm::int-avx2-reg
  :vop-type sb-vm::simd-pack-256-sb16
  :vector-type short-vector
  :element-type short)

(defoptype ushort8 (simd-pack ushort)
  :width 128
  :simd-width 8
  :scs sb-vm::int-sse-reg
  :vop-type sb-vm::simd-pack-ub16
  :vector-type ushort-vector
  :element-type ushort)

(defoptype ushort16 (simd-pack-256 ushort)
  :width 256
  :simd-width 16
  :scs sb-vm::int-avx2-reg
  :vop-type sb-vm::simd-pack-256-ub16
  :vector-type ushort-vector
  :element-type ushort)

(defoptype int4 (simd-pack int)
  :width 128
  :simd-width 4
  :scs sb-vm::int-sse-reg
  :vop-type sb-vm::simd-pack-sb32
  :vector-type int-vector
  :element-type int)

(defoptype int8 (simd-pack-256 int)
  :width 256
  :simd-width 8
  :scs sb-vm::int-avx2-reg
  :vop-type sb-vm::simd-pack-256-sb32
  :vector-type int-vector
  :element-type int)

(defoptype uint4 (simd-pack uint)
  :width 128
  :simd-width 4
  :scs sb-vm::int-sse-reg
  :vop-type sb-vm::simd-pack-ub32
  :vector-type uint-vector
  :element-type uint)

(defoptype uint8 (simd-pack-256 uint)
  :width 256
  :simd-width 8
  :scs sb-vm::int-avx2-reg
  :vop-type sb-vm::simd-pack-256-ub32
  :vector-type uint-vector
  :element-type uint)

(defoptype long2 (simd-pack long)
  :width 128
  :simd-width 2
  :scs sb-vm::int-sse-reg
  :vop-type sb-vm::simd-pack-sb64
  :vector-type long-vector
  :element-type long)

(defoptype long4 (simd-pack-256 long)
  :width 256
  :simd-width 4
  :scs sb-vm::int-avx2-reg
  :vop-type sb-vm::simd-pack-256-sb64
  :vector-type long-vector
  :element-type long)

(defoptype ulong2 (simd-pack ulong)
  :width 128
  :simd-width 2
  :scs sb-vm::int-sse-reg
  :vop-type sb-vm::simd-pack-ub64
  :vector-type ulong-vector
  :element-type ulong)

(defoptype ulong4 (simd-pack-256 ulong)
  :width 256
  :simd-width 4
  :scs sb-vm::int-avx2-reg
  :vop-type sb-vm::simd-pack-256-ub64
  :vector-type ulong-vector
  :element-type ulong)

(defoptype p128 simd-pack
  :width 128
  :scs (sb-vm::single-sse-reg
        sb-vm::double-sse-reg
        sb-vm::int-sse-reg)
  :vop-type simd-pack
  :element-type t
  :deftype nil)

(defoptype p256 simd-pack
  :width 256
  :scs (sb-vm::single-avx2-reg
        sb-vm::double-avx2-reg
        sb-vm::int-avx2-reg)
  :vop-type simd-pack-256
  :element-type t
  :deftype nil)

;; Matrix is an opaque type. In an ideal world, that would be AVX512 vector
;;   but that's not currently supported neither by SBCL nor by mainstream processors.
;; In a bit less ideal but still wonderful world, SBCL could have a
;;   calling convention similiar to C's __vectorcall on Windows.
;; Still, at least SBCL can allocate simple arrays on stack, and that's good enough.
(defoptype float4x4 (simple-array single (16))
  :width 512
  :simd-width 16
  :scs sb-vm::descriptor-reg
  :vop-type sb-vm::simple-array-single-float
  :element-type single
  :vector-type float-vector)

(deftype p128 ()
  '(or float4 double2 sbyte16 ubyte16 short8 ushort8 int4 uint4 long2 ulong2))

(deftype p256 ()
  '(or float8 double4 sbyte32 ubyte32 short16 ushort16 int8 uint8 long4 ulong4))

(defmacro with-primitive-argument ((symbol type) &body body)
  (cond
    ((optype-immediate-p type)
     (multiple-value-bind (low high) (optype-immediate-low-high type)
       `(ecase ,symbol
          ,@(loop :for v :from low :to high
                  :collect `(,v (symbol-macrolet ((,symbol ,v)) ,@body))))))
    ((symbolp type)
     (case type
       (p128
        `(etypecase ,symbol
           ,@(loop :for type :in (optypes-128)
                   :collect `(,type ,@body))))
       (p256
        `(etypecase ,symbol
           ,@(loop :for type :in (optypes-256)
                   :collect `(,type ,@body))))
       (t `(progn ,@body))))
    (t (error "Ill-formed type: ~s" type))))

(defmacro with-primitive-arguments ((&rest forms) &body body)
  (if (endp forms)
    `(progn ,@body)
    `(with-primitive-argument ,(first forms)
       (with-primitive-arguments ,(rest forms)
         ,@body))))

;;; vim: ft=lisp et
