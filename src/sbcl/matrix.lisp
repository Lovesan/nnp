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

(definline fill-float4x4 (m r0 r1 r2 r3)
  (declare (type float4x4 m))
  (let ((r0 (float4 r0))
        (r1 (float4 r1))
        (r2 (float4 r2))
        (r3 (float4 r3)))
    (setf (float4-row-major-aref m 0) r0
          (float4-row-major-aref m 1) r1
          (float4-row-major-aref m 2) r2
          (float4-row-major-aref m 3) r3)
    m))

(definline alloc-float4x4 ()
  (make-array 16 :element-type 'single-float))

(definline make-float4x4 (r0 r1 r2 r3)
  (let ((data (alloc-float4x4)))
    (fill-float4x4 data r0 r1 r2 r3)))

(definline float4x4-r0 (m)
  (declare (type float4x4 m))
  (float4-load m 0))

(definline float4x4-r1 (m)
  (declare (type float4x4 m))
  (float4-load m 4))

(definline float4x4-r2 (m)
  (declare (type float4x4 m))
  (float4-load m 8))

(definline float4x4-r3 (m)
  (declare (type float4x4 m))
  (float4-load m 12))

(definline copy-float4x4 (m)
  (declare (type float4x4 m))
  (make-float4x4 (float4x4-r0 m)
                 (float4x4-r1 m)
                 (float4x4-r2 m)
                 (float4x4-r3 m)))

(definline float4x4-identity ()
  (make-float4x4 (make-float4 1 0 0 0)
                 (make-float4 0 1 0 0)
                 (make-float4 0 0 1 0)
                 (make-float4 0 0 0 1)))

(definline float2-transform (v m)
  (declare (type float4x4 m))
  (let* ((v (float4 v))
         (rv (%float4-fmadd (%float4-permute v #4r1111)
                            (float4x4-r1 m)
                            (float4x4-r3 m)))
         (tmp (%float4-permute v #4r0000)))
    (setf rv (%float4-fmadd tmp (float4x4-r0 m) rv))
    rv))

(definline float3-transform (v m)
  (declare (type float4x4 m))
  (let* ((v (float4 v))
         (rv (%float4-fmadd (%float4-permute v #4r2222)
                            (float4x4-r2 m)
                            (float4x4-r3 m)))
         (tmp (%float4-permute v #4r1111)))
    (setf rv (%float4-fmadd tmp (float4x4-r1 m) rv)
          tmp (%float4-permute v #4r0000)
          rv (%float4-fmadd tmp (float4x4-r0 m) rv))
    rv))

(definline float4-transform (v m)
  (declare (type float4x4 m))
  (let* ((v (float4 v))
         (rv (%float4* (%float4-permute v #4r3333)
                       (float4x4-r3 m)))
         (tmp (%float4-permute v #4r2222)))
    (setf rv (%float4-fmadd tmp (float4x4-r2 m) rv)
          tmp (%float4-permute v #4r1111)
          rv (%float4-fmadd tmp (float4x4-r1 m) rv)
          tmp (%float4-permute v #4r0000)
          rv (%float4-fmadd tmp (float4x4-r0 m) rv))
    rv))

(definline %float4x4* (m1 m2)
  (declare (type float4x4 m1 m2))
  (let* ((t0 (%float8-insert-float4 (float8! (float4x4-r0 m1))
                                    (float4x4-r1 m1)
                                    1))
         (t1 (%float8-insert-float4 (float8! (float4x4-r2 m1))
                                    (float4x4-r3 m1)
                                    1))
         (u0 (%float8-insert-float4 (float8! (float4x4-r0 m2))
                                    (float4x4-r1 m2)
                                    1))
         (u1 (%float8-insert-float4 (float8! (float4x4-r2 m2))
                                    (float4x4-r3 m2)
                                    1))
         (a0 (%float8-shuffle t0 t0 #4r0000))
         (a1 (%float8-shuffle t1 t1 #4r0000))
         (b0 (%float8-shuffle* u0 u0 #x00))
         (c0 (%float8* a0 b0))
         (c1 (%float8* a1 b0))
         (a0 (%float8-shuffle t0 t0 #4r1111))
         (a1 (%float8-shuffle t1 t1 #4r1111))
         (b0 (%float8-shuffle* u0 u0 #x11))
         (c2 (%float8-fmadd a0 b0 c0))
         (c3 (%float8-fmadd a1 b0 c1))
         (a0 (%float8-shuffle t0 t0 #4r2222))
         (a1 (%float8-shuffle t1 t1 #4r2222))
         (b1 (%float8-shuffle* u1 u1 #x00))
         (c4 (%float8* a0 b1))
         (c5 (%float8* a1 b1))
         (a0 (%float8-shuffle t0 t0 #4r3333))
         (a1 (%float8-shuffle t1 t1 #4r3333))
         (b1 (%float8-shuffle* u1 u1 #x11))
         (c6 (%float8-fmadd a0 b1 c4))
         (c7 (%float8-fmadd a1 b1 c5))
         (t0 (%float8+ c2 c6))
         (t1 (%float8+ c3 c7)))
    (make-float4x4 (float4! t0)
                   (%float8-extract-float4 t0 1)
                   (float4! t1)
                   (%float8-extract-float4 t1 1))))

(defun float4x4* (&rest args)
  (loop :with result = (float4x4-identity)
        :for arg :in args
        :do (setf result (%float4x4* result arg))
        :finally (return result)))

(define-compiler-macro float4x4* (&rest args)
  (let ((len (length args)))
    (case len
      (0 `(float4x4-identity))
      (1 (first args))
      (t `(%float4x4*
           (float4x4* ,@(subseq args 0 (floor len 2)))
           (float4x4* ,@(subseq args (floor len 2))))))))

(definline float4x4-transpose (m)
  (declare (type float4x4 m))
  (let* ((t0 (%float8-insert-float4 (float8! (float4x4-r0 m))
                                    (float4x4-r1 m)
                                    1))
         (t1 (%float8-insert-float4 (float8! (float4x4-r2 m))
                                    (float4x4-r3 m)
                                    1))
         (tmp1 (%float8-interleave-low t0 t1))
         (tmp2 (%float8-interleave-high t0 t1))
         (tmp3 (%float8-shuffle* tmp1 tmp2 #x20))
         (tmp4 (%float8-shuffle* tmp1 tmp2 #x31))
         (tmp1 (%float8-interleave-low tmp3 tmp4))
         (tmp2 (%float8-interleave-high tmp3 tmp4))
         (t0 (%float8-shuffle* tmp1 tmp2 #x20))
         (t1 (%float8-shuffle* tmp1 tmp2 #x31)))
    (make-float4x4 (float4! t0)
                   (%float8-extract-float4 t0 1)
                   (float4! t1)
                   (%float8-extract-float4 t1 1))))

(definline float4-tensor-product (v1 v2)
  (let ((v1 (float4 v1))
        (v2 (float4 v2)))
    (make-float4x4 (%float4* (%float4-permute v1 #4r0000) v2)
                   (%float4* (%float4-permute v1 #4r1111) v2)
                   (%float4* (%float4-permute v1 #4r2222) v2)
                   (%float4* (%float4-permute v1 #4r3333) v2))))

;;; vim: ft=lisp et
