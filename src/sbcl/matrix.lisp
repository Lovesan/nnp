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

;;; vim: ft=lisp et
