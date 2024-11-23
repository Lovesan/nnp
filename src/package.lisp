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

(in-package #:cl-user)

(uiop:define-package #:nnp
  (:use #:cl #:global-vars)
  (:import-from #:alexandria
                #:define-constant
                #:ensure-list
                #:symbolicate
                #:with-gensyms
                #:once-only)
  #+sbcl
  (:import-from #:sb-c
                #:define-vop
                #:defknown
                #:foldable
                #:flushable
                #:movable
                #:always-translatable
                #:location=
                #:move
                #:tn-p)
  #+sbcl
  (:import-from #:sb-assem
                #:inst
                #:gen-label
                #:emit-label)
  #+sbcl
  (:import-from #:sb-ext
                #:simd-pack
                #:simd-pack-256)
  (:export
   ;; float4
   #:single
   #:single!
   #:float!
   #:float-vector
   #:float-vector-p
   #:float4
   #:float4p
   #:float4!
   #:make-float4
   #:float4-values
   #:float4-permute
   #:float4-shuffle
   #:float4-extract
   #:float4-interleave-low
   #:float4-interleave-high
   #:float4-load
   #:float4-row-major-aref
   #:float4-aref
   #:float4-mem-ref
   #:float4-mem-aref
   #:float4-store
   #:float4-non-temporal-load
   #:float4-non-temporal-row-major-aref
   #:float4-non-temporal-aref
   #:float4-non-temporal-mem-ref
   #:float4-non-temporal-mem-aref
   #:float4-non-temporal-store
   #:float4+
   #:float4*
   #:float4-min
   #:float4-max
   #:float4-and
   #:float4-or
   #:float4-xor
   #:float4-incf
   #:float4-decf
   #:float4-clamp
   #:float4-
   #:float4/
   #:float4-h+
   #:float4-h-
   #:float4-h*
   #:float4-hmax
   #:float4-hmin
   #:float4-hand
   #:float4-hor
   #:float4-hxor
   #:float4-fmadd
   #:float4-fnmadd
   #:float4-fmsub
   #:float4-fmaddsub
   #:float4-fmsubadd
   #:float4-andc1
   #:float4-not
   #:float4=
   #:float4<
   #:float4>
   #:float4<=
   #:float4>=
   #:float4/=
   #:float4-truncate
   #:float4-round
   #:float4-floor
   #:float4-ceiling
   #:float4-abs
   #:float4-sqrt
   #:float4-rsqrt
   #:float4-dot
   #:float4-length
   #:float4-unit
   #:float3-dot
   #:float3-length
   #:float3-unit
   #:float2-dot
   #:float2-length
   #:float2-unit
   #:float4-lerp
   #:float4-saturate
   #:float4-scale
   #:float4-log
   #:float4-exp
   #:float4-expt
   #:float4-sin
   #:float4-cos
   #:float4-sincos
   #:float4-tancot
   #:float4-tan
   #:float4-cot

   ;; float8
   #:float8
   #:float8p
   #:float8!
   #:make-float8
   #:float8-values
   #:float8-permute
   #:float8-shuffle
   #:float8-shuffle*
   #:float8-interleave-low
   #:float8-interleave-high
   #:float8-extract-low
   #:float8-extract-high
   #:float8-insert-low
   #:float8-insert-high
   #:float8-load
   #:float8-row-major-aref
   #:float8-aref
   #:float8-mem-ref
   #:float8-mem-aref
   #:float8-store
   #:float8-non-temporal-load
   #:float8-non-temporal-row-major-aref
   #:float8-non-temporal-aref
   #:float8-non-temporal-mem-ref
   #:float8-non-temporal-mem-aref
   #:float8-non-temporal-store
   #:float8+
   #:float8*
   #:float8-min
   #:float8-max
   #:float8-and
   #:float8-or
   #:float8-xor
   #:float8-incf
   #:float8-decf
   #:float8-clamp
   #:float8-
   #:float8/
   #:float8-h+
   #:float8-h-
   #:float8-h*
   #:float8-hmax
   #:float8-hmin
   #:float8-hand
   #:float8-hor
   #:float8-hxor
   #:float8-fmadd
   #:float8-fnmadd
   #:float8-fmsub
   #:float8-fmaddsub
   #:float8-fmsubadd
   #:float8-andc1
   #:float8-not
   #:float8=
   #:float8<
   #:float8>
   #:float8<=
   #:float8>=
   #:float8/=
   #:float8-truncate
   #:float8-round
   #:float8-floor
   #:float8-ceiling
   #:float8-abs
   #:float8-sqrt
   #:float8-rsqrt

   ;; double2
   #:double
   #:double!
   #:double-vector
   #:double-vector-p
   #:double2
   #:double2p
   #:double2!
   #:make-double2
   #:double2-values
   #:double2-permute
   #:double2-shuffle
   #:double2-interleave-low
   #:double2-interleave-high
   #:double2-load
   #:double2-row-major-aref
   #:double2-aref
   #:double2-mem-ref
   #:double2-mem-aref
   #:double2-store
   #:double2-non-temporal-load
   #:double2-non-temporal-row-major-aref
   #:double2-non-temporal-aref
   #:double2-non-temporal-mem-ref
   #:double2-non-temporal-mem-aref
   #:double2-non-temporal-store
   #:double2+
   #:double2*
   #:double2-min
   #:double2-max
   #:double2-and
   #:double2-or
   #:double2-xor
   #:double2-incf
   #:double2-decf
   #:double2-clamp
   #:double2-
   #:double2/
   #:double2-h+
   #:double2-h-
   #:double2-h*
   #:double2-hmax
   #:double2-hmin
   #:double2-hand
   #:double2-hor
   #:double2-hxor
   #:double2-fmadd
   #:double2-fnmadd
   #:double2-fmsub
   #:double2-fmaddsub
   #:double2-fmsubadd
   #:double2-andc1
   #:double2-not
   #:double2=
   #:double2<
   #:double2>
   #:double2<=
   #:double2>=
   #:double2/=
   #:double2-truncate
   #:double2-round
   #:double2-floor
   #:double2-ceiling
   #:double2-abs
   #:double2-sqrt
   #:double2-rsqrt

   ;; double4
   #:double4
   #:double4p
   #:double4!
   #:make-double4
   #:double4-values
   #:double4-permute
   #:double4-shuffle
   #:double4-shuffle*
   #:double4-interleave-low
   #:double4-interleave-high
   #:double4-extract-low
   #:double4-extract-high
   #:double4-insert-low
   #:double4-insert-high
   #:double4-load
   #:double4-row-major-aref
   #:double4-aref
   #:double4-mem-ref
   #:double4-mem-aref
   #:double4-store
   #:double4-non-temporal-load
   #:double4-non-temporal-row-major-aref
   #:double4-non-temporal-aref
   #:double4-non-temporal-mem-ref
   #:double4-non-temporal-mem-aref
   #:double4-non-temporal-store
   #:double4+
   #:double4*
   #:double4-min
   #:double4-max
   #:double4-and
   #:double4-or
   #:double4-xor
   #:double4-incf
   #:double4-decf
   #:double4-clamp
   #:double4-
   #:double4/
   #:double4-h+
   #:double4-h-
   #:double4-h*
   #:double4-hmax
   #:double4-hmin
   #:double4-hand
   #:double4-hor
   #:double4-hxor
   #:double4-fmadd
   #:double4-fnmadd
   #:double4-fmsub
   #:double4-fmaddsub
   #:double4-fmsubadd
   #:double4-andc1
   #:double4-not
   #:double4=
   #:double4<
   #:double4>
   #:double4<=
   #:double4>=
   #:double4/=
   #:double4-truncate
   #:double4-round
   #:double4-floor
   #:double4-ceiling
   #:double4-abs
   #:double4-sqrt
   #:double4-rsqrt

   ;; int4
   #:int
   #:int!
   #:int-vector
   #:int-vector-p
   #:int4
   #:int4p
   #:int4!
   #:make-int4
   #:int4-values
   #:int4-permute
   #:int4-shuffle
   #:int4-extract
   #:int4-interleave-low
   #:int4-interleave-high
   #:int4-load
   #:int4-row-major-aref
   #:int4-aref
   #:int4-mem-ref
   #:int4-mem-aref
   #:int4-store
   #:int4-non-temporal-load
   #:int4-non-temporal-row-major-aref
   #:int4-non-temporal-aref
   #:int4-non-temporal-mem-ref
   #:int4-non-temporal-mem-aref
   #:int4-non-temporal-store
   #:int4+
   #:int4*
   #:int4-min
   #:int4-max
   #:int4-and
   #:int4-or
   #:int4-xor
   #:int4-incf
   #:int4-decf
   #:int4-clamp
   #:int4-shiftl
   #:int4-shiftr
   #:int4-
   #:int4-h+
   #:int4-h-
   #:int4-h*
   #:int4-hmax
   #:int4-hmin
   #:int4-hand
   #:int4-hor
   #:int4-hxor
   #:int4-andc1
   #:int4-not
   #:int4=
   #:int4<
   #:int4>
   #:int4<=
   #:int4>=
   #:int4/=
   #:int4-abs

   ;; int8
   #:int8
   #:int8p
   #:int8!
   #:make-int8
   #:int8-values
   #:int8-permute
   #:int8-shuffle
   #:int8-shuffle*
   #:int8-interleave-low
   #:int8-interleave-high
   #:int8-extract-low
   #:int8-extract-high
   #:int8-insert-low
   #:int8-insert-high
   #:int8-load
   #:int8-row-major-aref
   #:int8-aref
   #:int8-mem-ref
   #:int8-mem-aref
   #:int8-store
   #:int8-non-temporal-load
   #:int8-non-temporal-row-major-aref
   #:int8-non-temporal-aref
   #:int8-non-temporal-mem-ref
   #:int8-non-temporal-mem-aref
   #:int8-non-temporal-store
   #:int8+
   #:int8*
   #:int8-min
   #:int8-max
   #:int8-and
   #:int8-or
   #:int8-xor
   #:int8-incf
   #:int8-decf
   #:int8-clamp
   #:int8-shiftl
   #:int8-shiftr
   #:int8-
   #:int8-h+
   #:int8-h-
   #:int8-h*
   #:int8-hmax
   #:int8-hmin
   #:int8-hand
   #:int8-hor
   #:int8-hxor
   #:int8-andc1
   #:int8-not
   #:int8=
   #:int8<
   #:int8>
   #:int8<=
   #:int8>=
   #:int8/=
   #:int8-abs

   ;; long2
   #:long
   #:long!
   #:long-vector
   #:long-vector-p
   #:long2
   #:long2p
   #:long2!
   #:make-long2
   #:long2-values
   #:long2-permute
   #:long2-shuffle
   #:long2-interleave-low
   #:long2-interleave-high
   #:long2-load
   #:long2-row-major-aref
   #:long2-aref
   #:long2-mem-ref
   #:long2-mem-aref
   #:long2-store
   #:long2-non-temporal-load
   #:long2-non-temporal-row-major-aref
   #:long2-non-temporal-aref
   #:long2-non-temporal-mem-ref
   #:long2-non-temporal-mem-aref
   #:long2-non-temporal-store
   #:long2+
   #:long2*
   #:long2-min
   #:long2-max
   #:long2-and
   #:long2-or
   #:long2-xor
   #:long2-incf
   #:long2-decf
   #:long2-clamp
   #:long2-shiftl
   #:long2-shiftr
   #:long2-
   #:long2-h+
   #:long2-h-
   #:long2-h*
   #:long2-hmax
   #:long2-hmin
   #:long2-hand
   #:long2-hor
   #:long2-hxor
   #:long2-andc1
   #:long2-not
   #:long2=
   #:long2<
   #:long2>
   #:long2<=
   #:long2>=
   #:long2/=
   #:long2-abs

   ;; long4
   #:long4
   #:long4p
   #:long4!
   #:make-long4
   #:long4-values
   #:long4-permute
   #:long4-shuffle
   #:long4-shuffle*
   #:long4-interleave-low
   #:long4-interleave-high
   #:long4-extract-low
   #:long4-extract-high
   #:long4-insert-low
   #:long4-insert-high
   #:long4-load
   #:long4-row-major-aref
   #:long4-aref
   #:long4-mem-ref
   #:long4-mem-aref
   #:long4-store
   #:long4-non-temporal-load
   #:long4-non-temporal-row-major-aref
   #:long4-non-temporal-aref
   #:long4-non-temporal-mem-ref
   #:long4-non-temporal-mem-aref
   #:long4-non-temporal-store
   #:long4+
   #:long4*
   #:long4-min
   #:long4-max
   #:long4-and
   #:long4-or
   #:long4-xor
   #:long4-incf
   #:long4-decf
   #:long4-clamp
   #:long4-shiftl
   #:long4-shiftr
   #:long4-
   #:long4-h+
   #:long4-h-
   #:long4-h*
   #:long4-hmax
   #:long4-hmin
   #:long4-hand
   #:long4-hor
   #:long4-hxor
   #:long4-andc1
   #:long4-not
   #:long4=
   #:long4<
   #:long4>
   #:long4<=
   #:long4>=
   #:long4/=
   #:long4-abs

   ;; float4x4
   #:make-float4x4
   #:float4x4p
   #:float4x4-r0
   #:float4x4-r1
   #:float4x4-r2
   #:float4x4-r3
   #:float4x4-identity
   #:float2-transform
   #:float3-transform
   #:float4-transform
   #:float4x4*
   #:float4x4-transpose
   #:float4-tensor-product

   ;; misc
   #:imm1
   #:imm2
   #:imm3
   #:imm4
   #:imm5
   #:imm6
   #:imm7
   #:imm8
   #:index
   #:index-vector
   #:index-vector-p
   #:intptr
   #:intptr-vector
   #:intptr-vector-p
   #:uintptr
   #:uintptr-vector
   #:uintptr-vector-p
   #:pointer))

;;; vim: ft=lisp et
