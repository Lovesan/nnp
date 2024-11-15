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

;; reinterpret casts

(macrolet ((defcasts ()
             `(progn
                ,@(loop :for type :in (optypes-128)
                        :for name = (symbolicate! type '#:-from-p128)
                        :for vop-name = (symbolicate% name)
                        :append
                        `((defvop ,vop-name ((v p128 :target rv))
                              ((rv ,type))
                              ()
                            (unless (location= rv v)
                              (move rv v)))
                          (definline ,name (v)
                            (declare (type p128 v))
                            (with-primitive-argument (v p128)
                              (,vop-name v))))))))
  (defcasts))

(macrolet ((def (to from)
             `(defvop ,(symbolicate! to '#:-from- from)
                  ((x ,from :target rv))
                  ((rv ,to))
                  ()
                (unless (location= x rv)
                  (move rv x)))))
  (def single float4)
  (def double double2)
  (def float4 single)
  (def double2 double))

(macrolet ((def (to from inst)
             `(defvop ,(symbolicate! to '#:-from- from)
                  ((v ,from))
                  ((rv ,to))
                  ()
                (inst ,inst rv v))))
  (def uint int4 vmovd)
  (def uint uint4 vmovd)
  (def long long2 vmovq)
  (def long ulong2 vmovq)
  (def ulong long2 vmovq)
  (def ulong ulong2 vmovq)
  (def int4 int vmovd)
  (def int4 uint vmovd)
  (def uint4 int vmovd)
  (def uint4 uint vmovd)
  (def long2 long vmovq)
  (def long2 ulong vmovq)
  (def ulong2 long vmovq)
  (def ulong2 ulong vmovq))

(macrolet ((def (to from size)
             `(defvop ,(symbolicate! to '#:-from- from)
                  ((v ,from))
                  ((rv ,to))
                  ()
                (inst vmovd rv v)
                (inst movsx '(,size :qword) rv rv))))
  (def sbyte sbyte16 :byte)
  (def sbyte ubyte16 :byte)
  (def short short8 :word)
  (def short ushort8 :word)
  (def int int4 :dword)
  (def int uint4 :dword))

(macrolet ((def (to from size)
             `(defvop ,(symbolicate! to '#:-from- from)
                  ((x ,from))
                  ((rv ,to))
                  ()
                (inst movzx '(,size :dword) x x)
                (inst vmovd rv x))))
  (def sbyte16 sbyte :byte)
  (def ubyte16 sbyte :byte)
  (def sbyte16 ubyte :byte)
  (def ubyte16 ubyte :byte)
  (def short8 short :word)
  (def ushort8 short :word)
  (def short8 ushort :word)
  (def ushort8 ushort :word))

(macrolet ((def (to from size)
             `(defvop ,(symbolicate! to '#:-from- from)
                  ((v ,from))
                  ((rv ,to))
                  ()
                (inst vmovq rv v)
                (inst movzx '(,size :qword) rv rv))))
  (def ubyte sbyte16 :byte)
  (def ubyte ubyte16 :byte)
  (def ushort short8 :word)
  (def ushort ushort8 :word))

;; scalar casts

(macrolet ((def (type from inst)
             `(defvop ,(symbolicate type '#:-from- from)
                  ((x ,from :target rv))
                  ((rv ,type))
                  ()
                (inst ,inst rv x))))
  (def single double vcvtsd2ss)
  (def double single vcvtss2sd))

(macrolet ((def (type from inst)
             `(defvop ,(symbolicate type '#:-from- from)
                  ((x ,from))
                  ((rv ,type))
                  ()
                (inst ,inst rv rv x))))
  (def single long vcvtsi2ss)
  (def single int vcvtsi2ss)
  (def double long vcvtsi2sd)
  (def double int vcvtsi2sd))

(definline single (x)
  (etypecase x
    (single x)
    (double (single-from-double x))
    (long (single-from-long x))
    (real (coerce x 'single))))

(definline double (x)
  (etypecase x
    (double x)
    (single (double-from-single x))
    (long (double-from-long x))
    (real (coerce x 'double))))

(definline sbyte (x)
  (etypecase x
    (sbyte x)))

(definline ubyte (x)
  (etypecase x
    (ubyte x)))

(definline short (x)
  (etypecase x
    (short x)))

(definline ushort (x)
  (etypecase x
    (ushort x)))

(definline int (x)
  (etypecase x
    (int x)))

(definline uint (x)
  (etypecase x
    (uint x)))

(definline long (x)
  (etypecase x
    (long x)))

(definline ulong (x)
  (etypecase x
    (ulong x)))

(definline single! (x)
  (etypecase x
    (single x)
    (int (single-from-float4! (float4-from-p128! (int4-from-int! x))))
    (uint (single-from-float4! (float4-from-p128! (uint4-from-uint! x))))
    (p128 (single-from-float4! (float4-from-p128! x)))))

(definline double! (x)
  (etypecase x
    (double x)
    (long (double-from-double2! (double2-from-p128! (long2-from-long! x))))
    (ulong (double-from-double2! (double2-from-p128! (ulong2-from-ulong! x))))
    (p128 (double-from-double2! (double2-from-p128! x)))))

(definline sbyte! (x)
  (etypecase x
    (sbyte x)
    (ubyte (- x #x100))
    (p128 (sbyte-from-sbyte16! (sbyte16-from-p128! x)))))

(definline ubyte! (x)
  (etypecase x
    (ubyte x)
    (sbyte (logand x #xFF))
    (p128 (ubyte-from-ubyte16! (ubyte16-from-p128! x)))))

(definline short! (x)
  (etypecase x
    (short x)
    (ushort (- x #x10000))
    (p128 (short-from-short8! (short8-from-p128! x)))))

(definline ushort! (x)
  (etypecase x
    (ushort x)
    (short (logand x #xFFFF))
    (p128 (ushort-from-ushort8! (ushort8-from-p128! x)))))

(definline int! (x)
  (etypecase x
    (int x)
    (uint (- x #x100000000))
    (single (int-from-int4! (int4-from-p128! (float4-from-single! x))))
    (p128 (int-from-int4! (int4-from-p128! x)))))

(definline uint! (x)
  (etypecase x
    (uint x)
    (int (ldb (byte 32 0) x))
    (single (uint-from-uint4! (uint4-from-p128! (float4-from-single! x))))
    (p128 (uint-from-uint4! (uint4-from-p128! x)))))

(definline long! (x)
  (etypecase x
    (long x)
    (ulong (- x #x10000000000000000))
    (single (uint-from-uint4! (uint4-from-p128! (float4-from-single! x))))
    (double (long-from-long2! (long2-from-p128! (double2-from-double! x))))
    (p128 (long-from-long2! (long2-from-p128! x)))))

(definline ulong! (x)
  (etypecase x
    (ulong x)
    (long (ldb (byte 64 0) x))
    (single (uint-from-uint4! (uint4-from-p128! (float4-from-single! x))))
    (double (ulong-from-ulong2! (ulong2-from-p128! (double2-from-double! x))))
    (p128 (ulong-from-ulong2! (ulong2-from-p128! x)))))

;; broadcasts

(macrolet ((def (type inst)
             (let* ((eltype (optype-element-type type))
                    (name (symbolicate type '#:-from- eltype)))
               `(defvop ,name ((x ,eltype :target rv))
                    ((rv ,type))
                    ()
                  (inst ,inst rv x)))))
  (def float4 vbroadcastss)
  (def double2 vbroadcastsd))

(macrolet ((def (type move-inst bcast-inst)
             (let* ((eltype (optype-element-type type))
                    (name (symbolicate type '#:-from- eltype)))
               `(defvop ,name ((x ,eltype :target rv))
                    ((rv ,type))
                    ()
                  (inst ,move-inst rv x)
                  (inst ,bcast-inst rv rv)))))
  (def sbyte16 vmovd vpbroadcastb)
  (def ubyte16 vmovd vpbroadcastb)
  (def short8 vmovd vpbroadcastw)
  (def ushort8 vmovd vpbroadcastw)
  (def int4 vmovd vpbroadcastd)
  (def uint4 vmovd vpbroadcastd)
  (def long2 vmovq vpbroadcastq)
  (def ulong2 vmovq vpbroadcastq))

;; constructors

(defvop %make-float4 ((x single) (y single) (z single) (w single))
    ((rv float4))
    ((tmp float4))
  (cond ((location= rv x)
         (inst vinsertps x x y #x10)
         (inst vinsertps x x z #x20)
         (inst vinsertps x x w #x30))
        ((and (or (not (tn-p y))
                  (not (location= rv y)))
              (or (not (tn-p z))
                  (not (location= rv z)))
              (or (not (tn-p w))
                  (not (location= rv w))))
         (move rv x)
         (inst vinsertps rv rv y #x10)
         (inst vinsertps rv rv z #x20)
         (inst vinsertps rv rv w #x30))
        (t (move tmp x)
           (inst vinsertps tmp tmp y #x10)
           (inst vinsertps tmp tmp z #x20)
           (inst vinsertps tmp tmp w #x30)
           (move rv tmp))))

(definline make-float4 (x y z w)
  (%make-float4 (single x) (single y) (single z) (single w)))

(defvop %make-double2 ((x double :target rv) (y double))
    ((rv double2))
    ()
  (inst vunpcklpd rv x y))

(definline make-double2 (x y)
  (%make-double2 (double x) (double y)))

(macrolet ((def (type)
             (let* ((eltype (optype-element-type type))
                    (name (symbolicate '#:make- type))
                    (vop-name (symbolicate% name)))
               `(progn (defvop ,vop-name ((x ,eltype)
                                          (y ,eltype)
                                          (z ,eltype)
                                          (w ,eltype))
                           ((rv ,type))
                           ()
                         (inst vmovd rv x)
                         (inst vpinsrd rv rv y 1)
                         (inst vpinsrd rv rv z 2)
                         (inst vpinsrd rv rv w 3))
                       (definline ,name (x y z w)
                         (,vop-name (,eltype x)
                                    (,eltype y)
                                    (,eltype z)
                                    (,eltype w)))))))
  (def int4)
  (def uint4))

(macrolet ((def (type)
             (let* ((eltype (optype-element-type type))
                    (name (symbolicate '#:make- type))
                    (vop-name (symbolicate% name)))
               `(progn (defvop ,vop-name ((x ,eltype) (y ,eltype))
                           ((rv ,type))
                           ()
                         (inst vmovq rv x)
                         (inst vpinsrq rv rv y 1))
                       (definline ,name (x y)
                         (,vop-name (,eltype x) (,eltype y)))))))
  (def long2)
  (def ulong2))

(macrolet ((def (type)
             (let ((eltype (optype-element-type type))
                   (name (symbolicate '#:make- type))
                   (cast-name (symbolicate type '#:-from-p128!)))
               `(progn
                  (definline ,name (x0 x1 x2 x3 x4 x5 x6 x7)
                    (let ((x (logior (ash (logand #xFFFF (,eltype x0)) 0)
                                     (ash (logand #xFFFF (,eltype x1)) 16)))
                          (y (logior (ash (logand #xFFFF (,eltype x2)) 0)
                                     (ash (logand #xFFFF (,eltype x3)) 16)))
                          (z (logior (ash (logand #xFFFF (,eltype x4)) 0)
                                     (ash (logand #xFFFF (,eltype x5)) 16)))
                          (w (logior (ash (logand #xFFFF (,eltype x6)) 0)
                                     (ash (logand #xFFFF (,eltype x7)) 16))))
                      (,cast-name (make-uint4 x y z w))))))))
  (def short8)
  (def ushort8))

(macrolet ((def (type)
             (let ((eltype (optype-element-type type))
                   (name (symbolicate '#:make- type))
                   (cast-name (symbolicate! type '#:-from-p128)))
               `(definline ,name (x0 x1 x2 x3 x4 x5 x6 x7
                                  x8 x9 x10 x11 x12 x13 x14 x15)
                  ;; well, i guess it would be used rather sparingly, so
                  ;;   no optimizations
                  (let ((x (logior (ash (logand #xFF (,eltype x0)) 0)
                                   (ash (logand #xFF (,eltype x1)) 8)
                                   (ash (logand #xFF (,eltype x2)) 16)
                                   (ash (logand #xFF (,eltype x3)) 24)))
                        (y (logior (ash (logand #xFF (,eltype x4)) 0)
                                   (ash (logand #xFF (,eltype x5)) 8)
                                   (ash (logand #xFF (,eltype x6)) 16)
                                   (ash (logand #xFF (,eltype x7)) 24)))
                        (z (logior (ash (logand #xFF (,eltype x8)) 0)
                                   (ash (logand #xFF (,eltype x9)) 8)
                                   (ash (logand #xFF (,eltype x10)) 16)
                                   (ash (logand #xFF (,eltype x11)) 24)))
                        (w (logior (ash (logand #xFF (,eltype x12)) 0)
                                   (ash (logand #xFF (,eltype x13)) 8)
                                   (ash (logand #xFF (,eltype x14)) 16)
                                   (ash (logand #xFF (,eltype x15)) 24))))
                    (,cast-name (make-uint4 x y z w)))))))
  (def sbyte16)
  (def ubyte16))

;; permutation vops - needed early

(macrolet ((def (type)
             `(defvop ,(symbolicate% type '#:-permute)
                  ((v ,type :target rv)
                   (mask imm8))
                ((rv ,type))
                ()
                ;;  VPSHUFD is bugged in the current SBCL.
                ;;  The bug has been fixed but the fix is not released yet.
                (inst vpermilps rv v mask))))
  (def float4)
  (def int4)
  (def uint4))

(macrolet ((def (type)
             `(defvop ,(symbolicate% type '#:-permute)
                  ((v ,type :target rv)
                   (mask imm8))
                  ((rv ,type))
                  ()
                (inst vpermilpd rv v mask))))
  (def double2)
  (def long2)
  (def ulong2))

;; values - needed early

;; Values extraction is done as a function and not a VOP
;;   because this way SBCL can optimize away unneeded permutation insts

(macrolet ((def (type)
             (let* ((name (symbolicate% type '#:-values))
                    (eltype (optype-element-type type))
                    (cast (symbolicate! eltype '#:-from- type))
                    (permute (symbolicate% type '#:-permute)))
               `(definline ,name (v)
                  (declare (type ,type v))
                  (values (,cast v)
                          (,cast (,permute v 1))
                          (,cast (,permute v 2))
                          (,cast (,permute v 3)))))))
  (def float4)
  (def int4)
  (def uint4))

(macrolet ((def (type)
             (let* ((name (symbolicate% type '#:-values))
                    (eltype (optype-element-type type))
                    (cast (symbolicate! eltype '#:-from- type))
                    (permute (symbolicate% type '#:-permute)))
               `(definline ,name (v)
                  (declare (type ,type v))
                  (values (,cast v)
                          (,cast (,permute v 1)))))))
  (def double2)
  (def long2)
  (def ulong2))

;; casts

(defvop float4-from-int4 ((v int4 :target rv))
    ((rv float4))
    ()
  (inst vcvtdq2ps rv v))

;; emulate VCVTQQ2PD
(defvop double2-from-long2 ((v long2))
    ((rv double2))
    ((tmp long)
     (src long2))
  (if (location= rv v)
    (progn
      (move src v)
      (inst vpextrq tmp src 1)
      (inst vcvtsi2sd rv rv tmp)
      (inst vpextrq tmp src 0))
    (progn
      (inst vpextrq tmp v 1)
      (inst vcvtsi2sd rv rv tmp)
      (inst vpextrq tmp v 0)))
  (inst vpermilpd rv rv #4r00)
  (inst vcvtsi2sd rv rv tmp))

(definline float4 (x)
  (typecase x
    (float4 x)
    (int4 (float4-from-int4 x))
    ;; unsigned conversion is only available in AVX512
    (uint4 (multiple-value-bind (x y z w) (%uint4-values x)
             (make-float4 (float x)
                          (float y)
                          (float z)
                          (float w))))
    (t (float4-from-single (single x)))))

(definline double2 (x)
  (typecase x
    (double2 x)
    (long2 (double2-from-long2 x))
    (ulong2 (multiple-value-bind (x y) (%ulong2-values x)
              (make-double2 (double x)
                            (double y))))
    (t (double2-from-double (double x)))))

(definline sbyte16 (x)
  (typecase x
    (sbyte16 x)
    (t (sbyte16-from-sbyte (sbyte x)))))

(definline ubyte16 (x)
  (typecase x
    (ubyte16 x)
    (t (ubyte16-from-ubyte (ubyte x)))))

(definline short8 (x)
  (typecase x
    (short8 x)
    (t (short8-from-short (short x)))))

(definline ushort8 (x)
  (typecase x
    (ushort8 x)
    (t (ushort8-from-ushort (ushort x)))))

(definline int4 (x)
  (typecase x
    (int4 x)
    (t (int4-from-int (int x)))))

(definline uint4 (x)
  (typecase x
    (uint4 x)
    (t (uint4-from-uint (uint x)))))

(definline long2 (x)
  (typecase x
    (long2 x)
    (t (long2-from-long (long x)))))

(definline ulong2 (x)
  (typecase x
    (ulong2 x)
    (t (ulong2-from-ulong (ulong x)))))

(macrolet ((def (type (&rest allowed-scalar-types))
             (let* ((eltype (optype-element-type type))
                    (name (symbolicate! type)))
               `(definline ,name (x)
                  (etypecase x
                    (,type x)
                    (,eltype (,(symbolicate! type '#:-from- eltype) x))
                    ,@(when allowed-scalar-types
                        `(((or ,@allowed-scalar-types)
                           (,type (,(symbolicate! eltype) x)))))
                    (p128 (,(symbolicate! type '#:-from-p128) x)))))))
  (def float4 (int uint))
  (def double2 (long ulong))
  (def sbyte16 (ubyte))
  (def ubyte16 (sbyte))
  (def short8 (ushort))
  (def ushort8 (short))
  (def int4 (uint))
  (def uint4 (int))
  (def long2 (ulong))
  (def ulong2 (long)))

;; values - late

(macrolet ((def (type)
             (let* ((name (symbolicate type '#:-values))
                    (vop-name (symbolicate% name)))
               `(definline ,name (v)
                  (,vop-name (,type v))))))
  (def float4)
  (def int4)
  (def uint4)
  (def double2)
  (def long2)
  (def ulong2))

(macrolet ((def (type)
             (let* ((eltype (optype-element-type type))
                    (name (symbolicate type '#:-values))
                    (eltype-cast (symbolicate! eltype)))
               `(definline ,name (v)
                  (let* ((v (,type v)))
                    (multiple-value-bind (x y z w)
                        (uint4-values (uint4-from-p128! v))
                      (values (,eltype-cast (logand #xFFFF (ash x 0)))
                              (,eltype-cast (logand #xFFFF (ash x -16)))
                              (,eltype-cast (logand #xFFFF (ash y 0)))
                              (,eltype-cast (logand #xFFFF (ash y -16)))
                              (,eltype-cast (logand #xFFFF (ash z 0)))
                              (,eltype-cast (logand #xFFFF (ash z -16)))
                              (,eltype-cast (logand #xFFFF (ash w 0)))
                              (,eltype-cast (logand #xFFFF (ash w -16))))))))))
  (def short8)
  (def ushort8))

(macrolet ((def (type)
             (let* ((eltype (optype-element-type type))
                    (name (symbolicate type '#:-values))
                    (eltype-cast (symbolicate! eltype)))
               `(definline ,name (v)
                  (let* ((v (,type v)))
                    (multiple-value-bind (x y z w)
                        (uint4-values (uint4-from-p128! v))
                      (values (,eltype-cast (logand #xFF (ash x 0)))
                              (,eltype-cast (logand #xFF (ash x -8)))
                              (,eltype-cast (logand #xFF (ash x -16)))
                              (,eltype-cast (logand #xFF (ash x -24)))
                              (,eltype-cast (logand #xFF (ash y 0)))
                              (,eltype-cast (logand #xFF (ash y -8)))
                              (,eltype-cast (logand #xFF (ash y -16)))
                              (,eltype-cast (logand #xFF (ash y -24)))
                              (,eltype-cast (logand #xFF (ash z 0)))
                              (,eltype-cast (logand #xFF (ash z -8)))
                              (,eltype-cast (logand #xFF (ash z -16)))
                              (,eltype-cast (logand #xFF (ash z -24)))
                              (,eltype-cast (logand #xFF (ash w 0)))
                              (,eltype-cast (logand #xFF (ash w -8)))
                              (,eltype-cast (logand #xFF (ash w -16)))
                              (,eltype-cast (logand #xFF (ash w -24))))))))))
  (def sbyte16)
  (def ubyte16))

;; permutations

(macrolet ((def (type)
             (let* ((name (symbolicate type '#:-permute))
                    (vop-name (symbolicate% name)))
               `(progn (definline ,name (v x y z w)
                         (declare (type imm2 x y z w))
                         (let ((mask (logior (ash x 0)
                                             (ash y 2)
                                             (ash z 4)
                                             (ash w 6))))
                           (declare (type imm8 mask))
                           (with-primitive-argument (mask imm8)
                             (,vop-name (,type v) mask))))))))
  (def float4)
  (def int4)
  (def uint4))

(macrolet ((def (type)
             (let* ((name (symbolicate type '#:-permute))
                    (vop-name (symbolicate% name)))
               `(definline ,name (v x y)
                  (declare (type imm1 x y))
                  (let ((mask (logior (ash x 0)
                                      (ash y 1))))
                    (declare (type imm2 mask))
                    (with-primitive-argument (mask imm2)
                      (,vop-name (,type v) mask)))))))
  (def double2)
  (def long2)
  (def ulong2))

(macrolet ((def (type)
             (let* ((name (symbolicate type '#:-permute))
                    (low-name (symbolicate name '#:-low))
                    (high-name (symbolicate name '#:-high))
                    (low-vop-name (symbolicate% low-name))
                    (high-vop-name (symbolicate% high-name)))
               `(progn
                  (defvop ,low-vop-name ((v ,type :target rv)
                                         (mask imm8))
                      ((rv ,type))
                      ()
                    (inst vpshuflw rv v mask))
                  (defvop ,high-vop-name ((v ,type :target rv)
                                          (mask imm8))
                      ((rv ,type))
                      ()
                    (inst vpshufhw rv v mask))
                  (definline ,low-name (v x0 x1 x2 x3)
                    (declare (type imm2 x0 x1 x2 x3))
                    (let* ((v (,type v))
                           (imm-low (logior (ash x0 0)
                                            (ash x1 2)
                                            (ash x2 4)
                                            (ash x3 6))))
                      (declare (type imm8 imm-low))
                      (with-primitive-argument (imm-low imm8)
                        (,low-vop-name v imm-low))))
                  (definline ,high-name (v x4 x5 x6 x7)
                    (declare (type imm2 x4 x5 x6 x7))
                    (let* ((v (,type v))
                           (imm-high (logior (ash x4 0)
                                             (ash x5 2)
                                             (ash x6 4)
                                             (ash x7 6))))
                      (declare (type imm8 imm-high))
                      (with-primitive-argument (imm-high imm8)
                        (,high-vop-name v imm-high))))))))
  (def short8)
  (def ushort8))

(macrolet ((def (type)
             (let* ((name (symbolicate type '#:-permute))
                    (vop-name (symbolicate% name)))
               `(progn (defvop ,vop-name ((v ,type) (control ulong2))
                           ((rv ,type))
                           ()
                         (inst vpshufb rv v control))
                       (definline ,name (v control)
                         (,vop-name (,type v)
                                    (etypecase control
                                      (unsigned-byte
                                       (make-ulong2 (ldb (byte 64 0) control)
                                                    (ldb (byte 64 64) control)))
                                      (p128 (ulong2! control)))))))))
  (def sbyte16)
  (def ubyte16))

(macrolet ((def (type)
             (let* ((name (symbolicate type '#:-shuffle))
                    (vop-name (symbolicate% name)))
               `(progn
                  (defvop ,vop-name ((v1 ,type :target rv) (v2 ,type) (mask imm8))
                      ((rv ,type))
                      ()
                    (inst vshufps rv v1 v2 mask))
                  (definline ,name (v1 v2 x y z w)
                    (declare (type imm2 x y z w))
                    (let* ((v1 (,type v1))
                           (v2 (,type v2))
                           (mask (logior (ash x 0)
                                         (ash y 2)
                                         (ash z 4)
                                         (ash w 6))))
                      (with-primitive-argument (mask imm8)
                        (,vop-name v1 v2 mask))))))))
  (def float4)
  (def int4)
  (def uint4))

(macrolet ((def (type)
             (let* ((name (symbolicate type '#:-shuffle))
                    (vop-name (symbolicate% name)))
               `(progn
                  (defvop ,vop-name ((v1 ,type :target rv) (v2 ,type) (mask imm2))
                      ((rv ,type))
                      ()
                    (inst vshufpd rv v1 v2 mask))
                  (definline ,name (v1 v2 x y)
                    (declare (type imm1 x y))
                    (let* ((v1 (,type v1))
                           (v2 (,type v2))
                           (mask (logior (ash x 0)
                                         (ash y 1))))
                      (with-primitive-argument (mask imm2)
                        (,vop-name v1 v2 mask))))))))
  (def double2)
  (def long2)
  (def ulong2))

;;; vim: ft=lisp et
