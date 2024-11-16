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

;; associative ops

(macrolet ((def (name type inst &optional identity)
             (let ((vop-name (symbolicate% name)))
               `(progn
                  (defvop ,vop-name ((v1 ,type :target rv)
                                     (v2 ,type))
                      ((rv ,type))
                      ()
                    (inst ,inst rv v1 v2))
                  ,(if identity
                     `(definline ,name (&rest args)
                        (declare (dynamic-extent args))
                        (loop :with acc = ,identity
                              :for x :in args
                              :do (setf acc (,vop-name acc (,type x)))
                              :finally (return acc)))
                     `(definline ,name (arg &rest more-args)
                        (declare (dynamic-extent more-args))
                        (loop :with acc = (,type arg)
                              :for x :in more-args
                              :do (setf acc (,vop-name acc (,type x)))
                              :finally (return acc))))
                  ,(if identity
                     `(define-compiler-macro ,name (&rest args)
                        (let ((len (length args)))
                          (case len
                            (0 ,identity)
                            (1 `(,',type ,(first args)))
                            (t `(,',vop-name
                                 (,',name ,@(subseq args 0 (floor len 2)))
                                 (,',name ,@(subseq args (floor len 2))))))))
                     `(define-compiler-macro ,name (&whole whole &rest args)
                        (let ((len (length args)))
                          (case len
                            (0 whole)
                            (1 `(,',type ,(first args)))
                            (t `(,',vop-name
                                 (,',name ,@(subseq args 0 (floor len 2)))
                                 (,',name ,@(subseq args (floor len 2)))))))))))))
  (def float4+ float4 vaddps (float4 0))
  (def float4* float4 vmulps (float4 1))
  (def float4-min float4 vminps)
  (def float4-max float4 vmaxps)
  (def float4-and float4 vandps (float4! (int4 -1)))
  (def float4-or float4 vorps (float4! (int4 0)))
  (def float4-xor float4 vxorps (float4! (int4 0)))
  (def double2+ double2 vaddpd (double2 0))
  (def double2* double2 vmulpd (double2 1))
  (def double2-min double2 vminpd)
  (def double2-max double2 vmaxpd)
  (def double2-and double2 vandpd (double2! (long2 -1)))
  (def double2-or double2 vorpd (double2! (long2 0)))
  (def double2-xor double2 vxorpd (double2! (long2 0)))
  (def int4+ int4 vpaddd (int4 0))
  (def int4* int4 vpmulld (int4 1))
  (def int4-min int4 vpminsd)
  (def int4-max int4 vpmaxsd)
  (def int4-and int4 vpand (int4 -1))
  (def int4-or int4 vpor (int4 0))
  (def int4-xor int4 vpxor (int4 0)))

;; clamp

(macrolet ((def (type)
             (let ((name (symbolicate type '#:-clamp)))
               `(definline ,name (v min max)
                  (,(symbolicate type '#:-min)
                   (,(symbolicate type '#:-max)
                    (,type v)
                    (,type min))
                   (,type max))))))

  (def float4)
  (def double2)
  (def int4))

;; shifts

(macrolet ((def (name type inst)
             (let ((vop-name (symbolicate% name)))
               `(progn
                  (defvop ,vop-name ((v ,type :target rv) (n imm5))
                      ((rv ,type))
                      ()
                    (unless (location= rv v)
                      (move rv v))
                    (inst ,inst rv n))
                  (definline ,name (v n)
                    (declare (type imm5 n))
                    (with-primitive-argument (n imm5)
                      (,vop-name (,type v) n)))))))
  (def int4-shiftl int4 pslld-imm)
  (def int4-shiftr int4 psrld-imm))

;; reducing operation

(macrolet ((def (name type inst initial)
             (let ((vop-name (symbolicate% name)))
               `(progn
                  (defvop ,vop-name ((v1 ,type :target rv) (v2 ,type))
                      ((rv ,type))
                      ()
                    (inst ,inst rv v1 v2))
                  (defun ,name (arg &rest more-args)
                    (declare (dynamic-extent more-args))
                    (if (endp more-args)
                      (,vop-name ,initial (,type arg))
                      (loop :with rv = (,type arg)
                            :for x :in more-args
                            :do (setf rv (,vop-name rv (,type x)))
                            :finally (return rv))))
                  (define-compiler-macro ,name (arg &rest more-args)
                    (if (endp more-args)
                      `(,',vop-name ,',initial (,',type ,arg))
                      (reduce (lambda (x y)
                                `(,',vop-name (,',type ,x)
                                              (,',type ,y)))
                              more-args
                              :initial-value `(,',type ,arg))))))))
  (def float4- float4 vsubps (float4 0))
  (def float4/ float4 vdivps (float4 1))
  (def double2- double2 vsubpd (double2 0))
  (def double2/ double2 vdivpd (double2 1))
  (def int4- int4 vpsubd (int4 0)))

;; horizontal functions

(defvop %float4-h+ ((v float4 :target rv))
    ((rv single))
    ()
  (inst vhaddps rv v v)
  (inst vhaddps rv rv rv))

(definline float4-h+ (v)
  (%float4-h+ (float4 v)))

(defvop %double2-h+ ((v double2 :target rv))
    ((rv double))
    ()
  (inst vhaddpd rv v v))

(definline double2-h+ (v)
  (%double2-h+ (double2 v)))

(defvop %float4-h- ((v float4 :target rv))
    ((rv single))
    ((tmp1 float4)
     (tmp2 float4))
  (inst vhaddps tmp1 v v)
  (inst vpermilps tmp1 tmp1 1)
  (inst vhsubps tmp2 v v)
  (inst vsubps rv tmp2 tmp1))

(definline float4-h- (v)
  (%float4-h- (float4 v)))

(defvop %double2-h- ((v double2 :target rv))
    ((rv double))
    ()
  (inst vhsubpd rv v v))

(definline double2-h- (v)
  (%double2-h- (double2 v)))

(defvop %int4-h+ ((v int4 :target rv))
    ((rv int4))
    ()
  (inst vphaddd rv v v)
  (inst vphaddd rv rv rv))

(definline int4-h+ (v)
  (int! (%int4-h+ (int4 v))))

(defvop %int4-h- ((v int4 :target tmp2))
    ((rv int))
    ((tmp1 int4)
     (tmp2 int4))
  (inst vphaddd tmp1 v v)
  (inst vpermilps tmp1 tmp1 1)
  (inst vphsubd tmp2 v v)
  (inst vpsubd tmp2 tmp2 tmp1)
  (inst vmovd rv tmp2))

(definline int4-h- (v)
  (%int4-h- (int4 v)))

(macrolet ((def (type op simd-op)
             (let ((name (symbolicate type '#:-h op))
                   (permute-name (symbolicate type '#:-permute))
                   (cast (symbolicate! (optype-element-type type))))
               `(definline ,name (v)
                  (let* ((v (,type v))
                         (tmp (,simd-op v (,permute-name v 1 0 3 2))))
                    (,cast (,simd-op tmp (,permute-name tmp 2 3 0 1))))))))
  (def float4 * float4*)
  (def float4 max float4-max)
  (def float4 min float4-min)
  (def float4 and float4-and)
  (def float4 or float4-or)
  (def float4 xor float4-xor)
  (def int4 * int4*)
  (def int4 max int4-max)
  (def int4 min int4-min)
  (def int4 and int4-and)
  (def int4 or int4-or)
  (def int4 xor int4-xor))

(macrolet ((def (type op simd-op)
             (let ((name (symbolicate type '#:-h op))
                   (permute-name (symbolicate type '#:-permute))
                   (cast (symbolicate! (optype-element-type type))))
               `(definline ,name (v)
                  (let ((v (,type v)))
                    (,cast (,simd-op v (,permute-name v 1 0))))))))
  (def double2 * double2*)
  (def double2 max double2-max)
  (def double2 min double2-min)
  (def double2 and double2-and)
  (def double2 or double2-or)
  (def double2 xor double2-xor))

;; FMA

(macrolet ((def (name type inst)
             (let ((vop-name (symbolicate '#:% name)))
               `(progn
                  (defvop ,vop-name ((v1 ,type :target rv)
                                     (v2 ,type)
                                     (v3 ,type))
                      ((rv ,type))
                      ((tmp ,type))
                    (cond ((location= rv v1)
                           (inst ,inst rv v2 v3))
                          ((and (or (not (tn-p v2))
                                    (not (location= rv v2)))
                                (or (not (tn-p v3))
                                    (not (location= rv v3))))
                           (move rv v1)
                           (inst ,inst rv v2 v3))
                          (t (move tmp v1)
                             (inst ,inst tmp v2 v3)
                             (move rv tmp))))
                  (definline ,name (v1 v2 v3)
                    (,vop-name (,type v1) (,type v2) (,type v3)))))))
  (def float4-fmadd float4 vfmadd213ps)
  (def float4-fnmadd float4 vfnmadd213ps)
  (def float4-fmsub float4 vfmsub213ps)
  ;; (def float4-fnmsub float4 vfnmsub213ps) ;; SBCL lacks VFNMSUB213PS
  (def float4-fmaddsub float4 vfmaddsub213ps)
  (def float4-fmsubadd float4 vfmsubadd213ps)
  (def double2-fmadd double2 vfmadd213pd)
  (def double2-fnmadd double2 vfnmadd213pd)
  (def double2-fmsub double2 vfmsub213pd)
  ;; (def double2-fnmsub float4 vfnmsub213pd) ;; SBCL lacks VFNMSUB213PD
  (def double2-fmaddsub double2 vfmaddsub213pd)
  (def double2-fmsubadd double2 vfmsubadd213pd))

;; misc two-arg

(macrolet ((def (name type inst)
             (let ((vop-name (symbolicate% name)))
               `(progn (defvop ,vop-name ((v1 ,type) (v2 ,type))
                           ((rv ,type))
                           ()
                         (inst ,inst rv v1 v2))
                       (definline ,name (v1 v2)
                         (,vop-name (,type v1) (,type v2)))))))
  (def float4-andc1 float4 vandnps)
  (def double2-andc1 double2 vandnpd)
  (def int4-andc1 int4 vpandn))

;; not
(macrolet ((def (type)
             (let ((andc1-name (symbolicate type '#:-andc1))
                   (name (symbolicate type '#:-not))
                   (cast (symbolicate! type)))
               `(definline ,name (v)
                  (,andc1-name (,type v) (,cast (int4 -1)))))))
  (def float4)
  (def double2)
  (def int4))

;;; vim: ft=lisp et