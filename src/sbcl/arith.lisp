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
                     `(defun ,name (&rest args)
                        (declare (dynamic-extent args))
                        (loop :with acc = ,identity
                              :for x :in args
                              :do (setf acc (,vop-name acc (,type x)))
                              :finally (return acc)))
                     `(defun ,name (arg &rest more-args)
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
  (def int4-xor int4 vpxor (int4 0))
  (def long2+ long2 vpaddq (long2 0))
  (def long2-and long2 vpand (long2 -1))
  (def long2-or long2 vpor (long2 0))
  (def long2-xor long2 vpxor (long2 0)))

(macrolet ((def (name type fn)
             (let ((vop-name (symbolicate% name))
                   (ctor-name (symbolicate '#:make- type))
                   (values-name (symbolicate type '#:-values)))
               `(progn
                  (definline ,vop-name (v1 v2)
                    (declare (type ,type v1 v2))
                    (multiple-value-bind (x1 y1) (,values-name v1)
                      (multiple-value-bind (x2 y2) (,values-name v2)
                        (,ctor-name (,fn x1 x2) (,fn y1 y2)))))
                  (defun ,name (arg &rest more-args)
                    (declare (dynamic-extent more-args))
                    (loop :with acc = (,type arg)
                          :for x :in more-args
                          :do (setf acc (,vop-name acc (,type x)))
                          :finally (return acc)))
                  (define-compiler-macro ,name (&whole whole &rest args)
                    (let ((len (length args)))
                      (case len
                        (0 whole)
                        (1 `(,',type ,(first args)))
                        (t `(,',vop-name
                             (,',name ,@(subseq args 0 (floor len 2)))
                             (,',name ,@(subseq args (floor len 2))))))))))))
  (def long2-max long2 max)
  (def long2-min long2 min))

;; Emulate VPMULLQ
(defvop %long2* ((v1 long2 :target rv) (v2 long2))
    ((rv long2))
    ((tmp1 long)
     (tmp2 long))
  (inst vpextrq tmp1 v1 0)
  (inst vpextrq tmp2 v2 0)
  (inst imul tmp1 tmp2)
  (inst vpinsrq rv rv tmp1 0)
  (inst vpextrq tmp1 v1 1)
  (inst vpextrq tmp2 v2 1)
  (inst imul tmp1 tmp2)
  (inst vpinsrq rv rv tmp1 1))

(defun long2* (&rest args)
  (loop :with acc = (long2 1)
        :for arg :in args
        :do (setf acc (%long2* (long2 arg) acc))
        :finally (return acc)))

(define-compiler-macro long2* (&rest args)
  (let ((len (length args)))
    (case len
      (0 `(long2 1))
      (1 `(long2 ,(first args)))
      (t `(%long2*
           (long2* ,@(subseq args 0 (floor len 2)))
           (long2* ,@(subseq args (floor len 2))))))))

;; incf/decf

(macrolet ((def (type)
             (let ()
               `(progn
                  (defmacro ,(symbolicate type '#:-incf) (place &optional (delta 1))
                    `(setf ,place (,',(symbolicate type '#:+) ,place ,delta)))
                  (defmacro ,(symbolicate type '#:-decf) (place &optional (delta 1))
                    `(setf ,place (,',(symbolicate type '#:-) ,place ,delta)))))))
  (def float4)
  (def int4)
  (def double2)
  (def long2))

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
  (def int4)
  (def long2))

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
  (def int4-shiftr int4 psrld-imm)
  (def long2-shiftl long2 psllq-imm)
  (def long2-shiftr long2 psrlq-imm))

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
  (def int4- int4 vpsubd (int4 0))
  (def long2- long2 vpsubq (long2 0)))

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

(macrolet ((def (name type op)
             (let ((values-name (symbolicate type '#:-values)))
               `(definline ,name (v)
                  (let ((v (,type v)))
                    (multiple-value-bind (x y) (,values-name v)
                      (,op x y)))))))
  (def long2-h+ long2 +)
  (def long2-h- long2 -)
  (def long2-h* long2 *)
  (def long2-hmax long2 max)
  (def long2-hmin long2 min)
  (def long2-hand long2 logand)
  (def long2-hor long2 logior)
  (def long2-hxor long2 logxor))

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
  (def int4-andc1 int4 vpandn)
  (def long2-andc1 long2 vpandn))

;; not
(macrolet ((def (type)
             (let ((andc1-name (symbolicate type '#:-andc1))
                   (name (symbolicate type '#:-not))
                   (cast (symbolicate! type)))
               `(definline ,name (v)
                  (,andc1-name (,type v) (,cast (int4 -1)))))))
  (def float4)
  (def double2)
  (def int4)
  (def long2))

;; comparisons

(macrolet ((def (type op inst cmp)
             (let ((name (symbolicate% type op)))
               `(defvop ,name ((v1 ,type :target rv) (v2 ,type))
                    ((rv ,type))
                    ()
                  (inst ,inst ,cmp rv v1 v2)))))
  (def float4 = vcmpps :eq)
  (def float4 < vcmpps :lt)
  (def float4 <= vcmpps :le)
  (def float4 > vcmpps :gt)
  (def float4 >= vcmpps :ge)
  (def float4 /= vcmpps :neq)
  (def double2 = vcmppd :eq)
  (def double2 < vcmppd :lt)
  (def double2 <= vcmppd :le)
  (def double2 > vcmppd :gt)
  (def double2 >= vcmppd :ge)
  (def double2 /= vcmppd :neq))

(macrolet ((def (type op inst)
             (let ((name (symbolicate% type op)))
               `(defvop ,name ((v1 ,type :target rv) (v2 ,type))
                    ((rv ,type))
                    ()
                  (inst ,inst rv v1 v2)))))
  (def int4 = vpcmpeqd)
  (def int4 > vpcmpgtd)
  (def long2 = vpcmpeqq)
  (def long2 > vpcmpgtq))

(macrolet ((def (type op reverse-op)
             (let ((name (symbolicate% type op))
                   (not-name (symbolicate type '#:-not))
                   (reverse-name (symbolicate% type reverse-op)))
               `(definline ,name (v1 v2)
                  (declare (type ,type v1 v2))
                  (,not-name (,reverse-name v1 v2))))))
  (def int4 <= >)
  (def int4 /= =)
  (def long2 <= >)
  (def long2 /= =))

(macrolet ((def (type)
             (let ()
               `(progn
                  (definline ,(symbolicate% type '>=) (v1 v2)
                    (declare (type ,type v1 v2))
                    (,(symbolicate type '#:-or)
                     (,(symbolicate% type '>) v1 v2)
                     (,(symbolicate% type '=) v1 v2)))
                  (definline ,(symbolicate% type '<) (v1 v2)
                    (declare (type ,type v1 v2))
                    (,(symbolicate type '#:-not)
                     (,(symbolicate type '#:-or)
                      (,(symbolicate% type '>) v1 v2)
                      (,(symbolicate% type '=) v1 v2))))))))
  (def int4)
  (def long2))

(macrolet ((def (type op true andfn)
             (let* ((name (symbolicate type op))
                    (vop-name (symbolicate% name)))
               `(progn
                  (defun ,name (arg &rest more-args)
                    (declare (dynamic-extent more-args))
                    (if (endp more-args)
                      ,true
                      (loop :with a = (,type arg)
                            :with b = (,type (first more-args))
                            :with result = (,vop-name a b)
                            :for arg :in (rest more-args)
                            :do (shiftf a b (,type arg))
                                (setf result (,andfn result (,vop-name a b)))
                            :finally (return result))))
                  (define-compiler-macro ,name (arg &rest more-args)
                    (if (endp more-args)
                      ,true
                      (let ((bindings
                              (loop :for arg :in (list* arg more-args)
                                    :collect `(,(gensym (string '#:arg))
                                               (,',type ,arg)))))
                        `(let ,bindings
                           (,',andfn ,@(loop :for ((a nil) (b nil) . rest) :on bindings
                                             :collect `(,',vop-name ,a ,b)
                                             :until (endp rest)))))))))))
  (def float4 = (float4! (int4 -1)) float4-and)
  (def float4 < (float4! (int4 -1)) float4-and)
  (def float4 <= (float4! (int4 -1)) float4-and)
  (def float4 > (float4! (int4 -1)) float4-and)
  (def float4 >= (float4! (int4 -1)) float4-and)
  (def double2 = (double2! (int4 -1)) float4-and)
  (def double2 < (double2! (int4 -1)) float4-and)
  (def double2 <= (double2! (int4 -1)) float4-and)
  (def double2 > (double2! (int4 -1)) float4-and)
  (def double2 >= (double2! (int4 -1)) float4-and)
  (def int4 = (int4 -1) int4-and)
  (def int4 < (int4 -1) int4-and)
  (def int4 <= (int4 -1) int4-and)
  (def int4 > (int4 -1) int4-and)
  (def int4 >= (int4 -1) int4-and)
  (def long2 = (long2 -1) long2-and)
  (def long2 < (long2 -1) long2-and)
  (def long2 <= (long2 -1) long2-and)
  (def long2 > (long2 -1) long2-and)
  (def long2 >= (long2 -1) long2-and))

(macrolet ((def (type true andfn)
             (let* ((name (symbolicate type '/=))
                    (vop-name (symbolicate% name)))
               `(progn
                  (defun ,name (arg &rest more-args)
                    (declare (dynamic-extent more-args))
                    (loop :with args = (list* (,type arg) (mapcar ',type more-args))
                          :with result = ,true
                          :for (a . rest) :on args :do
                            (loop :for b :in rest :do
                              (setf result (,andfn result (,vop-name a b))))
                          :finally (return result)))
                  (define-compiler-macro ,name (arg &rest more-args)
                    (if (endp more-args)
                      ,true
                      (let ((bindings (loop :for arg :in (list* arg more-args)
                                            :collect `(,(gensym (string '#:arg))
                                                       (,',type ,arg)))))
                        `(let ,bindings
                           (,',andfn
                            ,@(loop :for ((a nil) . rest) :on bindings
                                    :append
                                    (loop :for (b nil) :in rest
                                          :collect `(,',vop-name ,a ,b))))))))))))
  (def float4 (float4! (int4 -1)) float4-and)
  (def double2 (double2! (int4 -1)) double2-and)
  (def int4 (int4 -1) int4-and)
  (def long2 (long2 -1) long2-and))

(defvop %float4-truncate ((v float4 :target rv))
    ((rv int4))
    ()
  (inst vcvttps2dq rv v))

(definline float4-truncate (v)
  (%float4-truncate (float4 v)))

(defvop %float4-round ((v float4 :target rv) (rounding imm2))
    ((rv int4))
    ((tmp float4))
  (inst vroundps tmp v rounding)
  (inst vcvtps2dq rv tmp))

(macrolet ((def (name mask)
             `(definline ,name (v)
                (%float4-round (float4 v) ,mask))))
  (def float4-round #x0)
  (def float4-floor #x1)
  (def float4-ceiling #x2))

(defvop %double2-round ((v double2 :target rv) (rounding imm2))
    ((rv double2))
    ()
  (inst vroundpd rv v rounding))

(macrolet ((def (name mask)
             `(definline ,name (v)
                (long2-from-double2 (%double2-round (double2 v) ,mask)))))
  (def double2-round #x0)
  (def double2-floor #x1)
  (def double2-ceiling #x2)
  (def double2-truncate #x3))

(macrolet ((def (type)
             `(definline ,(symbolicate type '#:-abs) (v)
                (let ((v (,type v)))
                  (,(symbolicate% type '#:-max)
                   v
                   (,(symbolicate% type '-) (,type 0) v))))))
  (def float4)
  (def double2)
  (def long2))

(macrolet ((def (type inst)
             (let* ((name (symbolicate type '#:-abs))
                    (vop-name (symbolicate% name)))
               `(progn
                  (defvop ,vop-name ((v ,type))
                      ((rv ,type))
                      ()
                    (inst ,inst rv v))
                  (definline ,name (v)
                    (,vop-name (,type v)))))))
  (def int4 vpabsd))

;;; vim: ft=lisp et
