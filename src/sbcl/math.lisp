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

(defconstant +float-qnan+ (single! #x7FC00000))
(defconstant +float-infinity+ (single! #x7F800000))
;; min-norm-pos is the smallest non denormalized float number
(defconstant +float-min-norm-pos+ (single! #x00800000))
(defconstant +float-inv-mant-mask+ (single! (lognot #x7f800000)))
(defconstant +cephes-sqrthf+ +0.707106781186547524d0)
(defconstant +cephes-log-p0+ +7.0376836292d-2)
(defconstant +cephes-log-p1+ -1.1514610310d-1)
(defconstant +cephes-log-p2+ +1.1676998740d-1)
(defconstant +cephes-log-p3+ -1.2420140846d-1)
(defconstant +cephes-log-p4+ +1.4249322787d-1)
(defconstant +cephes-log-p5+ -1.6668057665d-1)
(defconstant +cephes-log-p6+ +2.0000714765d-1)
(defconstant +cephes-log-p7+ -2.4999993993d-1)
(defconstant +cephes-log-p8+ +3.3333331174d-1)
(defconstant +cephes-log-q1+ -2.12194440d-4)
(defconstant +cephes-log-q2+ +0.693359375d0)
(defconstant +exp-min+ -88.3762626647949d0)
(defconstant +exp-max+ 88.3762626647949d0)
(defconstant +cephes-log2ef+ 1.44269504088896341d0)
(defconstant +cephes-exp-c1+ 0.693359375d0)
(defconstant +cephes-exp-c2+ -2.12194440d-4)
(defconstant +cephes-exp-p0+ 1.9875691500d-4)
(defconstant +cephes-exp-p1+ 1.3981999507d-3)
(defconstant +cephes-exp-p2+ 8.3334519073d-3)
(defconstant +cephes-exp-p3+ 4.1665795894d-2)
(defconstant +cephes-exp-p4+ 1.6666665459d-1)
(defconstant +cephes-exp-p5+ 5.0000001201d-1)

(defvop %float4-sqrt ((v float4))
    ((rv float4))
    ()
  (inst vsqrtps rv v))

(definline float4-sqrt (v)
  (%float4-sqrt (float4 v)))

(defvop %float4-dot ((v1 float4) (v2 float4) (mask imm8))
    ((rv float4))
    ()
  (inst vdpps rv v1 v2 mask))

(macrolet ((def (name-prefix mask)
             `(progn (definline ,(symbolicate name-prefix '#:-dot) (v1 v2)
                       (let ((v1 (float4 v1))
                             (v2 (float4 v2)))
                         (single!
                          (%float4-dot v1 v2 ,mask))))
                     (definline ,(symbolicate name-prefix '#:-length) (v)
                       (let ((v (float4 v)))
                         (single!
                          (%float4-sqrt
                           (%float4-dot v v ,mask)))))
                     (definline ,(symbolicate name-prefix '#:-unit) (v)
                       (let* ((v (float4 v))
                              (len-sq (%float4-dot v v ,mask))
                              (len (%float4-sqrt len-sq))
                              ;; test for a division by zero
                              (zero-mask (%float4/= len (float4 0)))
                              ;; if length is infinity, set to zero
                              (inf-mask (%float4/= len-sq (float4 +float-infinity+)))
                              ;; divide for normalization
                              (result (%float4/ v len))
                              ;; set infinity components to zero
                              (result (%float4-and result zero-mask))
                              ;; select qnan or result based on infinite length
                              (tmp1 (%float4-andc1 inf-mask (float4 +float-qnan+)))
                              (tmp2 (%float4-and result inf-mask)))
                         (%float4-or tmp1 tmp2))))))
  (def float4 #xFF)
  (def float3 #x7F)
  (def float2 #x3F))

(definline float4-lerp (v1 v2 z)
  (%float4+ v1 (%float4* (float4 z)
                         (float4- v2 v2))))

(definline float4-saturate (v)
  (float4-clamp v (float4 0) (float4 1)))

(definline float4-scale (v s)
  (float4* v s))

(definline %float4-log (v)
  (declare (type float4 v))
  (let* ((e (float4 0))
         (one (float4 1))
         (invalid-mask (%float4<= v (float4 0)))
         (v (%float4-max v (float4 +float-min-norm-pos+)))
         (emm0 (%int4-shiftr (int4! v) 23)))

    ;; keep only the fractional part
    (setf v (%float4-and v (float4 +float-inv-mant-mask+))
          v (%float4-or v (float4 0.5))

          emm0 (%int4- emm0 (int4 #x7f))
          e (float4 emm0)
          e (%float4+ e one))

    ;; part2:
    ;;  if( x < SQRTHF ) {
    ;;    e -= 1;
    ;;    x = x + x - 1.0;
    ;;  } else { x = x - 1.0; }
    (let* ((mask (%float4< v (float4 +cephes-sqrthf+)))
           (tmp (%float4-and v mask))
           (z (float4 0))
           (y (float4 0)))
      (setf v (%float4- v one)
            e (%float4- e (%float4-and one mask))
            v (%float4+ v tmp)

            z (%float4* v v)
            y (float4 +cephes-log-p0+)

            y (%float4-fmadd y v (float4 +cephes-log-p1+))
            y (%float4-fmadd y v (float4 +cephes-log-p2+))
            y (%float4-fmadd y v (float4 +cephes-log-p3+))
            y (%float4-fmadd y v (float4 +cephes-log-p4+))
            y (%float4-fmadd y v (float4 +cephes-log-p5+))
            y (%float4-fmadd y v (float4 +cephes-log-p6+))
            y (%float4-fmadd y v (float4 +cephes-log-p7+))
            y (%float4-fmadd y v (float4 +cephes-log-p8+))
            y (%float4* y v)

            y (%float4* y z)

            y (%float4-fmadd e (float4 +cephes-log-q1+) y)

            y (%float4-fmadd z (float4 -0.5) y)

            v (%float4+ v y)
            v (%float4-fmadd e (float4 +cephes-log-q2+) v)
            ;; negative arg will be NAN
            v (%float4-or v invalid-mask))
      v)))

(definline float4-exp (v)
  (let* ((v (float4 v))
         (one (float4 1))
         (ps0p5 (float4 0.5))
         (v (float4-clamp v +exp-min+ +exp-max+)))
    ;; express exp(x) as exp(g + n*log(2))
    (let* ((fx (%float4-fmadd v (float4 +cephes-log2ef+) ps0p5))
           (emm0 (%float4-truncate fx))
           (tmp (float4 emm0))
           ;; if greater, subtract 1
           (mask (%float4-and (float4> tmp fx) one))
           (fx (%float4- tmp mask))
           (tmp (%float4* fx (float4 +cephes-exp-c1+)))
           (z (%float4* fx (float4 +cephes-exp-c2+)))
           (y (float4 +cephes-exp-p0+)))

      (setf v (%float4- v tmp)
            v (%float4- v z)

            z (%float4* v v)

            y (%float4-fmadd y v (float4 +cephes-exp-p1+))
            y (%float4-fmadd y v (float4 +cephes-exp-p2+))
            y (%float4-fmadd y v (float4 +cephes-exp-p3+))
            y (%float4-fmadd y v (float4 +cephes-exp-p4+))
            y (%float4-fmadd y v (float4 +cephes-exp-p5+))
            y (%float4-fmadd y z v)
            y (%float4+ y one))

      ;; build 2^n
      (setf emm0 (%float4-truncate fx)
            emm0 (%int4+ emm0 (int4 #x7f))
            emm0 (%int4-shiftl emm0 23))
      (let ((pow2n (float4! emm0)))
        (setf y (%float4* y pow2n))
        y))))

(definline float4-expt (base power)
  (float4-exp (%float4* (float4 power) (%float4-log (float4 base)))))

(definline float4-log (x &optional (base nil basep))
  (let ((x (float4 x))
        (base (if basep (float4 base) (float4 0))))
    (if basep
      (%float4/ (%float4-log x)
                (%float4-log base))
      (%float4-log x))))

;;; vim: ft=lisp et
