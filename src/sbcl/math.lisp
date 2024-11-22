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

(defconstant +float-negative-zero+ (single! #x80000000))
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
(defconstant +cephes-fopi+ 1.2732395447351628d0) ; (/ 4 pi)
(defconstant +minus-cephes-dp1+ -0.78515625d0)
(defconstant +minus-cephes-dp2+ -2.4187564849853515625d-4)
(defconstant +minus-cephes-dp3+ -3.77489497744594108d-8)
(defconstant +sincof-p0+ -1.9515295891d-4)
(defconstant +sincof-p1+  8.3321608736d-3)
(defconstant +sincof-p2+ -1.6666654611d-1)
(defconstant +coscof-p0+  2.443315711809948d-5)
(defconstant +coscof-p1+ -1.388731625493765d-3)
(defconstant +coscof-p2+  4.166664568298827d-2)
(defconstant +tancof-p0+ 9.38540185543d-3)
(defconstant +tancof-p1+ 3.11992232697d-3)
(defconstant +tancof-p2+ 2.44301354525d-2)
(defconstant +tancof-p3+ 5.34112807005d-2)
(defconstant +tancof-p4+ 1.33387994085d-1)
(defconstant +tancof-p5+ 3.33331568548d-1)
(defconstant +tancot-eps+ 1.0d-4)

(macrolet ((def (name type inst)
             (let ((vop-name (symbolicate% name)))
               `(progn (defvop ,vop-name ((v ,type))
                           ((rv ,type))
                           ()
                         (inst ,inst rv v))
                       (definline ,name (v)
                         (,vop-name (,type v)))))))
  (def float4-sqrt float4 vsqrtps)
  (def double2-sqrt double2 vsqrtpd)
  (def float4-rsqrt float4 vrsqrtps))

(definline double2-rsqrt (v)
  (let ((v (double2 v)))
    (%double2/ (double2 1) (%double2-sqrt v))))

(defvop %float4-dot ((v1 float4) (v2 float4) (mask imm8))
    ((rv float4))
    ()
  (inst vdpps rv v1 v2 mask))

(macrolet ((def (name-prefix mask result-mask)
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
                         (%float4-and (%float4-or tmp1 tmp2) ,result-mask))))))
  (def float4 #xFF (float4! (make-int4 -1 -1 -1 -1)))
  (def float3 #x7F (float4! (make-int4 -1 -1 -1 0)))
  (def float2 #x3F (float4! (make-int4 -1 -1 0 0))))

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
         (v (float4-clamp v +exp-min+ +exp-max+)))
    ;; express exp(x) as exp(g + n*log(2))
    (let* ((fx (%float4-fmadd v (float4 +cephes-log2ef+) (float4 0.5)))
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

;; The code is the exact rewriting of the cephes sinf function.
;; Precision is excellent as long as x < 8192
(definline float4-sin (v)
  (let* ((v (float4 v))
         (sign-bit (%float4-and v (float4 +float-negative-zero+)))
         (v (float4-abs v))
         (y (%float4* v (float4 +cephes-fopi+)))
         ;; j=(j+1) & (~1) (see the cephes sources)
         (emm2 (%int4-and (%int4+ (%float4-truncate y)
                                  (int4 1))
                          (int4 (lognot 1))))
         (y (float4-from-int4 emm2))
         ;; get the swap sign flag
         (swap-sign-bit
           (float4! (%int4-shiftl (%int4-and emm2 (int4 4))
                                  29)))
         ;; get the polynom selection mask
         ;; there is one polynom for 0 <= x <= pi/4
         ;; and another one for pi/4 < x <= pi/2
         ;; Both branches will be computed.
         (poly-mask (float4! (%int4= (%int4-and emm2 (int4 2))
                                     (int4 0))))
         (sign-bit (%float4-xor sign-bit swap-sign-bit))
         (z (float4 0))
         (y2 (float4 0)))
    ;; The magic pass: "Extended precision modular arithmetic "
    ;; x = ((x - y * DP1) - y * DP2) - y * DP3;
    (setf v (%float4-fmadd y (float4 +minus-cephes-dp1+) v)
          v (%float4-fmadd y (float4 +minus-cephes-dp2+) v)
          v (%float4-fmadd y (float4 +minus-cephes-dp3+) v))
    ;; Evaluate the first polynom (0 <= x <= pi/4)
    (setf y (float4 +coscof-p0+)
          z (%float4* v v)

          y (%float4-fmadd y z (float4 +coscof-p1+))
          y (%float4-fmadd y z (float4 +coscof-p2+))
          y (%float4* y z)
          y (%float4* y z)
          y (%float4-fnmadd z (float4 0.5) y)
          y (%float4+ y (float4 1)))
    ;; Evaluate the second polynom (pi/4 <= x <= 0)
    (setf y2 (float4 +sincof-p0+)
          y2 (%float4-fmadd y2 z (float4 +sincof-p1+))
          y2 (%float4-fmadd y2 z (float4 +sincof-p2+))
          y2 (%float4* y2 z)
          y2 (%float4-fmadd y2 v v))
    ;; Select the correct result from the two polynoms
    (setf y2 (%float4-and poly-mask y2)
          y (%float4-andc1 poly-mask y)
          y (%float4+ y y2))
    ;; Update the sign
    (setf y (%float4-xor y sign-bit))
    y))

;; almost the same as sin

(definline float4-cos (v)
  (let* ((v (float4 v))
         (v (float4-abs v))
         (y (%float4* v (float4 +cephes-fopi+)))
         ;; j=(j+1) & (~1) (see the cephes sources)
         (emm2 (%int4-and (%int4+ (%float4-truncate y)
                                  (int4 1))
                          (int4 (lognot 1))))
         (y (float4-from-int4 emm2))
         (emm2 (%int4- emm2 (int4 2)))
         ;; get the swap sign flag
         (emm0 (%int4-shiftl (%int4-andc1 emm2 (int4 4))
                             29))
         ;; get the polynom selection mask
         (emm2 (%int4= (%int4-and emm2 (int4 2))
                       (int4 0)))
         (sign-bit (float4! emm0))
         (poly-mask (float4! emm2))
         (z (float4 0))
         (y2 (float4 0)))
    ;; The magic pass: "Extended precision modular arithmetic "
    ;; x = ((x - y * DP1) - y * DP2) - y * DP3;
    (setf v (%float4-fmadd y (float4 +minus-cephes-dp1+) v)
          v (%float4-fmadd y (float4 +minus-cephes-dp2+) v)
          v (%float4-fmadd y (float4 +minus-cephes-dp3+) v))
    ;; Evaluate the first polynom (0 <= x <= pi/4)
    (setf y (float4 +coscof-p0+)
          z (%float4* v v)

          y (%float4-fmadd y z (float4 +coscof-p1+))
          y (%float4-fmadd y z (float4 +coscof-p2+))
          y (%float4* y z)
          y (%float4* y z)
          y (%float4-fnmadd z (float4 0.5) y)
          y (%float4+ y (float4 1)))
    ;; Evaluate the second polynom (pi/4 <= x <= 0)
    (setf y2 (float4 +sincof-p0+)
          y2 (%float4-fmadd y2 z (float4 +sincof-p1+))
          y2 (%float4-fmadd y2 z (float4 +sincof-p2+))
          y2 (%float4* y2 z)
          y2 (%float4-fmadd y2 v v))
    ;; Select the correct result from the two polynoms
    (setf y2 (%float4-and poly-mask y2)
          y (%float4-andc1 poly-mask y)
          y (%float4+ y y2))
    ;; Update the sign
    (setf y (%float4-xor y sign-bit))
    y))

;; since sin and cos are almost identical, sincos could replace both of them
;; is is almost as fast, and gives you a free cosine with your sine

(definline float4-sincos (v)
  (let* ((v (float4 v))
         (sign-bit-sin (%float4-and v (float4 +float-negative-zero+)))
         (v (float4-abs v))
         (y (%float4* v (float4 +cephes-fopi+)))
         ;; j=(j+1) & (~1) (see the cephes sources)
         (emm2 (%int4-and (%int4+ (%float4-truncate y)
                                  (int4 1))
                          (int4 (lognot 1))))
         (y (float4-from-int4 emm2))
         ;; get the swap sign flag for sine
         (swap-sign-bit-sin
           (float4! (%int4-shiftl (%int4-and emm2 (int4 4))
                                  29)))
         (poly-mask (float4! (%int4= (%int4-and emm2 (int4 2))
                                     (int4 0))))
         (sign-bit-cos
           (float4! (%int4-shiftl (%int4-andc1 (%int4- emm2 (int4 2))
                                               (int4 4))
                                  29)))
         (sign-bit-sin (%float4-xor sign-bit-sin swap-sign-bit-sin))
         (z (float4 0))
         (y2 (float4 0)))
    ;; The magic pass: "Extended precision modular arithmetic "
    ;; x = ((x - y * DP1) - y * DP2) - y * DP3;
    (setf v (%float4-fmadd y (float4 +minus-cephes-dp1+) v)
          v (%float4-fmadd y (float4 +minus-cephes-dp2+) v)
          v (%float4-fmadd y (float4 +minus-cephes-dp3+) v))
    ;; Evaluate the first polynom (0 <= x <= pi/4)
    (setf y (float4 +coscof-p0+)
          z (%float4* v v)

          y (%float4-fmadd y z (float4 +coscof-p1+))
          y (%float4-fmadd y z (float4 +coscof-p2+))
          y (%float4* y z)
          y (%float4* y z)
          y (%float4-fnmadd z (float4 0.5) y)
          y (%float4+ y (float4 1)))
    ;; Evaluate the second polynom (pi/4 <= x <= 0)
    (setf y2 (float4 +sincof-p0+)
          y2 (%float4-fmadd y2 z (float4 +sincof-p1+))
          y2 (%float4-fmadd y2 z (float4 +sincof-p2+))
          y2 (%float4* y2 z)
          y2 (%float4-fmadd y2 v v))
    (let* ((ysin1 (%float4-andc1 poly-mask y))
           (ysin2 (%float4-and poly-mask y2))
           (y2 (%float4- y2 ysin2))
           (y (%float4- y ysin1))
           (tmp1 (%float4+ ysin1 ysin2))
           (tmp2 (%float4+ y y2)))
      (values (%float4-xor tmp1 sign-bit-sin)
              (%float4-xor tmp2 sign-bit-cos)))))

(definline float4-tancot (v)
  (let* ((v (float4 v))
         (sign-bit v)
         (v (float4-abs v))
         (sign-bit (%float4-and sign-bit (float4 +float-negative-zero+)))
         (y (%float4* v (float4 +cephes-fopi+)))
         ;; j=(j+1) & (~1) (see the cephes sources)
         (emm2 (%int4-and (%int4+ (%float4-truncate y)
                                  (int4 1))
                          (int4 (lognot 1))))
         (y (float4-from-int4 emm2))
         ;; get polynom selection mask
         (poly-mask (float4! (%int4= (%int4-and emm2 (int4 2))
                                     (int4 0))))
         (z (float4 0))
         )
    ;; The magic pass: "Extended precision modular arithmetic "
    ;; x = ((x - y * DP1) - y * DP2) - y * DP3;
    (setf v (%float4-fmadd y (float4 +minus-cephes-dp1+) v)
          v (%float4-fmadd y (float4 +minus-cephes-dp2+) v)
          v (%float4-fmadd y (float4 +minus-cephes-dp3+) v))

    (setf z (%float4* v v)
          y (float4 +tancof-p0+)

          y (%float4-fmadd y z (float4 +tancof-p1+))
          y (%float4-fmadd y z (float4 +tancof-p2+))
          y (%float4-fmadd y z (float4 +tancof-p3+))
          y (%float4-fmadd y z (float4 +tancof-p4+))
          y (%float4-fmadd y z (float4 +tancof-p5+))
          y (%float4* y z)
          y (%float4-fmadd y v v))
    ;; select the correct result from the two polynoms
    (values
     (let* ((y2 (%float4-xor (%float4/ (float4 1) y)
                             (float4 +float-negative-zero+)))
            (y (%float4-and poly-mask y))
            (y2 (%float4-andc1 poly-mask y2))
            (y (%float4-or y y2)))
       ;; update the sign
       (%float4-xor y sign-bit))
     (let* ((y2 (%float4-xor y (float4 +float-negative-zero+)))
            (y (%float4-and poly-mask (%float4/ (float4 1) y)))
            (y2 (%float4-andc1 poly-mask y2))
            (y (%float4-or y y2)))
       ;; update the sign
       (%float4-xor y sign-bit)))))

(definline float4-tan (v)
  (nth-value 0 (float4-tancot v)))

(definline float4-cot (v)
  (nth-value 1 (float4-tancot v)))

;;; vim: ft=lisp et
