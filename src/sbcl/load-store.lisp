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

;; helper vops

(defvop (memory-fence :wrapper t :pure nil) ()
    ()
    ()
  (inst mfence))

(defvop (load-fence :wrapper t :pure nil) ()
    ()
    ()
  (inst lfence))

(defvop (store-fence :wrapper t :pure nil) ()
    ()
    ()
  (inst sfence))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun vector-ea (type base index &optional constp)
    (let* ((vector-data-offset (+ (* sb-vm:vector-data-offset
                                     sb-vm:n-word-bytes)
                                  (- sb-vm:other-pointer-lowtag)))
           (eltype (optype-element-type type))
           (eltype-width-in-bytes (floor (optype-width eltype) 8)))
      (if constp
        (ea (+ vector-data-offset
               (* eltype-width-in-bytes index))
            base)
        (ea vector-data-offset
            base index (optype-index-scale type index)))))
  (defun vector-ea-form (type index-var &optional constp)
    (let* ((vector-data-offset (+ (* sb-vm:vector-data-offset
                                     sb-vm:n-word-bytes)
                                  (- sb-vm:other-pointer-lowtag)))
           (eltype (optype-element-type type))
           (eltype-width-in-bytes (floor (optype-width eltype) 8))
           (index-scale (optype-index-scale-form type index-var)))
      (if constp
        `(ea (+ ,vector-data-offset
                (* ,eltype-width-in-bytes ,index-var))
             v)
        `(ea ,vector-data-offset
             v ,index-var ,index-scale)))))

(macrolet ((defload (type inst &optional non-temporal-p)
             (let* ((prefix (if non-temporal-p '#:-non-temporal ""))
                    (name (symbolicate type prefix '#:-load))
                    (vop-name (symbolicate% name))
                    (vectype (optype-vector-type type)))
               `(progn
                  (defvop (,vop-name :pure nil :cost 2)
                      ((v ,vectype) (i index))
                      ((rv ,type))
                      ()
                    (inst ,inst rv ,(vector-ea-form type 'i)))
                  (defvop (,(symbolicate vop-name '#:-const)
                           :pure nil
                           :translate ,vop-name
                           :cost 1)
                      ((v ,vectype) (i low-index))
                      ((rv ,type))
                      ()
                    (inst ,inst rv ,(vector-ea-form type 'i t))))))
           (defmemref (type inst &optional non-temporal-p)
             (let* ((prefix (if non-temporal-p '#:-non-temporal ""))
                    (name (symbolicate type prefix '#:-mem-ref))
                    (vop-name (symbolicate% name)))
               `(progn
                  (defvop (,vop-name :pure nil :cost 2)
                      ((p pointer) (i index))
                      ((rv ,type))
                      ()
                    (inst ,inst rv (ea p i)))
                  (defvop (,(symbolicate vop-name '#:-const)
                           :pure nil
                           :translate ,vop-name
                           :cost 1)
                      ((p pointer) (i low-index))
                      ((rv ,type))
                      ()
                    (inst ,inst rv (ea i p))))))
           (defmemaref (type inst &optional non-temporal-p)
             (let* ((prefix (if non-temporal-p '#:-non-temporal ""))
                    (name (symbolicate type prefix '#:-mem-aref))
                    (vop-name (symbolicate% name))
                    (eltype (optype-element-type type))
                    (eltype-width-in-bytes (floor (optype-width eltype) 8)))
               `(progn
                  (defvop (,vop-name :pure nil :cost 2)
                      ((p pointer) (i index))
                      ((rv ,type))
                      ()
                    (inst ,inst rv (ea p i ,(optype-index-scale-form type 'i))))
                  (defvop (,(symbolicate vop-name '#:-const)
                           :pure nil
                           :translate ,vop-name
                           :cost 1)
                      ((p pointer) (i low-index))
                      ((rv ,type))
                      ()
                    (inst ,inst rv (ea (* ,eltype-width-in-bytes i) p))))))
           (defstore (type inst &optional non-temporal-p)
             (let* ((prefix (if non-temporal-p '#:-non-temporal ""))
                    (name (symbolicate type prefix '#:-store))
                    (vop-name (symbolicate% name))
                    (vectype (optype-vector-type type)))
               `(progn
                  (defvop (,vop-name :pure nil :cost 2)
                      ((x ,type :target rv) (v ,vectype) (i index))
                      ((rv ,type))
                      ()
                    (move rv x)
                    (inst ,inst ,(vector-ea-form type 'i) rv))
                  (defvop (,(symbolicate vop-name '#:-const)
                           :pure nil
                           :translate ,vop-name
                           :cost 1)
                      ((x ,type :target rv) (v ,vectype) (i low-index))
                      ((rv ,type))
                      ()
                    (move rv x)
                    (inst ,inst ,(vector-ea-form type 'i t) rv)))))
           (defmemset (type inst &optional non-temporal-p)
             (let* ((prefix (if non-temporal-p '#:-non-temporal ""))
                    (vop-name (symbolicate% type prefix '#:-mem-set)))
               `(progn
                  (defvop (,vop-name :pure nil :cost 2)
                      ((x ,type :target rv) (p pointer) (i index))
                      ((rv ,type))
                      ()
                    (move rv x)
                    (inst ,inst (ea p i) x))
                  (defvop (,(symbolicate vop-name '#:-const)
                           :pure nil
                           :translate ,vop-name
                           :cost 1)
                      ((x ,type) (p pointer) (i low-index))
                      ((rv ,type))
                      ()
                    (move rv x)
                    (inst ,inst (ea i p) x)))))
           (defmemaset (type inst &optional non-temporal-p)
             (let* ((prefix (if non-temporal-p '#:-non-temporal ""))
                    (vop-name (symbolicate% type prefix '#:-mem-aset))
                    (eltype (optype-element-type type))
                    (eltype-width-in-bytes (floor (optype-width eltype) 8)))
               `(progn
                  (defvop (,vop-name :pure nil :cost 2)
                      ((x ,type :target rv) (p pointer) (i index))
                      ((rv ,type))
                      ()
                    (move rv x)
                    (inst ,inst (ea p i ,(optype-index-scale-form type 'i)) x))
                  (defvop (,(symbolicate vop-name '#:-const)
                           :pure nil
                           :translate ,vop-name
                           :cost 1)
                      ((x ,type) (p pointer) (i low-index))
                      ((rv ,type))
                      ()
                    (move rv x)
                    (inst ,inst (ea (* ,eltype-width-in-bytes i) p) x)))))
           (def (type inst ntload-inst ntstore-inst)
             `(progn
                ;; Load
                (defload ,type ,inst)

                ;; mem-ref
                (defmemref ,type ,inst)

                ;; mem-aref
                (defmemaref ,type ,inst)

                ;; Store
                (defstore ,type ,inst)

                ;; (setf mem-ref)
                (defmemset ,type ,inst)

                ;; (setf mem-aref)
                (defmemaset ,type ,inst)

                ;; NT load
                (defload ,type ,ntload-inst t)

                ;; NT mem-ref
                (defmemref ,type ,ntload-inst t)

                ;; NT mem-aref
                (defmemaref ,type ,ntload-inst t)

                ;;  NT store
                (defstore ,type ,ntstore-inst t)

                ;; NT (setf mem-ref)
                (defmemset ,type ,ntstore-inst t)

                ;; NT (setf mem-aref)
                (defmemaset ,type ,ntstore-inst t))))
  (def float4 vmovups vmovntdqa vmovntps)
  (def float8 vmovups vmovntdqa vmovntps)
  (def double2 vmovupd vmovntdqa vmovntpd)
  (def double4 vmovupd vmovntdqa vmovntpd)
  (def sbyte16 vmovdqu vmovntdqa vmovntdq)
  (def sbyte32 vmovdqu vmovntdqa vmovntdq)
  (def ubyte16 vmovdqu vmovntdqa vmovntdq)
  (def ubyte32 vmovdqu vmovntdqa vmovntdq)
  (def short8 vmovdqu vmovntdqa vmovntdq)
  (def short16 vmovdqu vmovntdqa vmovntdq)
  (def ushort8 vmovdqu vmovntdqa vmovntdq)
  (def ushort16 vmovdqu vmovntdqa vmovntdq)
  (def int4 vmovdqu vmovntdqa vmovntdq)
  (def int8 vmovdqu vmovntdqa vmovntdq)
  (def uint4 vmovdqu vmovntdqa vmovntdq)
  (def uint8 vmovdqu vmovntdqa vmovntdq)
  (def long2 vmovdqu vmovntdqa vmovntdq)
  (def long4 vmovdqu vmovntdqa vmovntdq)
  (def ulong2 vmovdqu vmovntdqa vmovntdq)
  (def ulong4 vmovdqu vmovntdqa vmovntdq))

(macrolet ((defload (type &optional non-temporal-p)
             (let* ((prefix (if non-temporal-p '#:-non-temporal ""))
                    (simd-width (optype-simd-width type))
                    (eltype (optype-element-type type))
                    (name (symbolicate type prefix '#:-load))
                    (vop-name (symbolicate% name))
                    (aref-name (symbolicate type prefix '#:-aref)))
               `(progn
                  (defun ,name (array index)
                    (declare (type (array ,eltype) array)
                             (type index index))
                    (with-bounds-check (array index ,simd-width)
                      (,vop-name array index)))
                  (define-compiler-macro ,name (array index)
                    (once-only (array index)
                      `(with-bounds-check (,array ,index ,',simd-width)
                         (,',vop-name ,array ,index))))
                  (definline ,(symbolicate type prefix '#:-row-major-aref)
                      (array index)
                    (declare (type (array ,eltype) array)
                             (type index index))
                    (,name array (* index ,simd-width)))
                  (defun ,aref-name (array &rest subscripts)
                    (declare (type (array ,eltype) array))
                    (,name array (apply #'array-row-major-simd-index
                                        array
                                        ,simd-width
                                        subscripts)))
                  (define-compiler-macro ,aref-name (array &rest subscripts)
                    (once-only (array)
                      (with-gensyms (index)
                        (let* ((subscript-bindings
                                 (loop :for subscript :in subscripts
                                       :collect `(,(gensym (string '#:i)) ,subscript)))
                               (subscripts (mapcar #'first subscript-bindings)))
                          `(let ,subscript-bindings
                             (with-row-major-simd-index
                                 (,index ,array ,',simd-width ,@subscripts)
                               (,',name ,array ,index))))))))))
           (defmemref (type &optional non-temporal-p)
             (let* ((prefix (if non-temporal-p '#:-non-temporal ""))
                    (name (symbolicate type prefix '#:-mem-ref))
                    (vop-name (symbolicate% name)))
               `(definline ,name (pointer &optional (offset 0))
                  (declare (type pointer pointer)
                           (type index offset))
                  (,vop-name pointer offset))))
           (defmemaref (type &optional non-temporal-p)
             (let* ((prefix (if non-temporal-p '#:-non-temporal ""))
                    (name (symbolicate type prefix '#:-mem-aref))
                    (vop-name (symbolicate% name))
                    (simd-width (optype-simd-width type)))
               `(definline ,name (pointer &optional (index 0))
                  (declare (type pointer pointer)
                           (type index index))
                  (,vop-name pointer (* index ,simd-width)))))
           (defstore (type &optional non-temporal-p)
             (let* ((prefix (if non-temporal-p '#:-non-temporal ""))
                    (name (symbolicate type prefix '#:-store))
                    (vop-name (symbolicate% name))
                    (eltype (optype-element-type type))
                    (simd-width (optype-simd-width type))
                    (aref-name (symbolicate type prefix '#:-aref)))
               `(progn
                  (defun ,name (new-value array index)
                    (declare (type (array ,eltype) array)
                             (type index index))
                    (let ((new-value (,type new-value)))
                      (with-bounds-check (array index ,simd-width)
                        (,vop-name new-value array index))))
                  (define-compiler-macro ,name (new-value array index)
                    (once-only (array index)
                      (with-gensyms (value)
                        `(let ((,value (,',type ,new-value)))
                           (with-bounds-check (,array ,index ,',simd-width)
                             (,',vop-name ,value ,array ,index))))))
                  (definline (setf ,(symbolicate type prefix '#:-row-major-aref))
                      (new-value array index)
                    (declare (type (array ,eltype) array)
                             (type index index))
                    (,name new-value array (* index ,simd-width)))
                  (defun (setf ,aref-name) (new-value array &rest subscripts)
                    (declare (type (array ,eltype) array))
                    (,name new-value array (apply #'array-row-major-simd-index
                                                  array
                                                  ,simd-width
                                                  subscripts)))
                  (define-compiler-macro (setf ,aref-name) (new-value array &rest subscripts)
                    (once-only (array)
                      (with-gensyms (index)
                        (let* ((subscript-bindings
                                 (loop :for subscript :in subscripts
                                       :collect `(,(gensym (string '#:i)) ,subscript)))
                               (subscripts (mapcar #'first subscript-bindings)))
                          `(let ,subscript-bindings
                             (with-row-major-simd-index
                                 (,index ,array ,',simd-width ,@subscripts)
                               (,',name ,new-value ,array ,index))))))))))
           (defmemset (type &optional non-temporal-p)
             (let* ((prefix (if non-temporal-p '#:-non-temporal ""))
                    (name (symbolicate type prefix '#:-mem-ref))
                    (vop-name (symbolicate% type prefix '#:-mem-set)))
               `(definline (setf ,name) (new-value pointer &optional (offset 0))
                  (declare (type pointer pointer)
                           (type index offset))
                  (,vop-name (,type new-value) pointer offset))))
           (defmemaset (type &optional non-temporal-p)
             (let* ((prefix (if non-temporal-p '#:-non-temporal ""))
                    (name (symbolicate type prefix '#:-mem-aref))
                    (vop-name (symbolicate% type prefix '#:-mem-aset))
                    (simd-width (optype-simd-width type)))
               `(definline (setf ,name) (new-value pointer &optional (index 0))
                  (declare (type pointer pointer)
                           (type index index))
                  (,vop-name (,type new-value) pointer (* index ,simd-width)))))
           (def (type)
             `(progn
                ;; Load
                (defload ,type)

                ;; mem-ref
                (defmemref ,type)

                ;; mem-aref
                (defmemaref ,type)

                ;; Store
                (defstore ,type)

                ;; (setf mem-ref)
                (defmemset ,type)

                ;; (setf mem-aref)
                (defmemaset ,type)

                ;; NT load
                (defload ,type t)

                ;; NT mem-ref
                (defmemref ,type t)

                ;; NT mem-aref
                (defmemaref ,type t)

                ;;  NT store
                (defstore ,type t)

                ;; NT (setf mem-ref)
                (defmemset ,type t)

                ;; NT (setf mem-aref)
                (defmemaset ,type t))))
  (def float4)
  (def float8)
  (def double2)
  (def double4)
  (def sbyte16)
  (def sbyte32)
  (def ubyte16)
  (def ubyte32)
  (def short8)
  (def short16)
  (def ushort8)
  (def ushort16)
  (def int4)
  (def int8)
  (def uint4)
  (def uint8)
  (def long2)
  (def long4)
  (def ulong2)
  (def ulong4))

(defvop (%float3-load :pure nil
                      :cost 2)
    ((v float-vector)
     (i index))
    ((rv float4))
    ((mask float4)
     (zero long))
  (inst vpcmpeqd mask mask mask)
  (inst xor zero zero)
  (inst vpinsrd mask mask zero 3)
  (inst vmaskmovps rv mask (vector-ea 'float4 v i)))

(defvop (%float3-load-const :pure nil
                            :cost 1
                            :translate %float3-load)
    ((v float-vector)
     (i low-index))
    ((rv float4))
    ((mask float4)
     (zero long))
  (inst vpcmpeqd mask mask mask)
  (inst xor zero zero)
  (inst vpinsrd mask mask zero 3)
  (inst vmaskmovps rv mask (vector-ea 'float4 v i t)))

(defvop (%float3-mem-ref :pure nil
                         :cost 2)
    ((p pointer)
     (i index))
    ((rv float4))
    ((mask float4)
     (zero long))
  (inst vpcmpeqd mask mask mask)
  (inst xor zero zero)
  (inst vpinsrd mask mask zero 3)
  (inst vmaskmovps rv mask (ea p i)))

(defvop (%float3-mem-ref-const :pure nil
                               :cost 1
                               :translate %float3-mem-ref)
    ((p pointer)
     (i low-index))
    ((rv float4))
    ((mask float4)
     (zero long))
  (inst vpcmpeqd mask mask mask)
  (inst xor zero zero)
  (inst vpinsrd mask mask zero 3)
  (inst vmaskmovps rv mask (ea i p)))

(defvop (%float3-mem-aref :pure nil
                          :cost 2)
    ((p pointer)
     (i index))
    ((rv float4))
    ((mask float4)
     (zero long))
  (inst vpcmpeqd mask mask mask)
  (inst xor zero zero)
  (inst vpinsrd mask mask zero 3)
  (inst vmaskmovps rv mask (ea p i (optype-index-scale 'float4 i))))

(defvop (%float3-mem-aref-const :pure nil
                                :cost 1
                                :translate %float3-mem-aref)
    ((p pointer)
     (i low-index))
    ((rv float4))
    ((mask float4)
     (zero long))
  (inst vpcmpeqd mask mask mask)
  (inst xor zero zero)
  (inst vpinsrd mask mask zero 3)
  (inst vmaskmovps rv mask (ea (* i (floor (optype-width 'single) 8))
                               p)))

(defvop (%float3-store :pure nil
                       :cost 2)
    ((x float4 :target rv)
     (v float-vector)
     (i index))
    ((rv float4))
    ((mask float4)
     (zero long))
  (move rv x)
  (inst vpcmpeqd mask mask mask)
  (inst xor zero zero)
  (inst vpinsrd mask mask zero 3)
  (inst vmaskmovps (vector-ea 'float4 v i) mask rv))

(defvop (%float3-store-const :pure nil
                             :cost 1
                             :translate %float3-store)
    ((x float4 :target rv)
     (v float-vector)
     (i low-index))
    ((rv float4))
    ((mask float4)
     (zero long))
  (move rv x)
  (inst vpcmpeqd mask mask mask)
  (inst xor zero zero)
  (inst vpinsrd mask mask zero 3)
  (inst vmaskmovps (vector-ea 'float4 v i t) mask rv))

(defvop (%float3-mem-set :pure nil
                         :cost 2)
    ((x float4 :target rv)
     (p pointer)
     (i index))
    ((rv float4))
    ((mask float4)
     (zero long))
  (move rv x)
  (inst vpcmpeqd mask mask mask)
  (inst xor zero zero)
  (inst vpinsrd mask mask zero 3)
  (inst vmaskmovps (ea p i) mask rv))

(defvop (%float3-mem-set-const :pure nil
                               :cost 1
                               :translate %float3-mem-set)
    ((x float4 :target rv)
     (p pointer)
     (i low-index))
    ((rv float4))
    ((mask float4)
     (zero long))
  (move rv x)
  (inst vpcmpeqd mask mask mask)
  (inst xor zero zero)
  (inst vpinsrd mask mask zero 3)
  (inst vmaskmovps (ea i p) mask rv))

(defvop (%float3-mem-aset :pure nil
                          :cost 2)
    ((x float4 :target rv)
     (p pointer)
     (i index))
    ((rv float4))
    ((mask float4)
     (zero long))
  (move rv x)
  (inst vpcmpeqd mask mask mask)
  (inst xor zero zero)
  (inst vpinsrd mask mask zero 3)
  (inst vmaskmovps (ea p i (optype-index-scale 'float4 i)) mask rv))

(defvop (%float3-mem-aset-const :pure nil
                                :cost 1
                                :translate %float3-mem-aset)
    ((x float4 :target rv)
     (p pointer)
     (i low-index))
    ((rv float4))
    ((mask float4)
     (zero long))
  (move rv x)
  (inst vpcmpeqd mask mask mask)
  (inst xor zero zero)
  (inst vpinsrd mask mask zero 3)
  (inst vmaskmovps (ea (* i (floor (optype-width 'single) 8)) p) mask rv))

(defun float3-load (array index)
  (declare (type (array single) array)
           (type index index))
  (with-bounds-check (array index 3)
    (%float3-load array index)))

(define-compiler-macro float3-load (array index)
  (once-only (array index)
    `(with-bounds-check (,array ,index 3)
       (%float3-load ,array ,index))))

(definline float3-row-major-aref (array index)
  (declare (type (array single) array)
           (type index index))
  (float3-load array (* index 3)))

(defun float3-aref (array &rest subscripts)
  (declare (type (array single) array))
  (float3-load array (apply #'array-row-major-simd-index
                            array
                            3
                            subscripts)))

(define-compiler-macro float3-aref (array &rest subscripts)
  (once-only (array)
    (with-gensyms (index)
      (let* ((subscript-bindings
               (loop :for subscript :in subscripts
                     :collect `(,(gensym (string '#:i)) ,subscript)))
             (subscripts (mapcar #'first subscript-bindings)))
        `(let ,subscript-bindings
           (with-row-major-simd-index
               (,index ,array 3 ,@subscripts)
             (float3-load ,array ,index)))))))

(definline float3-mem-ref (pointer &optional (offset 0))
  (declare (type pointer pointer)
           (type index offset))
  (%float3-mem-ref pointer offset))

(definline float3-mem-aref (pointer &optional (index 0))
  (declare (type pointer pointer)
           (type index index))
  (%float3-mem-aref pointer (* index 3)))

(defun float3-store (new-value array index)
  (declare (type (array single) array)
           (type index index))
  (let ((new-value (float4 new-value)))
    (with-bounds-check (array index 3)
      (%float3-store new-value array index))))

(define-compiler-macro float3-store (new-value array index)
  (once-only (array index)
    (with-gensyms (value)
      `(let ((,value (float4 ,new-value)))
         (with-bounds-check (,array ,index 3)
           (%float3-store ,value ,array ,index))))))

(definline (setf float3-row-major-aref) (new-value array index)
  (declare (type (array single) array)
           (type index index))
  (float3-store new-value array (* index 3)))

(defun (setf float3-aref) (new-value array &rest subscripts)
  (declare (type (array single) array))
  (float3-store new-value array (apply #'array-row-major-simd-index
                                       array
                                       3
                                       subscripts)))

(define-compiler-macro (setf float3-aref) (new-value array &rest subscripts)
  (once-only (array)
    (with-gensyms (index)
      (let* ((subscript-bindings
               (loop :for subscript :in subscripts
                     :collect `(,(gensym (string '#:i)) ,subscript)))
             (subscripts (mapcar #'first subscript-bindings)))
        `(let ,subscript-bindings
           (with-row-major-simd-index
               (,index ,array 3 ,@subscripts)
             (float3-store ,new-value ,array ,index)))))))

(definline (setf float3-mem-ref) (new-value pointer &optional (offset 0))
  (declare (type pointer pointer)
           (type index offset))
  (let ((new-value (float4 new-value)))
    (%float3-mem-set new-value pointer offset)))

(definline (setf float3-mem-aref) (new-value pointer &optional (index 0))
  (declare (type pointer pointer)
           (type index index))
  (let ((new-value (float4 new-value)))
    (%float3-mem-aset new-value pointer (* index 3))))

(defvop (%float2-load :pure nil
                      :cost 2)
    ((v float-vector)
     (i index))
    ((rv float4))
    ()
  (inst vmovsd rv (vector-ea 'single v i)))

(defvop (%float2-load-const :pure nil
                            :translate %float2-load
                            :cost 1)
    ((v float-vector)
     (i low-index))
    ((rv float4))
    ()
  (inst vmovsd rv (vector-ea 'single v i t)))

(defvop (%float2-mem-ref :pure nil
                         :cost 2)
    ((p pointer)
     (i index))
    ((rv float4))
    ()
  (inst vmovsd rv (ea p i)))

(defvop (%float2-mem-ref-const :pure nil
                               :cost 1
                               :translate %float2-mem-ref)
    ((p pointer)
     (i low-index))
    ((rv float4))
    ()
  (inst vmovsd rv (ea i p)))

(defvop (%float2-mem-aref :pure nil
                          :cost 2)
    ((p pointer)
     (i index))
    ((rv float4))
    ()
  (inst vmovsd rv (ea p i (optype-index-scale 'double i))))

(defvop (%float2-mem-aref-const :pure nil
                                :cost 1
                                :translate %float2-mem-aref)
    ((p pointer)
     (i low-index))
    ((rv float4))
    ()
  (inst vmovsd rv (ea (* i (floor (optype-width 'double) 8)) p)))

(defvop (%float2-store :pure nil
                       :cost 2)
    ((x float4 :target rv)
     (v float-vector)
     (i index))
    ((rv float4))
    ()
  (move rv x)
  (inst vmovsd (vector-ea 'single v i) rv))

(defvop (%float2-store-const :pure nil
                             :translate %float2-store
                             :cost 1)
    ((x float4 :target rv)
     (v float-vector)
     (i low-index))
    ((rv float4))
    ()
  (move rv x)
  (inst vmovsd (vector-ea 'single v i t) rv))

(defvop (%float2-mem-set :pure nil
                         :cost 2)
    ((x float4 :target rv)
     (p pointer)
     (i index))
    ((rv float4))
    ()
  (move rv x)
  (inst vmovsd (ea p i) rv))

(defvop (%float2-mem-set-const :pure nil
                               :cost 1
                               :translate %float2-mem-set)
    ((x float4 :target rv)
     (p pointer)
     (i low-index))
    ((rv float4))
    ()
  (move rv x)
  (inst vmovsd (ea i p) rv))

(defvop (%float2-mem-aset :pure nil
                          :cost 2)
    ((x float4 :target rv)
     (p pointer)
     (i index))
    ((rv float4))
    ()
  (move rv x)
  (inst vmovsd (ea p i (optype-index-scale 'double i)) rv))

(defvop (%float2-mem-aset-const :pure nil
                                :cost 1
                                :translate %float2-mem-aset)
    ((x float4 :target rv)
     (p pointer)
     (i low-index))
    ((rv float4))
    ()
  (move rv x)
  (inst vmovsd (ea (* i (floor (optype-width 'double) 8)) p) rv))

(defun float2-load (array index)
  (declare (type (array single) array)
           (type index index))
  (with-bounds-check (array index 2)
    (%float2-load array index)))

(define-compiler-macro float2-load (array index)
  (once-only (array index)
    `(with-bounds-check (,array ,index 2)
       (%float2-load ,array ,index))))

(definline float2-row-major-aref (array index)
  (declare (type (array single) array))
  (float2-load array (* index 2)))

(defun float2-aref (array &rest subscripts)
  (declare (type (array single) array))
  (float2-load array (apply #'array-row-major-simd-index
                            array
                            2
                            subscripts)))

(define-compiler-macro float2-aref (array &rest subscripts)
  (once-only (array)
    (with-gensyms (index)
      (let* ((subscript-bindings
               (loop :for subscript :in subscripts
                     :collect `(,(gensym (string '#:i)) ,subscript)))
             (subscripts (mapcar #'first subscript-bindings)))
        `(let ,subscript-bindings
           (with-row-major-simd-index
               (,index ,array 2 ,@subscripts)
             (let ((,index ,index))
               (float2-load ,array ,index))))))))

(definline float2-mem-ref (pointer &optional (offset 0))
  (declare (type pointer pointer)
           (type index offset))
  (%float2-mem-ref pointer offset))

(definline float2-mem-aref (pointer &optional (index 0))
  (declare (type pointer pointer)
           (type index index))
  (%float2-mem-aref pointer index))

(defun float2-store (new-value array index)
  (declare (type (array single) array)
           (type index index))
  (let ((new-value (float4 new-value)))
    (with-bounds-check (array index 2)
      (%float2-store new-value array index))))

(define-compiler-macro float2-store (new-value array index)
  (once-only (array index)
    (with-gensyms (value)
      `(let ((,value (float4 ,new-value)))
         (with-bounds-check (,array ,index 2)
           (%float2-store ,value ,array ,index))))))

(definline (setf float2-row-major-aref) (new-value array index)
  (declare (type (array single) array)
           (type index index))
  (float2-store new-value array (* index 2)))

(defun (setf float2-aref) (new-value array &rest subscripts)
  (declare (type (array single) array))
  (float2-store new-value array (apply #'array-row-major-simd-index
                                       array
                                       2
                                       subscripts)))

(define-compiler-macro (setf float2-aref) (new-value array &rest subscripts)
  (once-only (array)
    (with-gensyms (index)
      (let* ((subscript-bindings
               (loop :for subscript :in subscripts
                     :collect `(,(gensym (string '#:i)) ,subscript)))
             (subscripts (mapcar #'first subscript-bindings)))
        `(let ,subscript-bindings
           (with-row-major-simd-index
               (,index ,array 2 ,@subscripts)
             (float2-store ,new-value ,array ,index)))))))

(definline (setf float2-mem-ref) (new-value pointer &optional (offset 0))
  (declare (type pointer pointer)
           (type index offset))
  (let ((new-value (float4 new-value)))
    (%float2-mem-set new-value pointer offset)))

(definline (setf float2-mem-aref) (new-value pointer &optional (index 0))
  (declare (type pointer pointer)
           (type index index))
  (let ((new-value (float4 new-value)))
    (%float2-mem-aset new-value pointer index)))

(defmacro with-simd-bounds-check ((array indices values-op simd-width)
                                  &body body &environment env)
  (check-type array symbol)
  (check-type indices symbol)
  (check-type simd-width (integer 1 *))
  (with-gensyms (tmp size)
    (let ((values (loop :for i :below simd-width
                        :collect (gensym (format nil "~a~a-" '#:v i)))))
      `(progn ,@(when (check-bounds-p env)
                  `((let ((,size (array-total-size ,array)))
                      (multiple-value-bind ,values
                          (,values-op ,indices)
                        ,@(loop :for v :in values
                                :collect `(sb-kernel:check-bound ,array ,size ,v))))))
              (multiple-value-bind (,array ,tmp)
                  (sb-kernel:%data-vector-and-index ,array 0)
                (declare (ignore ,tmp))
                ,@body)))))

(macrolet ((def (type idx-type inst default-mask)
             (let* ((aref-name (symbolicate type '#:-sparse-aref))
                    (aref-vop-name (symbolicate% aref-name))
                    (mem-aref-name (symbolicate type '#:-sparse-mem-aref))
                    (mem-aref-vop-name (symbolicate% mem-aref-name))
                    (vectype (optype-vector-type type))
                    (eltype (optype-element-type type))
                    (idx-values (symbolicate idx-type '#:-values))
                    (simd-width (optype-simd-width type)))
               `(progn
                  (defvop (,aref-vop-name :pure nil)
                      ((src ,type :target rv)
                       (v ,vectype)
                       (indices ,idx-type :target i)
                       (mask ,type :target tmp))
                      ((rv ,type))
                      ((tmp ,type)
                       (i ,idx-type))
                    (move i indices)
                    (move tmp mask)
                    (move rv src)
                    (inst ,inst rv ,(vector-ea-form type 'i) tmp))
                  (defun ,aref-name (array indices &optional (source (,type 0))
                                                             (mask ,default-mask))
                    (declare (type (array ,eltype) array))
                    (let ((indices (,idx-type indices))
                          (source (,type source))
                          (mask (,type mask)))
                      (with-simd-bounds-check (array indices ,idx-values ,simd-width)
                        (,aref-vop-name source array indices mask))))
                  (define-compiler-macro ,aref-name
                      (array indices &optional (source '(,type 0))
                                               (mask ',default-mask))
                    (once-only (array indices source mask)
                      `(let ((,indices (,',idx-type ,indices))
                             (,source (,',type ,source))
                             (,mask (,',type ,mask)))
                         (with-simd-bounds-check (,array
                                                  ,indices
                                                  ,',idx-values
                                                  ,',simd-width)
                           (,',aref-vop-name ,source ,array ,indices ,mask)))))
                  (defvop (,mem-aref-vop-name :pure nil)
                      ((src ,type :target rv)
                       (v pointer)
                       (indices ,idx-type :target i)
                       (mask ,type :target tmp))
                      ((rv ,type))
                      ((tmp ,type)
                       (i ,idx-type))
                    (move i indices)
                    (move tmp mask)
                    (move rv src)
                    (inst ,inst rv (ea v i ,(optype-index-scale-form type 'i)) tmp))
                  (definline ,mem-aref-name (pointer indices &optional (source (,type 0))
                                                                       (mask ,default-mask))
                    (declare (type pointer pointer))
                    (let ((indices (,idx-type indices))
                          (source (,type source))
                          (mask (,type mask)))
                      (,mem-aref-vop-name source pointer indices mask)))))))
  (def float4 int4 vgatherdps (float4! (int4 -1)))
  (def int4 int4 vpgatherdd (int4 -1))
  (def double2 long2 vgatherqpd (double2! (long2 -1)))
  (def long2 long2 vpgatherqq (long2 -1))
  (def float8 int8 vgatherdps (float8! (int8 -1)))
  (def int8 int8 vpgatherdd (int8 -1))
  (def double4 long4 vgatherqpd (double4! (long4 -1)))
  (def long4 long4 vpgatherqq (long4 -1)))

;;; vim: ft=lisp et
