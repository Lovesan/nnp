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

(macrolet ((def (type inst ntload-inst ntstore-inst)
             (let* ((eltype (optype-element-type type))
                    (vectype (optype-vector-type type))
                    (simd-width (optype-simd-width type))
                    (load-name (symbolicate type '#:-load))
                    (load-vop-name (symbolicate% load-name))
                    (load-vop-const-name (symbolicate load-vop-name '#:-const))
                    (aref-name (symbolicate type '#:-aref))
                    (rma-name (symbolicate type '#:-row-major-aref))
                    (mem-ref-name (symbolicate type '#:-mem-ref))
                    (mem-ref-vop-name (symbolicate% mem-ref-name))
                    (mem-ref-vop-const-name (symbolicate mem-ref-vop-name '#:-const))
                    (mem-aref-name (symbolicate type '#:-mem-aref))
                    (mem-aref-vop-name (symbolicate% mem-aref-name))
                    (mem-aref-vop-const-name (symbolicate mem-aref-vop-name '#:-const))
                    (store-name (symbolicate type '#:-store))
                    (store-vop-name (symbolicate% store-name))
                    (store-vop-const-name (symbolicate store-vop-name '#:-const))
                    (mem-set-name (symbolicate type '#:-mem-set))
                    (mem-set-vop-name (symbolicate% mem-set-name))
                    (mem-set-vop-const-name (symbolicate mem-set-vop-name '#:-const))
                    (mem-set-aref-name (symbolicate type '#:-mem-set-aref))
                    (mem-set-aref-vop-name (symbolicate% mem-set-aref-name))
                    (mem-set-aref-vop-const-name (symbolicate mem-set-aref-vop-name '#:-const))
                    (ntload-name (symbolicate type '#:-ntload))
                    (ntload-vop-name (symbolicate% ntload-name))
                    (ntload-vop-const-name (symbolicate ntload-vop-name '#:-const))
                    (ntaref-name (symbolicate type '#:-non-temporal-aref))
                    (ntmem-ref-name (symbolicate type '#:-non-temporal-mem-ref))
                    (ntmem-ref-vop-name (symbolicate% ntmem-ref-name))
                    (ntmem-ref-vop-const-name (symbolicate ntmem-ref-vop-name '#:-const))
                    (ntmem-aref-name (symbolicate type '#:-non-temporal-mem-aref))
                    (ntmem-aref-vop-name (symbolicate% ntmem-aref-name))
                    (ntmem-aref-vop-const-name (symbolicate ntmem-aref-vop-name '#:-const))
                    (ntstore-name (symbolicate type '#:-ntstore))
                    (ntstore-vop-name (symbolicate% ntstore-name))
                    (ntstore-vop-const-name (symbolicate ntstore-vop-name '#:-const))
                    (ntrmaref-name (symbolicate type '#:-non-temporal-row-major-aref))
                    (ntmem-set-name (symbolicate type '#:-non-temporal-mem-set))
                    (ntmem-set-vop-name (symbolicate% ntmem-set-name))
                    (ntmem-set-vop-const-name (symbolicate ntmem-set-vop-name '#:-const))
                    (ntmem-set-aref-name (symbolicate type '#:-non-temporal-mem-set-aref))
                    (ntmem-set-aref-vop-name (symbolicate% ntmem-set-aref-name))
                    (ntmem-set-aref-vop-const-name
                      (symbolicate ntmem-set-aref-vop-name '#:-const))
                    (index-scale (optype-index-scale-form type 'i))
                    (eltype-width-in-bytes (floor (optype-width eltype) 8))
                    (ea-form `(ea (+ (* sb-vm:vector-data-offset
                                        sb-vm:n-word-bytes)
                                     (- sb-vm:other-pointer-lowtag))
                                  v i ,index-scale))
                    (const-ea-form `(ea (+ (* sb-vm:vector-data-offset
                                              sb-vm:n-word-bytes)
                                           (* ,eltype-width-in-bytes i)
                                           (- sb-vm:other-pointer-lowtag))
                                        v)))
               `(progn
                  ;; Load
                  (defvop (,load-vop-name :cost 2)
                      ((v ,vectype) (i index))
                      ((rv ,type))
                      ()
                    (inst ,inst rv ,ea-form))
                  (defvop (,load-vop-const-name :pure nil
                                                :translate ,load-vop-name
                                                :cost 1)
                      ((v ,vectype) (i low-index))
                      ((rv ,type))
                      ()
                    (inst ,inst rv ,const-ea-form))
                  (definline ,load-name (array index)
                    (declare (type (array ,eltype) array)
                             (type index index))
                    (with-bounds-check (array index ,type)
                      (,load-vop-name array index)))
                  (define-compiler-macro ,load-name (array index)
                    (once-only (array index)
                      `(with-bounds-check (,array ,index ,',type)
                         (,',load-vop-name ,array ,index))))
                  (definline ,rma-name (array index)
                    (declare (type (array ,eltype) array)
                             (type index index))
                    (,load-name array (* index ,simd-width)))
                  (defun ,aref-name (array &rest subscripts)
                    (declare (type (array ,eltype) array))
                    (,load-name array (apply #'array-row-major-simd-index
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
                               (,',load-name ,array ,index)))))))

                  ;; mem-ref
                  (defvop (,mem-ref-vop-name :pure nil :cost 2)
                      ((p pointer) (i index))
                      ((rv ,type))
                      ()
                    (inst ,inst rv (ea p i)))
                  (defvop (,mem-ref-vop-const-name :pure nil
                                                   :translate ,mem-ref-vop-name
                                                   :cost 1)
                      ((p pointer) (i low-index))
                      ((rv ,type))
                      ()
                    (inst ,inst rv (ea i p)))
                  (definline ,mem-ref-name (pointer &optional (offset 0))
                    (declare (type pointer pointer)
                             (type index offset))
                    (,mem-ref-vop-name pointer offset))

                  ;; mem-aref
                  (defvop (,mem-aref-vop-name :pure nil :cost 2)
                      ((p pointer) (i index))
                      ((rv ,type))
                      ()
                    (inst ,inst rv (ea p i ,index-scale)))
                  (defvop (,mem-aref-vop-const-name :pure nil
                                                    :translate ,mem-aref-vop-name
                                                    :cost 1)
                      ((p pointer) (i low-index))
                      ((rv ,type))
                      ()
                    (inst ,inst rv (ea (* ,eltype-width-in-bytes i) p)))
                  (definline ,mem-aref-name (pointer &optional (index 0))
                    (declare (type pointer pointer)
                             (type index index))
                    (,mem-aref-vop-name pointer (* index ,simd-width)))

                  ;; Store
                  (defvop (,store-vop-name :pure nil :cost 2)
                      ((x ,type :target rv) (v ,vectype) (i index))
                      ((rv ,type))
                      ()
                    (unless (location= rv x)
                      (move rv x))
                    (inst ,inst ,ea-form rv))
                  (defvop (,store-vop-const-name :pure nil
                                                 :translate ,store-vop-name
                                                 :cost 1)
                      ((x ,type :target rv) (v ,vectype) (i low-index))
                      ((rv ,type))
                      ()
                    (unless (location= rv x)
                      (move rv x))
                    (inst ,inst ,const-ea-form rv))
                  (definline ,store-name (new-value array index)
                    (declare (type (array ,eltype) array)
                             (type index index))
                    (with-bounds-check (array index ,type)
                      (,store-vop-name (,type new-value) array index)))
                  (define-compiler-macro ,store-name (new-value array index)
                    (once-only (array index)
                      `(with-bounds-check (,array ,index ,',type)
                         (,',store-vop-name (,',type ,new-value) ,array ,index))))
                  (definline (setf ,rma-name) (new-value array index)
                    (declare (type (array ,eltype) array)
                             (type index index))
                    (,store-name new-value array (* index ,simd-width)))
                  (defun (setf ,aref-name) (new-value array &rest subscripts)
                    (declare (type (array ,eltype) array))
                    (,store-name new-value array (apply #'array-row-major-simd-index
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
                               (,',store-name ,new-value ,array ,index)))))))

                  ;; (setf mem-ref)
                  (defvop (,mem-set-vop-name :pure nil :cost 2)
                      ((x ,type :target rv) (p pointer) (i index))
                      ((rv ,type))
                      ()
                    (unless (location= rv x)
                      (move rv x))
                    (inst ,inst (ea p i) x))
                  (defvop (,mem-set-vop-const-name :pure nil
                                                   :translate ,mem-set-vop-name
                                                   :cost 1)
                      ((x ,type) (p pointer) (i low-index))
                      ((rv ,type))
                      ()
                    (unless (location= rv x)
                      (move rv x))
                    (inst ,inst (ea i p) x))
                  (definline (setf ,mem-ref-name) (new-value pointer &optional (offset 0))
                    (declare (type pointer pointer)
                             (type index offset))
                    (,mem-set-vop-name (,type new-value) pointer offset))

                  ;; (setf mem-aref)
                  (defvop (,mem-set-aref-vop-name :pure nil :cost 2)
                      ((x ,type :target rv) (p pointer) (i index))
                      ((rv ,type))
                      ()
                    (unless (location= rv x)
                      (move rv x))
                    (inst ,inst (ea p i ,index-scale) x))
                  (defvop (,mem-set-aref-vop-const-name :pure nil
                                                        :translate ,mem-set-aref-vop-name
                                                        :cost 1)
                      ((x ,type) (p pointer) (i low-index))
                      ((rv ,type))
                      ()
                    (unless (location= rv x)
                      (move rv x))
                    (inst ,inst (ea (* ,eltype-width-in-bytes i) p) x))
                  (definline (setf ,mem-aref-name) (new-value pointer &optional (index 0))
                    (declare (type pointer pointer)
                             (type index index))
                    (,mem-set-vop-name (,type new-value) pointer (* index ,simd-width)))

                  ;; NT load
                  (defvop (,ntload-vop-name :pure nil :cost 2)
                      ((v ,vectype) (i index))
                      ((rv ,type))
                      ()
                    (inst ,ntload-inst rv ,ea-form))
                  (defvop (,ntload-vop-const-name :pure nil
                                                  :translate ,ntload-vop-name
                                                  :cost 1)
                      ((v ,vectype) (i low-index))
                      ((rv ,type))
                      ()
                    (inst ,ntload-inst rv ,const-ea-form))
                  (definline ,ntload-name (array index)
                    (declare (type (array ,eltype) array)
                             (type index index))
                    (with-bounds-check (array index ,type)
                      (,ntload-vop-name array index)))
                  (define-compiler-macro ,ntload-name (array index)
                    (once-only (array index)
                      `(with-bounds-check (,array ,index ,',type)
                         (,',ntload-vop-name ,array ,index))))
                  (definline ,ntrmaref-name (array index)
                    (declare (type (array ,eltype) array)
                             (type index index))
                    (,ntload-name array (* index ,simd-width)))
                  (defun ,ntaref-name (array &rest subscripts)
                    (declare (type (array ,eltype) array))
                    (,ntload-name array (apply #'array-row-major-simd-index
                                               array
                                               ,simd-width
                                               subscripts)))
                  (define-compiler-macro ,ntaref-name (array &rest subscripts)
                    (once-only (array)
                      (with-gensyms (index)
                        (let* ((subscript-bindings
                                 (loop :for subscript :in subscripts
                                       :collect `(,(gensym (string '#:i)) ,subscript)))
                               (subscripts (mapcar #'first subscript-bindings)))
                          `(let ,subscript-bindings
                             (with-row-major-simd-index
                                 (,index ,array ,',simd-width ,@subscripts)
                               (,',ntload-name ,array ,index)))))))

                  ;; NT mem-ref
                  (defvop (,ntmem-ref-vop-name :pure nil :cost 2)
                      ((p pointer) (i index))
                      ((rv ,type))
                      ()
                    (inst ,ntload-inst rv (ea p i)))
                  (defvop (,ntmem-ref-vop-const-name :pure nil
                                                     :translate ,ntmem-ref-vop-name
                                                     :cost 1)
                      ((p pointer) (i low-index))
                      ((rv ,type))
                      ()
                    (inst ,ntload-inst rv (ea i p)))
                  (definline ,ntmem-ref-name (pointer &optional (offset 0))
                    (declare (type pointer pointer)
                             (type index offset))
                    (,ntmem-ref-vop-name pointer offset))

                  ;; NT mem-aref
                  (defvop (,ntmem-aref-vop-name :pure nil :cost 2)
                      ((p pointer) (i index))
                      ((rv ,type))
                      ()
                    (inst ,ntload-inst rv (ea p i ,index-scale)))
                  (defvop (,ntmem-aref-vop-const-name :pure nil
                                                      :translate ,ntmem-aref-vop-name
                                                      :cost 1)
                      ((p pointer) (i low-index))
                      ((rv ,type))
                      ()
                    (inst ,ntload-inst rv (ea (* ,eltype-width-in-bytes i) p)))
                  (definline ,ntmem-aref-name (pointer &optional (index 0))
                    (declare (type pointer pointer)
                             (type index index))
                    (,ntmem-aref-vop-name pointer (* index ,simd-width)))

                  ;;  NT store
                  (defvop (,ntstore-vop-name :pure nil :cost 2)
                      ((x ,type :target rv) (v ,vectype) (i index))
                      ((rv ,type))
                      ()
                    (unless (location= rv x)
                      (move rv x))
                    (inst ,ntstore-inst ,ea-form rv))
                  (defvop (,ntstore-vop-const-name :pure nil
                                                   :translate ,ntstore-vop-name
                                                   :cost 1)
                      ((x ,type :target rv) (v ,vectype) (i low-index))
                      ((rv ,type))
                      ()
                    (unless (location= rv x)
                      (move rv x))
                    (inst ,ntstore-inst ,const-ea-form rv))
                  (definline ,ntstore-name (new-value array index)
                    (declare (type (array ,eltype) array)
                             (type index index))
                    (with-bounds-check (array index ,type)
                      (,ntstore-vop-name (,type new-value) array index)))
                  (define-compiler-macro ,ntstore-name (new-value array index)
                    (once-only (array index)
                      `(with-bounds-check (,array ,index ,',type)
                         (,',ntstore-vop-name (,',type ,new-value) ,array ,index))))
                  (definline (setf ,ntrmaref-name) (new-value array index)
                    (declare (type (array ,eltype) array)
                             (type index index))
                    (,ntstore-name new-value array (* index ,simd-width)))
                  (defun (setf ,ntaref-name) (new-value array &rest subscripts)
                    (declare (type (array ,eltype) array))
                    (,ntstore-name new-value array (apply #'array-row-major-simd-index
                                                          array
                                                          ,simd-width
                                                          subscripts)))
                  (define-compiler-macro (setf ,ntaref-name) (new-value array &rest subscripts)
                    (once-only (array)
                      (with-gensyms (index)
                        (let* ((subscript-bindings
                                 (loop :for subscript :in subscripts
                                       :collect `(,(gensym (string '#:i)) ,subscript)))
                               (subscripts (mapcar #'first subscript-bindings)))
                          `(let ,subscript-bindings
                             (with-row-major-simd-index
                                 (,index ,array ,',simd-width ,@subscripts)
                               (,',ntstore-name ,new-value ,array ,index)))))))

                  ;; NT (setf mem-ref)
                  (defvop (,ntmem-set-vop-name :pure nil :cost 2)
                      ((x ,type :target rv) (p pointer) (i index))
                      ((rv ,type))
                      ()
                    (unless (location= rv x)
                      (move rv x))
                    (inst ,ntstore-inst (ea p i) x))
                  (defvop (,ntmem-set-vop-const-name :pure nil
                                                     :translate ,ntmem-set-vop-name
                                                     :cost 1)
                      ((x ,type) (p pointer) (i low-index))
                      ((rv ,type))
                      ()
                    (unless (location= rv x)
                      (move rv x))
                    (inst ,ntstore-inst (ea i p) x))
                  (definline (setf ,ntmem-ref-name) (new-value pointer &optional (offset 0))
                    (declare (type pointer pointer)
                             (type index offset))
                    (,ntmem-set-vop-name (,type new-value) pointer offset))

                  ;; NT (setf mem-aref)
                  (defvop (,ntmem-set-aref-vop-name :pure nil :cost 2)
                      ((x ,type :target rv) (p pointer) (i index))
                      ((rv ,type))
                      ()
                    (unless (location= rv x)
                      (move rv x))
                    (inst ,ntstore-inst (ea p i ,index-scale) x))
                  (defvop (,ntmem-set-aref-vop-const-name
                           :pure nil
                           :translate ,ntmem-set-aref-vop-name
                           :cost 1)
                      ((x ,type) (p pointer) (i low-index))
                      ((rv ,type))
                      ()
                    (unless (location= rv x)
                      (move rv x))
                    (inst ,ntstore-inst (ea (* ,eltype-width-in-bytes i) p) x))
                  (definline (setf ,ntmem-aref-name) (new-value pointer &optional (index 0))
                    (declare (type pointer pointer)
                             (type index index))
                    (,ntmem-set-vop-name (,type new-value)
                                         pointer
                                         (* index ,simd-width)))))))
  (def float4 vmovups vmovntdqa vmovntps)
  (def double2 vmovupd vmovntdqa vmovntpd)
  (def sbyte16 vmovdqu vmovntdqa vmovntdq)
  (def ubyte16 vmovdqu vmovntdqa vmovntdq)
  (def short8 vmovdqu vmovntdqa vmovntdq)
  (def ushort8 vmovdqu vmovntdqa vmovntdq)
  (def int4 vmovdqu vmovntdqa vmovntdq)
  (def uint4 vmovdqu vmovntdqa vmovntdq)
  (def long2 vmovdqu vmovntdqa vmovntdq)
  (def ulong2 vmovdqu vmovntdqa vmovntdq))

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

(definline float4x4p (object)
  (typep object 'float4x4))

(definline float4x4-identity ()
  (make-float4x4 (make-float4 1 0 0 0)
                 (make-float4 0 1 0 0)
                 (make-float4 0 0 1 0)
                 (make-float4 0 0 0 1)))

;;; vim: ft=lisp et
