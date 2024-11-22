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
                    (simd-width (optype-simd-width type))
                    (eltype (optype-element-type type))
                    (name (symbolicate type prefix '#:-load))
                    (vop-name (symbolicate% name))
                    (aref-name (symbolicate type prefix '#:-aref))
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
                    (inst ,inst rv ,(vector-ea-form type 'i t)))
                  (defun ,name (array index)
                    (declare (type (array ,eltype) array)
                             (type index index))
                    (with-bounds-check (array index ,type)
                      (,vop-name array index)))
                  (define-compiler-macro ,name (array index)
                    (once-only (array index)
                      `(with-bounds-check (,array ,index ,',type)
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
                    (inst ,inst rv (ea i p)))
                  (definline ,name (pointer &optional (offset 0))
                    (declare (type pointer pointer)
                             (type index offset))
                    (,vop-name pointer offset)))))
           (defmemaref (type inst &optional non-temporal-p)
             (let* ((prefix (if non-temporal-p '#:-non-temporal ""))
                    (name (symbolicate type prefix '#:-mem-aref))
                    (vop-name (symbolicate% name))
                    (eltype (optype-element-type type))
                    (eltype-width-in-bytes (floor (optype-width eltype) 8))
                    (simd-width (optype-simd-width type)))
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
                    (inst ,inst rv (ea (* ,eltype-width-in-bytes i) p)))
                  (definline ,name (pointer &optional (index 0))
                    (declare (type pointer pointer)
                             (type index index))
                    (,vop-name pointer (* index ,simd-width))))))
           (defstore (type inst &optional non-temporal-p)
             (let* ((prefix (if non-temporal-p '#:-non-temporal ""))
                    (name (symbolicate type prefix '#:-store))
                    (vop-name (symbolicate% name))
                    (vectype (optype-vector-type type))
                    (eltype (optype-element-type type))
                    (simd-width (optype-simd-width type))
                    (aref-name (symbolicate type prefix '#:-aref)))
               `(progn
                  (defvop (,vop-name :pure nil :cost 2)
                      ((x ,type :target rv) (v ,vectype) (i index))
                      ((rv ,type))
                      ()
                    (unless (location= rv x)
                      (move rv x))
                    (inst ,inst ,(vector-ea-form type 'i) rv))
                  (defvop (,(symbolicate vop-name '#:-const)
                           :pure nil
                           :translate ,vop-name
                           :cost 1)
                      ((x ,type :target rv) (v ,vectype) (i low-index))
                      ((rv ,type))
                      ()
                    (unless (location= rv x)
                      (move rv x))
                    (inst ,inst ,(vector-ea-form type 'i t) rv))
                  (defun ,name (new-value array index)
                    (declare (type (array ,eltype) array)
                             (type index index))
                    (with-bounds-check (array index ,type)
                      (,vop-name (,type new-value) array index)))
                  (define-compiler-macro ,name (new-value array index)
                    (once-only (array index)
                      `(with-bounds-check (,array ,index ,',type)
                         (,',vop-name (,',type ,new-value) ,array ,index))))
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
           (defmemset (type inst &optional non-temporal-p)
             (let* ((prefix (if non-temporal-p '#:-non-temporal ""))
                    (name (symbolicate type prefix '#:-mem-ref))
                    (vop-name (symbolicate% type prefix '#:-mem-set)))
               `(progn
                  (defvop (,vop-name :pure nil :cost 2)
                      ((x ,type :target rv) (p pointer) (i index))
                      ((rv ,type))
                      ()
                    (unless (location= rv x)
                      (move rv x))
                    (inst ,inst (ea p i) x))
                  (defvop (,(symbolicate vop-name '#:-const)
                           :pure nil
                           :translate ,vop-name
                           :cost 1)
                      ((x ,type) (p pointer) (i low-index))
                      ((rv ,type))
                      ()
                    (unless (location= rv x)
                      (move rv x))
                    (inst ,inst (ea i p) x))
                  (definline (setf ,name) (new-value pointer &optional (offset 0))
                    (declare (type pointer pointer)
                             (type index offset))
                    (,vop-name (,type new-value) pointer offset)))))
           (defmemaset (type inst &optional non-temporal-p)
             (let* ((prefix (if non-temporal-p '#:-non-temporal ""))
                    (name (symbolicate type prefix '#:-mem-aref))
                    (vop-name (symbolicate% type prefix '#:-mem-aset))
                    (eltype (optype-element-type type))
                    (eltype-width-in-bytes (floor (optype-width eltype) 8))
                    (simd-width (optype-simd-width type)))
               `(progn
                  (defvop (,vop-name :pure nil :cost 2)
                      ((x ,type :target rv) (p pointer) (i index))
                      ((rv ,type))
                      ()
                    (unless (location= rv x)
                      (move rv x))
                    (inst ,inst (ea p i ,(optype-index-scale-form type 'i)) x))
                  (defvop (,(symbolicate vop-name '#:-const)
                           :pure nil
                           :translate ,vop-name
                           :cost 1)
                      ((x ,type) (p pointer) (i low-index))
                      ((rv ,type))
                      ()
                    (unless (location= rv x)
                      (move rv x))
                    (inst ,inst (ea (* ,eltype-width-in-bytes i) p) x))
                  (definline (setf ,name) (new-value pointer &optional (index 0))
                    (declare (type pointer pointer)
                             (type index index))
                    (,vop-name (,type new-value) pointer (* index ,simd-width))))))
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

(definline float4x4-identity ()
  (make-float4x4 (make-float4 1 0 0 0)
                 (make-float4 0 1 0 0)
                 (make-float4 0 0 1 0)
                 (make-float4 0 0 0 1)))

;;; vim: ft=lisp et
