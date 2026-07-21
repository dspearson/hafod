#!chezscheme
;;; (hafod srfi-74) -- SRFI-74: Octet-Addressed Binary Objects
;;; Reference: https://srfi.schemers.org/srfi-74/srfi-74.html
;;; Copyright (c) 2026 Dominic Pearson.
;;; Maps SRFI-74 blob operations to Chez bytevector operations.

(library (hafod srfi-74)
  (export make-blob blob-length
          blob-u8-ref blob-u8-set!
          blob-s8-ref blob-s8-set!
          blob-u16-ref blob-u16-set!
          blob-s16-ref blob-s16-set!
          blob-u32-ref blob-u32-set!
          blob-s32-ref blob-s32-set!
          blob-u64-ref blob-u64-set!
          blob-s64-ref blob-s64-set!
          blob?
          blob-uint-ref blob-sint-ref blob-uint-set! blob-sint-set!
          blob->u8-list u8-list->blob
          blob->uint-list uint-list->blob
          blob->sint-list sint-list->blob
          blob-u16-native-ref blob-u16-native-set!
          blob-s16-native-ref blob-s16-native-set!
          blob-u32-native-ref blob-u32-native-set!
          blob-s32-native-ref blob-s32-native-set!
          blob-u64-native-ref blob-u64-native-set!
          blob-s64-native-ref blob-s64-native-set!
          blob-copy blob-copy! blob=?
)
  (import (chezscheme))

  (define make-blob make-bytevector)
  (define blob-length bytevector-length)
  (define blob-u8-ref bytevector-u8-ref)
  (define blob-u8-set! bytevector-u8-set!)
  (define blob-s8-ref bytevector-s8-ref)
  (define blob-s8-set! bytevector-s8-set!)

  (define (blob-u16-ref eness blob k)
    (bytevector-u16-ref blob k eness))
  (define (blob-u16-set! eness blob k val)
    (bytevector-u16-set! blob k val eness))
  (define (blob-s16-ref eness blob k)
    (bytevector-s16-ref blob k eness))
  (define (blob-s16-set! eness blob k val)
    (bytevector-s16-set! blob k val eness))

  (define (blob-u32-ref eness blob k)
    (bytevector-u32-ref blob k eness))
  (define (blob-u32-set! eness blob k val)
    (bytevector-u32-set! blob k val eness))
  (define (blob-s32-ref eness blob k)
    (bytevector-s32-ref blob k eness))
  (define (blob-s32-set! eness blob k val)
    (bytevector-s32-set! blob k val eness))

  (define (blob-u64-ref eness blob k)
    (bytevector-u64-ref blob k eness))
  (define (blob-u64-set! eness blob k val)
    (bytevector-u64-set! blob k val eness))
  (define (blob-s64-ref eness blob k)
    (bytevector-s64-ref blob k eness))
  (define (blob-s64-set! eness blob k val)
    (bytevector-s64-set! blob k val eness))

  ;; blob? -- bytevector-backed, non-disjoint (a deliberate documented divergence)
  (define blob? bytevector?)

  ;; Variable-width accessors. SRFI 74 argument order is (size endianness blob k
  ;; [n]); the Chez delegate order is (bytevector-uint-ref blob k endianness
  ;; size) -- size leads in the SRFI form but trails in the Chez call, so do not
  ;; transpose size and endianness.
  (define (blob-uint-ref size eness blob k)
    (bytevector-uint-ref blob k eness size))
  (define (blob-sint-ref size eness blob k)
    (bytevector-sint-ref blob k eness size))
  (define (blob-uint-set! size eness blob k n)
    (bytevector-uint-set! blob k n eness size))
  (define (blob-sint-set! size eness blob k n)
    (bytevector-sint-set! blob k n eness size))

  ;; blob<->list conversions. The u8 forms are plain octet loops; the sized uint
  ;; and sint forms step by `size` octets, delegating to the variable-width
  ;; accessors above (same argument order).
  (define (blob->u8-list blob)
    (let loop ((i (- (bytevector-length blob) 1)) (acc '()))
      (if (< i 0) acc
          (loop (- i 1) (cons (bytevector-u8-ref blob i) acc)))))
  (define (u8-list->blob lst)
    (let* ((n (length lst))
           (blob (make-bytevector n)))
      (let loop ((l lst) (i 0))
        (unless (null? l)
          (bytevector-u8-set! blob i (car l))
          (loop (cdr l) (+ i 1))))
      blob))
  (define (blob->uint-list size eness blob)
    (let ((n (div (bytevector-length blob) size)))
      (let loop ((i (- n 1)) (acc '()))
        (if (< i 0) acc
            (loop (- i 1)
                  (cons (bytevector-uint-ref blob (* i size) eness size) acc))))))
  (define (uint-list->blob size eness lst)
    (let* ((n (length lst))
           (blob (make-bytevector (* n size))))
      (let loop ((l lst) (i 0))
        (unless (null? l)
          (bytevector-uint-set! blob (* i size) (car l) eness size)
          (loop (cdr l) (+ i 1))))
      blob))
  (define (blob->sint-list size eness blob)
    (let ((n (div (bytevector-length blob) size)))
      (let loop ((i (- n 1)) (acc '()))
        (if (< i 0) acc
            (loop (- i 1)
                  (cons (bytevector-sint-ref blob (* i size) eness size) acc))))))
  (define (sint-list->blob size eness lst)
    (let* ((n (length lst))
           (blob (make-bytevector (* n size))))
      (let loop ((l lst) (i 0))
        (unless (null? l)
          (bytevector-sint-set! blob (* i size) (car l) eness size)
          (loop (cdr l) (+ i 1))))
      blob))

  ;; Native-endianness fixed-width variants -- direct aliases of the Chez native
  ;; primitives (the argument order matches 1:1, so no wrapper lambda). 8-bit
  ;; widths have no native variant: endianness is meaningless for a single octet.
  (define blob-u16-native-ref bytevector-u16-native-ref)
  (define blob-u16-native-set! bytevector-u16-native-set!)
  (define blob-s16-native-ref bytevector-s16-native-ref)
  (define blob-s16-native-set! bytevector-s16-native-set!)
  (define blob-u32-native-ref bytevector-u32-native-ref)
  (define blob-u32-native-set! bytevector-u32-native-set!)
  (define blob-s32-native-ref bytevector-s32-native-ref)
  (define blob-s32-native-set! bytevector-s32-native-set!)
  (define blob-u64-native-ref bytevector-u64-native-ref)
  (define blob-u64-native-set! bytevector-u64-native-set!)
  (define blob-s64-native-ref bytevector-s64-native-ref)
  (define blob-s64-native-set! bytevector-s64-native-set!)

  (define (blob-copy blob) (bytevector-copy blob))
  (define (blob-copy! src src-start dest dest-start n)
    (bytevector-copy! src src-start dest dest-start n))
  (define (blob=? a b) (bytevector=? a b)))
