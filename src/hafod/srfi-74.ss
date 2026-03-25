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

  (define (blob-copy blob) (bytevector-copy blob))
  (define (blob-copy! src src-start dest dest-start n)
    (bytevector-copy! src src-start dest dest-start n))
  (define (blob=? a b) (bytevector=? a b)))
