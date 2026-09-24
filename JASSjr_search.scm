#!/usr/bin/env -S csi -r7rs-syntax -ss

; JASSJR_SEARCH.SCM
; -----------------
; Copyright (c) 2026 Vaughan Kitchen
; Minimalistic BM25 search engine.

; Requires installing the srfi-69 egg with
; chicken-install srfi-69

(import
  (scheme base)
  srfi-69
  (chicken bitwise)
  (chicken file posix)
  (chicken io)
  (chicken number-vector))

(define k1 0.9) ; BM25 k1 parameter
(define b 0.4) ; BM25 b parameter

(define (bytevector-u32-ref bv i)
  (+ (bytevector-u8-ref bv i)
     (arithmetic-shift (bytevector-u8-ref bv (+ i 1)) 8)
     (arithmetic-shift (bytevector-u8-ref bv (+ i 2)) 16)
     (arithmetic-shift (bytevector-u8-ref bv (+ i 3)) 24)))

; Read the document lengths
(define doc-lengths
  (call-with-input-file "lengths.bin"
    (lambda (port)
      (let ((bv (make-bytevector (file-size port))))
        (read-bytevector! bv port)
        (bytevector->u32vector/shared bv)))))
; Read the primary_keys
(define doc-ids (read-lines (open-input-file "docids.bin")))
; Build the vocabulary in memory
(define vocab
  (call-with-input-file "vocab.bin"
    (lambda (port)
      (let ((vocab (make-hash-table))
            (bv (make-bytevector (file-size port))))
        (read-bytevector! bv port)
        (let loop ((current 0))
          (when (< current (bytevector-length bv))
            (let* ((string-length (bytevector-u8-ref bv current))
                   (term-start (+ current 1))
                   (where (bytevector-u32-ref bv (+ current string-length 2))) ; +1 for the length and + 1 for the '\0'
                   (size (bytevector-u32-ref bv (+ current string-length 2 4))) ; +1 for the length and + 1 for the '\0'
                   (term (utf8->string (bytevector-copy bv term-start (+ term-start string-length)))))
              (hash-table-set! vocab term (cons where size))
              (loop (+ current string-length 10)))))
        vocab))))

(define (main args)
  (begin
    (print doc-lengths)
    (print doc-ids)
    (hash-table-walk vocab
      (lambda (key value)
        (print "Key: " key ", Value: " value)))))

