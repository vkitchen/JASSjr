#!/usr/bin/env -S csi -r7rs-syntax -ss

; JASSJR_INDEX.SCM
; ----------------
; Copyright (c) 2026 Vaughan Kitchen
; Minimalistic BM25 search engine.

; Requires installing the srfi-69 egg with
; chicken-install srfi-69

(import
  scheme
  srfi-69
  (chicken io)
  (chicken irregex)
  (chicken process-context))

(define vocab (make-hash-table equal?)) ; the in-memory index
(define doc-ids '()) ; the primary keys
(define doc-lengths '()) ; hold the length of each document

(define docid -1)
(define document-length 0)
(define push-next #f) ; is the next token the primary key?

(define (string-downcase s)
  (list->string
    (map char-downcase (string->list s))))

(define (index tokens)
  (if (null? tokens)
    '()
     (let ((token (car tokens)))
       (begin
         ; If we see a <DOC> tag then we're at the start of the next document
         (if (string=? token "<DOC>")
           (begin
             ; Save the previous document length
             (if (not (= docid -1))
               (set! doc-lengths (cons document-length doc-lengths)))
           
             ; Move on to the next document
             (set! docid (+ docid 1))
             (set! document-length 0)
           
             (if (= (modulo docid 1000) 0)
               (print docid " documents indexed\n"))))
           
           ; if the last token we saw was a <DOCNO> then the next token is the primary key
         (if push-next
           (begin
             (set! doc-ids (cons token doc-ids))
             (set! push-next #f)))
         (if (string=? token "<DOCNO>") (set! push-next #t))
         
         ; Don't index XML tags
         (if (not (char=? (string-ref token 0) #\<))
           (let* (
             ; lower case the string
             (lowercase (string-downcase token))
         
             ; truncate any long tokens at 255 charactes (so that the length can be stored first and in a single byte)
             (lowercase (substring lowercase 0 (min 255 (string-length lowercase)))))
         
             ; add the posting to the in-memory index
             (let ((postings (hash-table-ref/default vocab lowercase #f)))
               (cond
                 ; if the term isn't in the vocab yet
                 ((not postings)
                  (hash-table-set! vocab lowercase
                     (list (cons docid 1))))
               
                 ; if the docno for this occurence has changed then create a new <d,tf> pair
                 ((not (= (caar postings) docid))
                   (hash-table-set! vocab lowercase
                     (cons (cons docid 1) postings)))
               
                 ; else increase the tf
                 (else
                   (set-cdr! (car postings)
                     (+ (cdar postings) 1)))))))

         ; Compute the document length
         (set! document-length (+ document-length 1))

     (index (cdr tokens))))))

(define (tokenise file)
  (let loop ((line (read-line file)))
    (if (eof-object? line)
      (close-input-port file)
      (begin
        (index (irregex-extract "[a-zA-Z0-9][a-zA-Z0-9-]*|<[^>]*>" line))
        (loop (read-line file))))))

(define (main args)
  ; Make sure we have one parameter, the filename
  (if (null? args)
    (display (string-append "Usage: " (program-name) " <infile.xml>\n"))
    ; open the file to index
    (tokenise (open-input-file (car args)))))
