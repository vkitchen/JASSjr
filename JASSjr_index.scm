#!/usr/bin/env -S csi -r5rs-syntax -ss

(import
  scheme
  (chicken io)
  (chicken irregex)
  (chicken process-context))

(define (print-list lst)
  (if (null? lst)
    '()
    (begin
      (print (car lst))
      (print-list (cdr lst)))))

(define (index file)
  (let loop ((line (read-line file)))
    (if (eof-object? line)
      (close-input-port file)
      (begin
	(print-list (irregex-extract "[a-zA-Z0-9][a-zA-Z0-9-]*|<[^>]*>" line))
	(loop (read-line file))))))

(define (main args)
  (if (null? args)
    (display (string-append "Usage: " (program-name) " <infile.xml>\n"))
    (index (open-input-file (car args)))))
