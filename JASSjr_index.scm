#!/usr/bin/env -S csi -r5rs-syntax -ss

(import scheme (chicken io) (chicken process-context))

(define (index file)
  (let loop ((line (read-line file)))
    (if (eof-object? line)
      (close-input-port file)
      (begin
	(print line)
	(loop (read-line file))))))

(define (main args)
  (if (null? args)
    (display (string-append "Usage: " (program-name) " <infile.xml>\n"))
    (index (open-input-file (car args)))))
