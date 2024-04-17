#!/usr/bin/env -S csi -r5rs-syntax -ss

(import
  scheme
  srfi-4
  (chicken blob)
  (chicken io)
)

(define (main args)
  (display (blob->s32vector (string->blob (with-input-from-file "lengths.bin" read-string))))
)
