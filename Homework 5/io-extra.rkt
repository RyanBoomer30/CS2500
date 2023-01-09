#lang racket
(provide (all-defined-out))

;; write-lines : PathString [List-of String] -> PathString
;; (write-lines filename lines) writes each line to filename, with a newline
;; separating each line. If filename already exists, it will replace it.
(define (write-lines path lines)
  (local ([define out-port (open-output-file path #:mode 'text #:exists 'truncate/replace)])
    (for ([line (in-list lines)])
      (if (not (string? line))
          (error 'write-lines "found a non-string: ~a" line)
          (begin
            (display line out-port)
            ; Produces CRLF on Windows.
            (newline out-port))))
    (close-output-port out-port)
    path))