#lang racket

(require racket/cmdline)

(define forever (make-parameter #f))
(define num-attempts (make-parameter 10))
(define delay (make-parameter 3))

(command-line)
