#lang racket

(define-syntax-rule
  (stacker-module-begin lines ...)
  (#%module-begin (eval-stack '(lines ...) null)))

(define (eval-stack lines stack)
  (cond
    [(null? lines) (car stack)]
    [(number? (car lines)) (eval-stack (cdr lines) (cons (car lines) stack))]
    [(equal? '+ (car lines)) (eval-stack (cdr lines) (stack-op + stack))]
    [(equal? '* (car lines)) (eval-stack (cdr lines) (stack-op * stack))]
    [(equal? '- (car lines)) (eval-stack (cdr lines) (stack-op - stack))]
    [(equal? '/ (car lines)) (eval-stack (cdr lines) (stack-op / stack))]
    [else (error (format "Invalid lines: ~a" (car lines)))]))

(define (stack-op op stack)
  (define num1 (car stack))
  (define num2 (car (cdr stack)))
  (define cdr-stack (cdr (cdr stack)))
  (cons (op num1 num2) cdr-stack))

(define (empty-string? str)
  (equal? (string-length (string-trim str)) 0))

(provide eval-stack)
(provide (rename-out [stacker-module-begin #%module-begin]))
