#!/usr/bin/env racket

#lang racket

(require racket/cmdline)

(define forever (make-parameter #f))
(define num-attempts (make-parameter 10))
(define delay (make-parameter 3))

(define (retry forever num-attempts attempt-num total-attempts delay cmd)
  (define cmd-string (string-join cmd " "))
  (let ([result (system/exit-code cmd-string)])
    (match result
      [0 (exit 0)]
      [error-code (cond
                    [forever ((lambda ()
                               (display
                                (format "Attempt ~a of ~a failed. Retrying in ~a seconds...\n"
                                        attempt-num
                                        total-attempts
                                        delay))
                               (sleep delay)
                               (retry forever
                                      (- num-attempts 1)
                                      (+ attempt-num 1)
                                      total-attempts
                                      delay
                                      cmd)))]
                    [(> num-attempts 1) ((lambda ()
                                          (display
                                           (format "Attempt ~a of ~a failed. Retrying in ~a seconds...\n"
                                                   attempt-num
                                                   total-attempts
                                                   delay))
                                          (sleep delay)
                                          (retry forever
                                                 (- num-attempts 1)
                                                 (+ attempt-num 1)
                                                 total-attempts
                                                 delay
                                                 cmd)))]
                    [else ((lambda ()
                             (display (format "Attempt ~a of ~a exhausted. Giving up\n"
                                              attempt-num
                                              total-attempts))
                            (exit 1)))])])))

(command-line
 #:program "retry"
 #:usage-help "Runs a command, retrying if the command fails"
 #:once-any
 ["--forever" "Retry until the command succeeds"
              (forever #t)]
 [("-n" "--num-attempts") number
                          "The maximum number of times to retry the command (default: 10)"
                          (num-attempts (string->number number))]
 #:once-each
 [("-d" "--delay") delay-seconds
                   "The number of seconds to wait between attempts (default: 3)"
                   (delay (string->number delay-seconds))]
 #:args cmd (retry (forever) (num-attempts) 1 (num-attempts) (delay) cmd))
