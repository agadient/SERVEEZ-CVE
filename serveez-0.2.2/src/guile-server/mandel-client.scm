;; -*-scheme-*-
;;
;; mandel-client.scm - mandelbrot fractal client
;;
;; Copyright (C) 2011-2013 Thien-Thi Nguyen
;; Copyright (C) 2001, 2002 Stefan Jahn <stefan@lkcc.org>,
;; Copyright (C) 2001 Raimund Jacob <raimi@lkcc.org>
;;
;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this package.  If not, see <http://www.gnu.org/licenses/>.

;; load shared functionality
(serveez-load "mandel-shared.scm")

;; epsilon environment
(define epsilon 1e-6)

;; return #t if the complex number z1 is near (within a certain epsilon
;; environment) z2, otherwise #f
(define (near? z1 z2)
  (and (< (abs (real-part (- z2 z1))) epsilon)
       (< (abs (imag-part (- z2 z1))) epsilon)))

;; compute the number of iterations for the mandelbrot algorithm for the
;; complex number z, abort calculation at max-iteration limit
(define (iterate-mandel z max-iteration)
  (let ((z-first z))
    (let loop ((i 0) (z 0) (old-z z))
      (cond ((>= i (1- max-iteration)) i)
            ((= z old-z) (1- max-iteration)) ;; use (near?)
            ((> (magnitude z) 2) i)
            (else (loop (1+ i) (+ z-first (* z z)) z))))))

(define todo #f)

;; handle one line sent by the server
(define (mandel-handle-request sock request len)
  (let* ((tokens (mandel-tokenize request))
         (command (list-ref tokens 1)))

    (case (string->symbol command)
      ;; server accepted us
      ((welcome)
       (svz:sock:print sock "(dnc:request)\r\n")
       0)
      ;; server told us to quit
      ((bye)
       -1)
      ;; server gave us something to compute
      ((value)
       (let* ((max-iteration (string->number (list-ref tokens 4)))
              (result (iterate-mandel (string->number (list-ref tokens 2))
                                      max-iteration)))

         (display (fs "*** Calculating point ~S -> ~S\n"
                      (list-ref tokens 3)
                      (number->string result)))
         (svz:sock:print sock
                         (fs "(dnc:value:~S:~S:~S)\r\n"
                             (list-ref tokens 2)
                             (list-ref tokens 3)
                             (number->string result)))
         (if (zero? (1- todo))
             (begin
               (svz:sock:final-print sock)
               (svz:sock:print sock "(dnc:bye)\r\n")
               0)
             (begin
               (set! todo (1- todo))
               (svz:sock:print sock "(dnc:request)\r\n")
               0))))
      ;; invalid server request
      (else -1))))

;; disconnected callback
(define (mandel-disconnected sock)
  (serveez-nuke)
  0)

;; main program entry point
(define (mandel-main max-todo)
  (set! todo max-todo)
  (let ((sock (svz:sock:connect "127.0.0.1" PROTO_TCP 1025)))
    (mandel-prepare-sock sock)
    (svz:sock:handle-request sock mandel-handle-request)
    (svz:sock:disconnected sock mandel-disconnected)
    (svz:sock:print sock mandel-magic)
    ))

(mandel-main (* 320 240))
