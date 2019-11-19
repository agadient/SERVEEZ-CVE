;; -*-scheme-*-
;;
;; eval-server.scm - example server for evaluating Scheme expressions
;;
;; Copyright (C) 2011-2013 Thien-Thi Nguyen
;; Copyright (C) 2001, 2002 Stefan Jahn <stefan@lkcc.org>,
;; Copyright (C) 2001 Martin Grabmueller <mgrabmue@cs.tu-berlin.de>
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

;; Some awkward compatibility kluges for making this run with Guile
;; 1.4 and 1.6/later.
;;
(if (defined? 'micro-version)
    (use-modules (ice-9 safe))
    (begin
      (let ((real-eval eval))
        (set! eval (lambda (expr env)
                     (real-eval expr))))
      (define (object->string obj)
        (format #f "~S" obj))
      (define (make-safe-module) #t)))

(use-modules
 ((ice-9 and-let-star) #:select (and-let*)))

(define (eval-global-init servertype)
  (println "Running eval global init " servertype ".")
  0)

(define (eval-init server)
  (println "Running eval init " server ".")
  0)

(define (eval-global-finalize servertype)
  (println "Running eval global finalizer " servertype ".")
  0)

(define (eval-finalize server)
  (println "Running eval finalizer " server ".")
  0)

(define (eval-detect-proto server sock)
  (println "Detecting eval protocol ...")
  1)

(define (eval-info-server server)
  (println "Running eval server info " server ".")
  " This is the eval server.")

(define (eval-handle-request sock request len)

  (define (out! s . args)
    (svz:sock:print sock (apply fs s args)))

  (or (and-let* ((idx (binary-search request (svz:server:config-ref sock "quit")))
                 ((zero? idx)))
        -1)
      (let ((safe-module (make-safe-module)))
        (catch #t
               (lambda ()
                 (let ((expr (call-with-input-string
                              (binary->string request) read)))
                   (let ((res (eval expr safe-module)))
                     (out! "=> ~S\r\n~A"
                           res
                           (svz:server:config-ref sock "prompt")))))
               (lambda args
                 (out! "Exception: ~A\r\n~A"
                       (apply fs (caddr args) (cadddr args))
                       (svz:server:config-ref sock "prompt"))))
        0)))

(define (eval-connect-socket server sock)
  (println "Running connect socket.")
  (svz:sock:boundary sock "\n")
  (svz:sock:handle-request sock eval-handle-request)
  (svz:sock:print sock (fs "~A\r\n~A"
                           (svz:server:config-ref server "greeting")
                           (svz:server:config-ref server "prompt")))
  0)

;; Port configuration.
(define-port! 'eval-port '((proto . tcp)
                           (port  . 2001)))

;; Servertype definitions.
(define-servertype! '(
  (prefix      . "eval")
  (description . "guile eval server")
  (detect-proto    . eval-detect-proto)
  (global-init     . eval-global-init)
  (init            . eval-init)
  (finalize        . eval-finalize)
  (global-finalize . eval-global-finalize)
  (connect-socket  . eval-connect-socket)
  (info-server     . eval-info-server)
  (configuration   . (
    (prompt   . ( string #t "eval: " ))
    (quit     . ( string #t "quit" ))
    (greeting . ( string #t "Hello, type `quit' to end the connection.\r
Type Scheme expression to see them evaluated (but only one-liners)." ))
    ))))

;; Server instantiation.
(define-server! 'eval-server '(
                               (prompt . "guile> ")
                               (quit   . "quit")
                               ))

;; Bind server to port.
(bind-server! 'eval-port 'eval-server)
