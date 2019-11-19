;; -*-scheme-*-
;;
;; mandel-server.scm - mandelbrot fractal server
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
(use-modules
 (srfi srfi-9)
 ((ice-9 and-let-star) #:select (and-let*))
 ((ice-9 rdelim) #:select (write-line)))

(define-record-type state-record
    (make-state-record)
    state-record?                       ; unused
  (data      sr-data      sr-data!)
  (missing   sr-missing   sr-missing!)
  (index     sr-index     sr-index!)
  (x-ratio   sr-x-ratio   sr-x-ratio!)
  (y-ratio   sr-y-ratio   sr-y-ratio!)
  (palette   sr-palette   sr-palette!)
  (bpp       sr-bpp       sr-bpp!))

(define server-state (make-object-property))

(define-macro (ss server field)
  `(,(symbol-append 'sr- ,field) (server-state ,server)))

(define-macro (ss! server field value)
  `(,(symbol-append 'sr- ,field '!) (server-state ,server) ,value))

;; initialize the server state by calculating values from the configuration
(define (mandel-init server)
  (let* ((x-res (svz:server:config-ref server "x-res"))
         (y-res (svz:server:config-ref server "y-res"))
         (start (string->number (svz:server:config-ref server "start")))
         (end   (string->number (svz:server:config-ref server "end")))
         (x-diff (real-part (- end start)))
         (y-diff (imag-part (- end start)))
         (x-ratio (/ x-diff x-res))
         (y-ratio (/ y-diff y-res)))

    (set! (server-state server) (make-state-record))
    (ss! server 'data (make-vector (* x-res y-res) -1))
    (ss! server 'missing (* x-res y-res))
    (ss! server 'index 0)
    (ss! server 'x-ratio x-ratio)
    (ss! server 'y-ratio y-ratio)
    0
    ))

;; pure integer division for k by n
(define (divide k n)
  (inexact->exact (floor (/ k n))))

;; fill left side string with zeros
(define (left-zeros text n)
  (let ((need (- n (string-length text))))
    (if (positive? need)
        (string-append (make-string need #\0) text)
        text)))

;; create an ascii representation for the given index i, each character is
;; limited to a certain range, creates a string with bpp (bytes per pixel)
;; characters
(define (generate-ascii range bpp i)
  (let* ((ascii (make-string bpp #\space)))
    (let loop ((n 0) (i i))
      (and (< n bpp)
           (let ((c (integer->char (+ (modulo i range)
                                      (char->integer #\space)))))
             (and (equal? c #\")
                  (set! c (integer->char (+ range (char->integer #\space)))))
             (string-set! ascii n c)
             (loop (1+ n) (divide i range)))))
    ascii))

;; generate a colourful palette
(define (mandel-palette server)
  (let* ((colors (svz:server:config-ref server "colors"))
         (rgb (cons (make-vector colors) (make-vector colors)))
         (bpp 1))
    (let loop ((i (1- colors)) (r 0) (g 0) (b 0))
      (and (>= i 0)
           (let* ((val 0)
                  (char-range (- (char->integer #\~) (char->integer #\space))))

             (set! bpp (let loop ((n 1) (cols char-range))
                         (if (>= cols colors) n
                             (loop (1+ n) (* cols char-range)))))
             (set! val (+ (inexact->exact (* 127 (- 1 (cos b))))
                          (* 256 (inexact->exact (* 127 (- 1 (cos g)))))
                          (* 256 256 (inexact->exact (* 127 (- 1 (cos r)))))))
             (vector-set! (car rgb) i (generate-ascii char-range bpp i))
             (vector-set! (cdr rgb) i (left-zeros (number->string val 16) 6))
             (loop (1- i) (+ r 0.2) (+ g 0.02) (+ b 0.33)))))
    (ss! server 'palette rgb)
    (ss! server 'bpp (number->string bpp))))

;; save the generated palette to a file
(define (mandel-save-palette server outfile)
  (let* ((rgb (ss server 'palette))
         (size (vector-length (car rgb))))
    (let loop ((i 0))
      (and (< i size)
           (begin
             (write-line (fs "\"~A\tc #~A\","
                             (vector-ref (car rgb) i)
                             (vector-ref (cdr rgb) i))
                         outfile)
             (loop (1+ i)))))))

;; write out a finished xpm picture
(define (mandel-write server)
  (let* ((data (ss server 'data))
         (x-res (svz:server:config-ref server "x-res"))
         (y-res (svz:server:config-ref server "y-res"))
         (colors (svz:server:config-ref server "colors"))
         (outfile (open-output-file (svz:server:config-ref server "outfile")))
         (cols '()))

    (display (fs "*** Generating \"~A\" ... "
                 (svz:server:config-ref server "outfile")))

    ;; create color palette
    (mandel-palette server)
    (set! cols (car (ss server 'palette)))

    ;; create header
    (write-line "/* XPM */" outfile)
    (write-line "static char * mandel_xpm[] = {" outfile)
    (write-line (fs "\"~A ~A ~A ~A\","
                    (number->string x-res)
                    (number->string y-res)
                    (number->string colors)
                    (ss server 'bpp))
                outfile)

    ;; write palette information
    (mandel-save-palette server outfile)

    ;; output picture data
    (do ((y 0 (1+ y))) ((>= y y-res))
      (write-line
       (fs "\"~A\"~A"
           (string-concatenate
            (map (lambda (x)
                   (vector-ref cols (vector-ref data (+ x (* y x-res)))))
                 (iota x-res)))
           (if (not (= y (1- y-res)))
               ","
               "};"))
       outfile))

    (close-output-port outfile)
    (display "done.\n")))

;; check if all points were calculated
(define (finished? server)
  (<= (ss server 'missing) 0))

;; store a calculated point
(define (save-point! server index value)
  (and-let* ((data (ss server 'data))
             ((< (vector-ref data index) 0)))
    (vector-set! data index value)
    (ss! server 'missing (1- (ss server 'missing)))))

;; calculate the complex number at a given array index
(define (index->z server index)
  (let* ((x (modulo index (svz:server:config-ref server "x-res")))
         (y (divide index (svz:server:config-ref server "x-res")))
         (offset (make-rectangular
                  (* (+ x 0.5) (ss server 'x-ratio))
                  (* (+ y 0.5) (ss server 'y-ratio))))
         (z (+ (string->number (svz:server:config-ref server "start"))
               offset)))
    z))

;; determine the next index to be calculated
(define (next-index! server)
  (let* ((index (ss server 'index))
         (data (ss server 'data))
         (size (vector-length data)))
    (set! index (let loop ((i index))
                  (and (>= i size)
                       (set! i 0))
                  (if (< (vector-ref data i) 0) i (loop (1+ i)))))
    (ss! server 'index (1+ index))
    index))

;; detect our client with a magic string
(define (mandel-detect-proto server sock)
  (or (and-let* ((idx (binary-search (svz:sock:receive-buffer sock)
                                     mandel-magic))
                 ((zero? idx)))
        (svz:sock:receive-buffer-reduce sock (string-length mandel-magic))
        -1)
      0))

;; server information callback for the control protocol
(define (mandel-info server)
  (fs " ~A\r\n ~A: ~A\r\n ~A: ~A"
      "Mandelbrot calculation server."
      "Points given for calculation"
      (ss server 'index)
      "Missing points"
      (ss server 'missing)))

;; connect a new client
(define (mandel-connect server sock)
  (mandel-prepare-sock sock)
  (svz:sock:handle-request sock mandel-handle-request)
  (if (finished? server)
      (begin
        (svz:sock:final-print sock)
        (svz:sock:print sock "(dnc:bye)\r\n") 0)
      (begin
        (svz:sock:print sock "(dnc:welcome)\r\n") 0)))

;; server instance finalizer callback
(define (mandel-finalize server)
  (set! (server-state server) #f)
  (and (finished? server)
       (system)
       (system (fs "~A ~A"
                   (svz:server:config-ref server "viewer")
                   (svz:server:config-ref server "outfile"))))
  0)

;; handle one request by a client
(define (mandel-handle-request sock request len)
  (let* ((server (svz:sock:server sock))
         (colors (svz:server:config-ref server "colors"))
         (tokens (mandel-tokenize request))
         (command (list-ref tokens 1)))

    (case (string->symbol command)
      ;; client wants to disconnect
      ((bye)
       -1)
      ;; client awaits new input data
      ((request)
       (if (finished? server)
           -1
           (let* ((index (next-index! server))
                  (z (index->z server index))
                  (answer (fs "(dnc:value:~S:~S:~S)\r\n"
                              z index colors)))
             (svz:sock:print sock answer)
             0)))
      ;; client has some result
      ((value)
       (save-point! server
                    (string->number (list-ref tokens 3))
                    (string->number (list-ref tokens 4)))
       (if (finished? server)
           (begin
             (mandel-write server)
             (serveez-nuke)
             -1)
           0))
      ;; invalid protocol
      (else -1))))

;; now make Serveez recognize this as a new server
(define-servertype! '(
                      (prefix         . "mandel")
                      (description    . "Distributed Mandelbrot Fractal")
                      (detect-proto   . mandel-detect-proto)
                      (init           . mandel-init)
                      (finalize       . mandel-finalize)
                      (connect-socket . mandel-connect)
                      (info-server    . mandel-info)
                      (configuration  . (
                                         (start   . (string #t "-2.0-1.5i"))
                                         (end     . (string #t "+1.1+1.5i"))
                                         (x-res   . (integer #t 320))
                                         (y-res   . (integer #t 240))
                                         (colors  . (integer #t 256))
                                         (outfile . (string #t "mandel.xpm"))
                                         (viewer  . (string #t "xv"))
                                         ))))

;; throw the pile together
(define-port! 'mandel-port '(
                             (proto . tcp)
                             (port . 1025)
                             (ipaddr . *)))

(define-server! 'mandel-server '(
                                 (x-res . 100)
                                 (y-res . 100)
                                 ))

(define-server! 'control-server '())

(bind-server! 'mandel-port 'mandel-server)
(bind-server! 'mandel-port 'control-server)
