;; -*-scheme-*-
;;
;; inetd.scm - replacement for the Internet ``super-server''
;;
;; Copyright (C) 2011-2013 Thien-Thi Nguyen
;; Copyright (C) 2001, 2002 Stefan Jahn <stefan@lkcc.org>
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

(use-modules
 ((srfi srfi-13) #:select (string-trim
                           string-index
                           string-take))
 ((srfi srfi-14) #:select (ucs-range->char-set))
 ((ice-9 and-let-star) #:select (and-let*))
 ((ice-9 rdelim) #:select (read-line)))

;; the inetd configuration file
(define config-file "/etc/inetd.conf")

;; working directory
(define directory "/tmp")

;; should inetd use ‘fork’ and ‘exec’?
(define do-fork #t)

;; print some messages if #t
(define verbose #f)

;; bind servers to this network address or device
;; (define ip-address "*")
(define ip-address "any")

;; displays a daemon-specific message to stdout, followed by newline
(define (hey s . args)
  (display "inetd: ")
  (display (apply fs s args))
  (newline))

;; opens a configuration file, reads each line and uncomments them,
;; comments are marked with a leading '#', returns a list of non-empty
;; lines
(define (read-config file)
  (catch #t
         (lambda ()
           (call-with-input-file file
             (lambda (port)
               (let loop ((lines '()))

                 (define (try s)
                   (loop (if (string-null? s)
                             lines
                             (cons s lines))))

                 (let ((line (read-line port)))
                   (cond ((eof-object? line)
                          ;; rv
                          (reverse! lines))
                         ((string-index line #\#)
                          => (lambda (hash)
                               (try (string-take line hash))))
                         (else
                          (try line))))))))
         (lambda args
           (hey "unable to parse `~A'" file)
           '())))

;; splits a string in the format "string1.string2" into a pair.  if
;; ".string2" is omitted the (cdr) defaults to #f.  the default delimiter
;; for a tuple is #\. and can be set via the last argument.
(define (split-tuple string . c)
  (let* ((c (if (pair? c) (car c) #\.))
         (i (string-index string c)))
    (if i
        (cons (string-take string i) (substring string (1+ i)))
        (cons string #f))))

;; returns the next position of a white space character in the given
;; string or #f if there is no further space or the string was empty
(define next-space-position
  (let ((cs (ucs-range->char-set 0 (1+ (char->integer #\space)))))
    ;; next-space-position
    (lambda (string)
      (string-index string cs))))

;; tokenizes a given string into a vector, if TOKENS is a number the
;; procedure parses this number of tokens and stores the remaining tokens as
;; a variable length vector in the last element of the returned vector,
;; otherwise it returns a variable length vector
(define (string-split string . tokens)
  (let loop ((acc '()) (stop (and (pair? tokens)
                                  (1- (car tokens)))))
    (set! string (string-trim string))
    (let* ((i (next-space-position string))
           (full (cons (if i
                           (string-take string i)
                           string)
                       acc)))

      (define (vec<- . last)
        (list->vector (reverse! (append last full))))

      (set! string (if i
                       (substring string i)
                       ""))
      (cond ((and i (or (not stop)
                        (positive? stop)))
             (loop full (and stop (1- stop))))
            (stop
             (vec<- (string-split string)))
            ((null? acc)
             #f)
            (else
             (vec<-))))))

;; returns the full rpc entry for a given service name or #f if there is
;; no such service found in the file `/etc/rpc'
(define (lookup-rpc-service name)
  (catch #t
         (lambda ()
           (getrpc name))
         (lambda key
           (hey "no such rpc service `~A'" name)
           #f)))

;; this procedure translates a given service line into a pair containing
;; the full rpc entry as a vector and their versions (also as a pair of
;; numbers)
(define (get-rpc-service service-line)
  (let* ((entry (split-tuple (vector-ref service-line 0) #\/))
         (name (car entry))
         (versions (if (cdr entry)
                       (split-tuple (cdr entry) #\-)
                       (cons "1" "1")))
         (version-begin (car versions))
         (version-end (if (cdr versions) (cdr versions) version-begin)))
    (or (cdr entry)
        (hey "~A: no rpc version" name))
    (cons (lookup-rpc-service name)
          (cons (string->number version-begin)
                (string->number version-end)))))

;; creates a unique name suffix for rpc servers and port configurations
(define (protocol-rpc-string prefix rpc proto)
  (fs "~A-~A-~A" prefix proto rpc))

;; creates and defines a rpc port configuration.  the network port is set
;; to zero in order to let the system choose one
(define (create-rpc-portcfg rpc proto)
  (let ((name "undefined"))
    (define-port! name
      `((,name
         . ,(protocol-rpc-string "inetd-port" rpc proto))
        ("port" .
         0)
        ("ipaddr" .
         ,ip-address)
        ("proto" .
         ,proto)))
    name))

;; creates and defines a rpc server instance from the given splitted service
;; line.
(define (create-rpc-server line rpc proto)
  (let ((name (protocol-rpc-string "prog-server" rpc proto))
        (threads (split-tuple (vector-ref line 3))))
    (define-server! name
      `(,@(cond ((vector-ref line 6)
                 => (lambda (argv)
                      `(("argv" .
                         ,(vector->list argv)))))
                (else '()))
        ,@(cond ((cdr threads)
                 => (lambda (freq)
                      `(("thread-frequency" .
                         ,(string->number freq)))))
                (else '()))
        ("single-threaded" .
         ,(equal? "wait" (car threads)))
        ("do-fork" .
         ,do-fork)
        ("user" .
         ,(vector-ref line 4))
        ("directory" .
         ,directory)
        ("binary" .
         ,(service-binary line))))
    name))

;; returns either "tcp" or "udp" needed to determine the kind of port
;; configuration the identd has to define
(define (rpc-protocol service-line)
  (cdr (split-tuple (vector-ref service-line 2) #\/)))

;; converts a given network protocol name into a valid ip protocol
;; identifier
(define (rpc-ip-proto service-line)
  (if (equal? (rpc-protocol service-line) "tcp")
      IPPROTO_TCP
      IPPROTO_UDP))

;; checks whether the rpc service identified by [number,version] is already
;; registered at the portmapper and returns #t if so.  otherwise the
;; procedure returns #f.
(define (check-rpc-portmapper number version)
  (let ((mappings (portmap-list)))
    (let loop ((mapping mappings))
      (and (not (null? mapping))
           (or (and (equal? (vector-ref (car mapping) 0) number)
                    (equal? (vector-ref (car mapping) 1) version))
               (loop (cdr mapping)))))))

;; this procedure registers the rpc service identified by the triplet
;; [number,version,protocol] at a network port system wide.  this
;; information can be obtained issuing the `rpcinfo -p' command.
(define (run-rpc-portmapper name number version protocol port)
  (let ((result #f))
    ;; should the previous setting really be disabled ?
    (catch #t
           (lambda ()
             (and (check-rpc-portmapper number version)
                  (begin
                    (and verbose
                         (hey "unregistering rpc service `~A'" name))
                    (portmap number version)))
             (set! result #t))
           (lambda key
             (hey "portmapping for rpc service `~A' cannot be unregistered"
                  name)
             (set! result #f)))

    (catch #t
           (lambda ()
             (portmap number version protocol port)
             #t)
           (lambda key
             (hey "portmapper for rpc service `~A' failed"
                  name)
             #f))))

;; when the inetd determines a valid rpc line in its configuration file
;; this procedure is called.
(define (bind-rpc-service service-line)
  (and-let* ((service (get-rpc-service service-line))
             (rpc (car service)))
    (let* ((versions (cdr service))
           ;; create port configuration and server
           (name (vector-ref rpc 0))
           (proto (rpc-protocol service-line))
           (port (create-rpc-portcfg name proto))
           (server (create-rpc-server service-line name proto)))
      ;; bind the server to its port
      (and verbose
           (hey "binding `~A' to `~A'" server port))
      (bind-server! port server)

      ;; now go through each listening socket structure the server got
      ;; finally bound to
      (for-each (lambda (sock)
                  ;; obtain the local network port
                  (let ((port (cdr (svz:sock:local-address sock))))
                    ;; for each version specified in the original
                    ;; service line
                    (do ((version (car versions) (+ version 1)))
                        ((> version (cdr versions)))
                      ;; create a port-mapping
                      (run-rpc-portmapper (vector-ref rpc 0)
                                          (vector-ref rpc 2) version
                                          (rpc-ip-proto service-line)
                                          (ntohs port)))))
                ;; get the listening socket sructures
                (svz:server:listeners server)))))

;; this checks if the given service line specifies a rpc service or not
(define (rpc-service? service-line)
  (cdr (split-tuple (vector-ref service-line 2) #\/)))

;; returns a service with fully qualified port, protocol, service
;; name and its aliases if there is such a service, otherwise the
;; procedure returns #f
(define (lookup-service service-line)
  (catch #t
         (lambda ()
           (getservbyname (vector-ref service-line 0)
                          (vector-ref service-line 2)))
         (lambda key
           (hey "no such service `~A', protocol `~A'"
                (vector-ref service-line 0)
                (vector-ref service-line 2))
           #f)))

;; returns the name of the program which is meant to be started for a given
;; service line
(define (service-binary service)
  (vector-ref service 5))

;; this procedure returns #t if the service specified by the given service
;; line is explicitely disabled by the keywork `disable'
(define (service-disabled? service)
  (equal? (service-binary service) "disable"))

;; creates a "protocol-port" text representation from a service vector
;; returned by (lookup-service)
(define (protocol-port-string prefix service)
  (fs "~A-~A-~A"
      prefix
      (vector-ref service 3)
      (vector-ref service 2)))

;; creates a port configuration for Serveez, returns the name of the new
;; port or #f on failure
(define (create-portcfg line)
  (and-let* ((service (lookup-service line))
             (name (protocol-port-string "inetd-port" service)))
    (define-port! name
      `(("port" .
         ,(vector-ref service 2))
        ("ipaddr" .
         ,ip-address)
        ("proto" .
         ,(vector-ref service 3))))
    name))

;; creates a program passthrough server for Serveez, returns the name of
;; the new server or #f on failure
(define (create-server line)
  (and-let* ((service (lookup-service line)))
    (if (equal? "internal" (service-binary line))
        (translate-internal-server line service)
        (let ((threads (split-tuple (vector-ref line 3)))
              (name (protocol-port-string "prog-server" service)))
          (define-server! name
            `(,@(cond ((vector-ref line 6)
                       => (lambda (argv)
                            `(("argv" .
                               ,(vector->list argv))))))
              ("single-threaded" .
               ,(equal? "wait" (car threads)))
              ,@(cond ((cdr threads)
                       => (lambda (freq)
                            `(("thread-frequency" .
                               ,(string->number freq)))))
                      (else '()))
              ("do-fork" .
               ,do-fork)
              ("user" .
               ,(vector-ref line 4))
              ("directory" .
               ,directory)
              ("binary" .
               ,(service-binary line))))
          name))))

;; translates the inetd servers marked with `internal' into Serveez
;; servers if possible
(define (translate-internal-server line service)
  (let* ((server '())
         (name (vector-ref service 0))
         (threads (split-tuple (vector-ref line 3))))
    (and verbose
         (hey "translating internal `~A' server" name))
    (cond
     ;; timeserver
     ((equal? name "time")
      (and (serveez-servertype? "sntp")
           (begin
             (set! name (protocol-port-string "sntp-server" service))
             (define-server! name server)
             name)))
     ;; echo
     ((equal? name "echo")
      #f)
     ;; sink
     ((equal? name "discard")
      #f)
     ;; daytime
     ((equal? name "daytime")
      #f)
     ;; ttytst source
     ((equal? name "chargen")
      #f)
     (else #f))))

;; glues the above port configurations and servers together:
;;   this is achieved by splitting the lines of the configuration file
;;   into tokens.  each inetd line looks like
;;     (service name) (socket type) (protocol) (wait/nowait[.max]) \
;;     (user[.group]) (server program) (server program arguments)
;;   that is why we split this line into 6 tokens with an additional rest
;;   token, pass them to the port configuration and server builders and
;;   finally bind the server to the port.
(define (create-bindings lines)
  (for-each
   (lambda (line)
     (let ((service (string-split line 6)))
       (or (service-disabled? service)
           (if (rpc-service? service)
               (bind-rpc-service service)
               (begin
                 (let* ((port (create-portcfg service))
                        (server (create-server service)))
                   (and-let* (port server)
                     (and verbose
                          (hey "binding `~A' to `~A'" server port))
                     (bind-server! port server))))))))
   lines))

;; main program entry point
(define (inetd-main)
  (let ((lines (read-config config-file)))
    (create-bindings lines)))

;; run
(inetd-main)
