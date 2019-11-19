;;; guile-boot.scm --- high-level built-in functionality

;; Copyright (C) 2011-2013 Thien-Thi Nguyen
;; Copyright (C) 2001 Martin Grabmueller <mgrabmue@cs.tu-berlin.de>
;; Copyright (C) 2001 Stefan Jahn <stefan@lkcc.org>
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

;;; Commentary:

;; These procedures complete the boot process.
;; See file guile.c function ‘guile_init’.

;;; Code:

;; Try to load @var{filename} (via @code{primitive-load}).
;; If @var{filename} is not absolute, search for it
;; in the list of directories returned by @code{serveez-loadpath}.
;; Return @code{#t} if successful, @code{#f} otherwise.
;;
(define (serveez-load filename)

  (define (try full)
    (false-if-exception (begin (primitive-load full)
                               #t)))

  (define (absolute?)
    (char=? #\/ (string-ref filename 0)))

  (define (under dir)
    (in-vicinity dir filename))

  (and (string? filename)
       (< 0 (string-length filename))
       (or-map try (if (absolute?)
                       (list filename)
                       (map under (serveez-loadpath))))))

;; Return a string made by applying @code{simple-format #f}
;; to @var{s} and @var{args}.  For example:
;;
;; @example
;; (fs "~A-~S" 'foo 42)
;; @result{} "foo-42"
;; @end example
;;
(define (fs s . args)
  (apply simple-format #f s args))

;; Do @code{display} on each @var{object}.
;; Then, output a newline.
;;
;;-args: (- 0 1 object)
;;
(define (println . args)
  (for-each display args)
  (newline))

;; For each @var{object}, do @code{display} on it
;; and on @var{spacer}, as well.  Then, output a newline.
;;
;;-args: (- 0 1 object)
;;
(define (printsln spacer . args)
  (for-each (lambda (x)
              (display x)
              (display spacer))
            args)
  (newline))

;; Add @var{interface} to the list of known network interfaces.
;; You can get the list of known interfaces by running the shell
;; command @samp{serveez -i}.  The @var{interface} argument must be in
;; dotted decimal form (e.g., @samp{127.0.0.1}).  Serveez provides this
;; procedure for systems where it is unable to detect the list of
;; network interface automatically.
;;
(define (interface-add! interface)
  (serveez-interfaces (append! (serveez-interfaces) (list interface))))

;; Append @var{dir}@dots{} to the server modules load path.
;;
(define (loadpath-add! . dir)
  (serveez-loadpath (append! (serveez-loadpath) dir)))

;; Bind all servers and ports in @var{args} to each other.
;; This is a cross-product operation; given @var{s} servers, and
;; @var{p} ports, @code{@var{s} * @var{p}} bindings will be created.
;;
(define (bind-servers! . args)
  (let ((server-list '())               ; Initialize lists.
        (port-list '()))

    ;; Iterate over argument list, separating ports from servers.
    (for-each
     (lambda (elem)
       (cond ((serveez-port? elem)
              (set! port-list (cons elem port-list)))
             ((serveez-server? elem)
              (set! server-list (cons elem server-list)))))
     args)

    ;; Iterate over server list and ...
    (for-each
     (lambda (server)
       ;; ... for each server, iterate over port list and ...
       (for-each
        (lambda (port)
          ;; ... bind each port to each server.
          (bind-server! port server))
        port-list))
     server-list)))

;; Define a new TCP port named by concatenating
;; @var{basename} and @var{port}.  Return the new name.
;;
(define (create-tcp-port! basename port)
  (let ((portname (fs "~A~A" basename port)))
    (or (serveez-port? portname)
        (define-port! portname
          `((proto . tcp)
            (port . ,port))))
    portname))

;; Bind the list of @var{servers} to simple TCP port configurations whose
;; network ports range between @var{from} and @var{to} both inclusive.
;;
;;-args: (- 0 1 servers)
;;
(define (bind-tcp-port-range! from to . args)
  (do ((no from (+ no 1)))
      ((> no to))
    (for-each
     (lambda (server)
       (bind-server! (create-tcp-port! "guile-tcp-port-" no) server))
     args)))

;; Define a new UDP port named by concatenating
;; @var{basename} and @var{port}.  Return the new name.
;;
(define (create-udp-port! basename port)
  (let ((portname (fs "~A~A" basename port)))
    (or (serveez-port? portname)
        (define-port! portname
          `((proto . udp)
            (port . ,port))))
    portname))

;; Bind the list of @var{servers} to simple UDP port configurations whose
;; network ports range between @var{from} and @var{to} both inclusive.
;;
;;-args: (- 0 1 servers)
;;
(define (bind-udp-port-range! from to . args)
  (do ((no from (+ no 1)))
      ((> no to))
    (for-each
     (lambda (server)
       (bind-server! (create-udp-port! "guile-udp-port-" no) server))
     args)))

;; Return the next RPC entry as a vector of the form:
;; @code{#(@var{name} @var{aliases} @var{program-number})}.
;; @var{name} is a symbol, @var{aliases} is a list (possibly empty)
;; of symbols, and @var{program-number} is an integer.
;; If the list is exhausted, return @code{#f}.
;;
(define (getrpcent) (getrpc))

;; Return the RPC entry for @var{name}, a string.
;; (FIXME: Should be able to handle a symbol, too.)
;; If no such service exists, signal error.
;;
(define (getrpcbyname name)
  (getrpc name))

;; Return the RPC entry for @var{number}, an integer.
;; If no such service exists, signal error.
;;
(define (getrpcbynumber number) (getrpc number))

;; Open and rewind the file @file{/etc/rpc}.
;; If optional arg @var{stayopen} (an integer) is non-zero,
;; the database will not be closed after each call to @code{getrpc}
;; (or its derivatives @code{getrpcent}, @code{getrpcbyname},
;; @code{getrpcbynumber}).
;;
;;-args: (- 1 0)
;;
(define (setrpcent . stayopen)
  (if (pair? stayopen)
      (setrpc (car stayopen))
      (setrpc #f)))

;; Close the file @file{/etc/rpc}.
;;
(define (endrpcent)
  (setrpc))
