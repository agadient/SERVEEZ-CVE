;;; scheme-scanner.scm

;; Copyright (C) 2010, 2011 Thien-Thi Nguyen
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this software; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(define-module (guile-baux scheme-scanner)
  #:export (scheme-scanner)
  #:use-module ((ice-9 regex) #:select (match:start match:end))
  #:use-module ((ice-9 rdelim) #:select (read-line)))

(define HASH-BANG  (make-regexp "^#!"))
(define BANG-HASH  (make-regexp "^!#"))
(define COMMENT    (make-regexp "^[ \t]*(;+)"))
(define WHITESPACE (make-regexp "^[ \f\r\t]*$"))
(define HASH-BAR   (make-regexp "^[ \t]*#\\|"))

;; Return a procedure @var{scanner} that takes one argument, @var{port}.
;; @var{scanner} reads a top-level element from @var{port}, returning
;; a pair that describes it, or the symbol @code{eof} on end-of-file.
;; In a description pair, the @sc{car} is the element @var{type}, the
;; @sc{cadr} is its @var{location} and @sc{cddr} is an alist whose
;; contents are @var{type}-specific.
;;
;; @example
;; (comment AT (leading-semicolons . N)
;;             (text . LINE))
;;
;; (whitespace AT (text . LINE))
;;
;; (hash-bang-comment AT (line-count . N)
;;                       (text-list LINE1 LINE2 ...))
;;
;; (hash-bar-comment AT (text . TEXT))
;;
;; ;; @var{custom} form (see below)
;;
;; (form AT (line-count . N)
;;          (sexp . SEXP))
;; @end example
;;
;; @var{at} is a form @code{(at @var{lno} @var{col} @var{beg} @var{end})};
;; @var{line} is a string sans newline; @var{text} is a string that may
;; contain newlines; @var{lno}, @var{col}, @var{beg}, @var{end} and @var{n}
;; are integers; @var{sexp} is what @code{(read @var{port})} returns.
;;
;; @var{scanner} may throw @code{incomplete-hash-bang-comment}
;; or @code{incomplate-hash-bar-comment} if end-of-file is
;; encountered while scanning those respective comment types.
;;
;; Optional arg @var{custom} is a procedure to be called right before
;; recognizing the (default) @code{form} element.  It takes one arg,
;; @var{port}.  If @var{custom} returns @code{#f}, @var{scanner} falls
;; through to the default, else it returns what @var{custom} returns.
;;
;;-args: (- 1 0 custom)
;;
(define (scheme-scanner . opts)
  (let ((custom (if (null? opts)
                    (lambda (port) #f)
                    (car opts))))
    ;; rv
    (lambda (p)

      (define (more)
        (read-line p))

      (let ((start (ftell p))
            (lno (1+ (port-line p)))
            (col (port-column p))
            (line (more)))

        (define (ok type . rest)
          (cons* type `(at ,lno ,col ,start ,(ftell p))
                 (let loop ((acc '()) (ls rest))
                   (if (null? ls)
                       acc
                       (loop (acons (car ls) (cadr ls) acc)
                             (cddr ls))))))

        (define (try rx)
          (regexp-exec rx line))

        (cond ((eof-object? line)
               'eof)
              ((try HASH-BANG)
               (let ((count 1) (text (list line)))
                 (let gather-hash-bang ()
                   (set! line (more))
                   (and (eof-object? line)
                        (throw 'incomplete-hash-bang-comment))
                   (set! count (1+ count))
                   (set! text (cons line text))
                   (if (try BANG-HASH)
                       (ok 'hash-bang-comment
                           'text-list (reverse! text)
                           'line-count count)
                       (gather-hash-bang)))))
              ((try WHITESPACE)
               (ok 'whitespace
                   'text line))
              ((try COMMENT)
               => (lambda (m)
                    (ok 'comment
                        'text line
                        'leading-semicolons (- (match:end m 1)
                                               (match:start m 1)))))
              ((try HASH-BAR)
               => (lambda (m)
                    (seek p (+ start (match:end m 0)) SEEK_SET)
                    (set-port-line! p (1- lno))
                    (let loop ((level 1) (acc (list #\| #\#)))
                      (if (zero? level)
                          (ok 'hash-bar-comment
                              'text (apply string (reverse! acc)))
                          (let ((c (read-char p)))
                            (and (eof-object? c)
                                 (throw 'incomplete-hash-bar-comment))
                            (loop
                             (cond ((and (char=? #\# c)
                                         (char=? #\| (car acc))
                                         (not (eq? #\# (cadr acc))))
                                    (1- level))
                                   ((and (char=? #\| c)
                                         (char=? #\# (car acc)))
                                    (1+ level))
                                   (else
                                    level))
                             (cons c acc)))))))
              (else
               (seek p start SEEK_SET)
               (set-port-line! p (1- lno))
               (or (custom p)
                   (let ((form (read p)))
                     (ok 'form
                         'line-count (1+ (- (port-line p) lno))
                         'sexp form)))))))))

;;; scheme-scanner.scm ends here
