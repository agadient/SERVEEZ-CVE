;;; common.scm

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

(define-module (guile-baux common)
  #:export (fs fso fse die check-hv qop<-args)
  #:autoload (ice-9 getopt-long) (getopt-long)
  #:autoload (ice-9 documentation) (file-commentary)
  #:use-module ((srfi srfi-13) #:select (string-trim-both)))

;; Apply @code{simple-format} to @var{s} and @var{args},
;; returning the result as a string.
;;
(define (fs s . args)
  (apply simple-format #f s args))

;; Apply @code{simple-format} to @var{s} and @var{args},
;; sending the result to the current output port.
;;
(define (fso s . args)
  (apply simple-format #t s args))

;; Apply @code{simple-format} to @var{s} and @var{args},
;; sending the result to the current error port.
;;
(define (fse s . args)
  (apply simple-format (current-error-port) s args))

;; If @var{s} is specified, apply @code{fse} to @var{s} and @var{args}.
;; Then @code{exit} with @var{exit-value}.
;;
;;-args: (- 1 1 s args)
;;
(define (die exit-value . why)
  (or (null? why) (apply fse why))
  (exit exit-value))

;; Check @var{args} (list of strings) for second element being
;; @samp{--help} or @samp{--version}.  If found, display the respective
;; information, using @var{config}, to stdout and then @code{exit}
;; successfully.  If not found, return @code{#f}.  The recognized
;; @var{config} keys are:
;;
;; @table @code
;; @item package
;; A string describing program affiliation (for @samp{--version}).
;;
;; @item version
;; A string, or a thunk that yields a string when called,
;; to use instead of the default "VERSION UNKNOWN".
;; Output, depending on whether or not @code{package} is specified, is:
;;
;; @smallexample
;; PROGRAM (PACKAGE) VERSION
;; PROGRAM VERSION
;; @end smallexample
;;
;; where @var{program} is @code{(basename (car args))}.
;;
;; @item help
;; Either a (typically multi-line) string, a thunk that produces a
;; string, or the symbol @code{commentary}, which means use
;; @code{file-commentary} from module @code{(ice-9 documentation)} to
;; obtain the string.
;; @end table
;;
;; All strings are trimmed of leading and trailing whitespace.
;;
;; Lastly, @var{flags} are zero or more symbols that further change
;; the default behavior:
;; @itemize
;; @item @code{no-exit} means don't @code{exit}; instead, after
;; doing output return @code{#t}.
;; @item @code{v-before} means for @samp{--help}, first do the output
;; for @samp{--version}.
;; @end itemize
;;
(define (check-hv args config . flags)
  (and (pair? args) (pair? (cdr args))  ; i.e., argc 2+
       (let ((program (car args))
             (done (if (memq 'no-exit flags)
                       identity
                       exit)))

         (define (det k)
           (assq-ref config k))

         (define (output-version)
           (fso "~A~A ~A~%"
                (basename program)
                (cond ((det 'package) => (lambda (s)
                                           (fs " (~A)" s)))
                      (else ""))
                (string-trim-both
                 (let ((v (det 'version)))
                   (cond ((string? v) v)
                         ((thunk? v) (v))
                         (else "VERSION UNKNOWN"))))))

         (case (string->symbol (cadr args))

           ((--version)
            (output-version)
            (done #t))

           ((--help)
            (and (memq 'v-before flags) (output-version))
            (display
             (string-trim-both
              (let ((v (det 'help)))
                (cond ((string? v) v)
                      ((thunk? v) (v))
                      ((eq? 'commentary v) (file-commentary program))
                      (else (string-append (basename program) " [ARG...]"))))))
            (newline)
            (done #t))

           (else #f)))))

;; Do @code{(getopt-long @var{args} @var{option-spec})},
;; and return a procedure @var{qop} that encapsulates the result.
;; You can then call @var{qop} in various ways:
;;
;; @table @code
;; @item (@var{qop} #t)
;; Return the raw result of the @code{getopt-long} call.
;;
;; @item (@var{qop} @var{key})
;; Return the value associated with @var{key}, or @code{#f}.
;;
;; As a special case, if @var{key} is the empty list, then
;; return the (possibly empty) list of strings comprising the
;; non-option @var{args}.  Note that @code{getopt-long} stops
;; processing @var{args} if it sees @samp{--} (hyphen, hyphen);
;; all elements following it are considered non-option.
;;
;; @item (@var{qop} @var{key} @var{proc})
;; If @var{key} has an associated value, call @code{proc} with
;; the value and return its result.  Otherwise, return @code{#f}.
;; This is a shorthand for @code{(and=> (qop @var{key}) @var{proc})}.
;; @end table
;;
(define (qop<-args args option-spec)
  (let ((parsed (getopt-long args option-spec)))
    (lambda (key . proc)
      (cond ((eq? #t key) parsed)
            ((option-ref parsed key #f) => (if (null? proc)
                                               identity
                                               (car proc)))
            (else #f)))))

;;; common.scm ends here
