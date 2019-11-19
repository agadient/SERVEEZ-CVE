;;; write-string.scm

;; Copyright (C) 2010 Thien-Thi Nguyen
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

(define-module (guile-baux write-string)
  #:export (write-string))

;; Like @code{write} for string @var{s}, but with the following escapes:
;;
;; @example
;; #\bel  \a     #\newline  \n     #\ht  \t
;; #\np   \f     #\cr       \r     #\vt  \v
;; @end example
;;
;; Another difference is that @var{s} is sent unconditionally
;; to the current output port.
;;
(define (write-string s)
  (display #\")
  (let loop ((ls (string->list s)))
    (or (null? ls)
        (let ((c (car ls)))
          (display (case c
                     ((#\bel)     "\\a")
                     ((#\np)      "\\f")
                     ((#\newline) "\\n")
                     ((#\cr)      "\\r")
                     ((#\ht)      "\\t")
                     ((#\vt)      "\\v")
                     ((#\")      "\\\"")
                     ((#\\)      "\\\\")
                     (else c)))
          (loop (cdr ls)))))
  (display #\"))

;;; write-string.scm ends here
