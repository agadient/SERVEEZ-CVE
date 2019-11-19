;;; a-dash-dash-b.scm

;; Copyright (C) 2011 Thien-Thi Nguyen
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

(define-module (guile-baux a-dash-dash-b)
  #:export (a-dash-dash-b)
  #:use-module ((srfi srfi-1) #:select (break))
  #:use-module ((srfi srfi-11) #:select (let-values)))

;; Return two values made by splitting @var{args}, a list of strings,
;; on @samp{--} (dash-dash), omitting it from the right-hand side.
;;
;; @example
;; (a-dash-dash-b '("prog" "arg" "--" "extra"))
;; @result{} ("prog" "arg")
;; @result{} ("extra")
;; @end example
;;
(define (a-dash-dash-b args)
  (let-values (((a b) (break (lambda (s)
                               (string=? "--" s))
                             args)))
    (values a (if (null? b)
                  b
                  ;; Omit "--".
                  (cdr b)))))

;;; a-dash-dash-b.scm ends here
