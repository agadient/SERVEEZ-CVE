;;; file-newer-than.scm

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

(define-module (guile-baux file-newer-than)
  #:export (file-newer-than))

;; Return @code{#t} if @var{a} is newer than @var{b}.
;; Both @var{a} and @var{b} may be a filename (string)
;; or an object returned from @code{stat}.
;; If @var{a} does not exist, the answer is @code{#f};
;; otherwise, if @var{b} does not exist, the answer is @code{#t}.
;;
;; Optional arg @var{component} specifies a procedure to use instead
;; of the default @code{stat:mtime}.  It should return a numeric value.
;;
;;-args: (- 1 0 component)
;;
(define (file-newer-than a b . opts)
  (call-with-current-continuation
   (lambda (return)

     (define (norm x easy-answer)
       (cond ((not (string? x)) x)
             ((file-exists? x)  (stat x))
             (else              (return easy-answer))))

     (set! a (norm a #f))
     (set! b (norm b #t))
     (let ((component (if (null? opts)
                          stat:mtime
                          (car opts))))
       (> (component a)
          (component b))))))

;;; file-newer-than.scm ends here
