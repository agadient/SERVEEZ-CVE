;;; filenamez.scm

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

(define-module (guile-baux filenamez)
  #:export (read-filenamez
            filenamez<-file)
  #:use-module ((ice-9 rdelim) #:select (read-delimited)))

;; Read and return a list of NUL-terminated filenames from @var{port}.
;;
(define (read-filenamez port)
  (let loop ((acc '()))
    (let ((fn (read-delimited "\0" port)))
      (cond ((eof-object? fn)
             acc)
            (else
             (loop (cons fn acc)))))))

;; Read and return a list of NUL-terminated filenames
;; from input file @var{filename}.
;;
(define (filenamez<-file filename)
  (let* ((p (open-input-file filename))
         (rv (read-filenamez p)))
    (close-port p)
    rv))

;;; filenamez.scm ends here
