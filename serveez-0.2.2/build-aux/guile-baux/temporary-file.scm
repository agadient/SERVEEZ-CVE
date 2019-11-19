;;; temporary-file.scm

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

(define-module (guile-baux temporary-file)
  #:export (unlink-port-filename
            unique-i/o-file-port
            temporary-file))

;; Remove the filename associated with @var{port} from the filesystem.
;; Optional arg @var{close?} non-@code{#f} means do @code{close-port}
;; as well (prior to the actual unlink operation).
;;
;;-args: (- 1 0)
;;
(define (unlink-port-filename port . close?)
  (let ((filename (port-filename port)))
    (and (pair? close?)
         (car close?)
         (close-port port))
    (and (string? filename)
         (file-exists? filename)
         (delete-file filename))))

;; Return a new i/o port opened on a file named using string
;; @var{base}.  The actual assigned filename, which can be
;; retrieved using @code{port-filename}, is @var{base} appended
;; with six random characters.  Optional arg @var{suffix} is a
;; string to append to the assigned filename.  For example:
;;
;; @lisp
;; (define p (unique-i/o-file-port "/tmp/foo-" ".c"))
;; (port-filename p)
;; @result{} "/tmp/foo-hoQtxh.c"
;; @end lisp
;;
;; Implementation/portability note: This procedure is a wrapper
;; around @code{mkstemp} and @code{mkstemp!} (tried in that order).
;;
;;-args: (- 1 0)
;;
(define (unique-i/o-file-port base . suffix)
  (let ((port (or (false-if-exception (mkstemp base))
                  (mkstemp! (string-append base "XXXXXX")))))
    (or (null? suffix)
        (let* ((old (port-filename port))
               (new (string-append old (car suffix))))
          (rename-file old new)
          (set-port-filename! port new)))
    port))

;; Return an input/output port to a unique temporary file named
;; using the directory prefix @code{P_tmpdir} defined in
;; @file{stdio.h}.  The file is automatically deleted when the
;; port is closed or the program terminates.
;; You can use the environment variable @samp{TMPDIR} to override
;; the default directory prefix.
;;
;; Implementation/portability note: This procedure is a wrapper
;; around @code{tmpfile} and @code{unique-i/o-file-port} (tried
;; in that order), falling back to a ``manual'' implementation if
;; all else fails.  The filename portion of the prefix (i.e., the
;; non-directory part) is unspecified for @code{tmpfile}; for the
;; other approaches, this is computed as:
;;
;; @lisp
;; (or (car (command-line))
;;     "tmp")
;; @end lisp
;;
(define (temporary-file)

  (define (base suffix)
    (in-vicinity (or (getenv "TMPDIR")
                     "/tmp")
                 (string-append (basename (or (car (command-line))
                                              "tmp"))
                                suffix)))

  (define (fake port)

    (define (zonk!)
      (unlink-port-filename port)
      #t)

    ;; Unlink immediately, ...
    (zonk!)
    ;; ... but don't trust things; check anyway.
    (and=> (false-if-exception (and add-hook! exit-hook))
           (lambda (hook)
             (add-hook! hook zonk!)))
    ;; rv
    port)

  (or (false-if-exception (tmpfile))
      (false-if-exception (fake (unique-i/o-file-port (base "."))))
      ;; If the last fallback throws an exception, don't mask it.
      (fake (let* ((now (gettimeofday))
                   (big (* (getpid) (car now) (if (zero? (cdr now))
                                                  1
                                                  (cdr now))))
                   (s (seed->random-state big))
                   (suffix (number->string (random big s) 36)))
              (open-file (base (string-append "." suffix)) "w+b")))))

;;; temporary-file.scm ends here
