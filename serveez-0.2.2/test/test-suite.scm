;; test-suite.scm - test suite library
;;
;; Copyright (C) 2011-2013 Thien-Thi Nguyen
;; Copyright (C) 2002, 2003 Stefan Jahn <stefan@lkcc.org>
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

(define-module (test-suite)
  #:export (test-suite
            run-test-suite
            run-test
            pass-if
            pass-if-exception))

(and (defined? 'micro-version)
     (use-modules (guile-user)))

(define VERBOSE? (equal? "1" (getenv "VERBOSE")))

(define BADNESS #f)

(define PAD (make-string 40 #\space))

(define (fso s . args)
  (apply simple-format #t s args))

(define (run-test description exception test)

  (define (report blurb)
    (or (string=? "ok" blurb)
        (set! BADNESS (1+ BADNESS)))
    (and VERBOSE?
         (fso "~A~A: ~A\r~%"
              (substring PAD (string-length description))
              description
              blurb)))

  (catch #t (lambda () (throw (if (equal? #t (test))
                                  'pass
                                  'fail)))
         (lambda (key . args)
           (case key
             ((pass) (report "ok"))
             ((fail) (report "failed"))
             ((quit) (report "fatal failure (exited)"))
             (else (cond ((eq? key exception)
                          (report "ok"))
                         (else
                          (report "unexpected exception")
                          (apply throw key args))))))))

(define-macro (pass-if description test)
  `(run-test ,description 'invalid (lambda () ,test)))

(define-macro (pass-if-exception description exception test)
  `(run-test ,description ,exception (lambda () ,test)))

(define (run-test-suite title test)
  (serveez-verbosity 0)
  (serveez-exceptions #f)
  (and VERBOSE? (fso "~A\r~%" title))
  (set! BADNESS 0)
  (test)
  (serveez-nuke BADNESS))

(define-macro (test-suite title . test)
  `(run-test-suite ,title (lambda () ,@test)))

;;; test-suite.scm ends here
