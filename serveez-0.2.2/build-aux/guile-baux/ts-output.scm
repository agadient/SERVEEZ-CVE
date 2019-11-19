;;; ts-output.scm

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

(define-module (guile-baux ts-output)
  #:export (extract-options-deleting!
            ar<-snippets
            write-ar)
  #:use-module ((guile-baux common) #:select (fs))
  #:use-module ((guile-baux ts-base) #:select (ts:blurb
                                               ts:filename ts:category
                                               ts:sig ts:at ts:options
                                               ts:module ts:name
                                               make-ar
                                               ar:coding ar:dirs
                                               ar:files ar:modules ar:items
                                               MAGIC FINISH FINISH-LEN))
  #:use-module ((guile-baux temporary-file) #:select (temporary-file))
  #:use-module ((srfi srfi-1) #:select (fold
                                        delete-duplicates!
                                        append-map!))
  #:use-module ((srfi srfi-13) #:select (string-prefix-length
                                         string-trim-right))
  #:use-module ((ice-9 regex) #:select (match:substring
                                        match:suffix))
  #:use-module ((ice-9 rw) #:select (read-string!/partial
                                     write-string/partial)))

;; Return an alist of options destructively extracted from @var{ls}, a
;; list of strings, each ending in @code{#\newline}.  An option line
;; begins with @samp{-} (hyphen), followed by a Scheme symbol (the
;; @dfn{key}), @samp{:} (colon), and one or more spaces.  The rest of
;; the line (sans trailing whitespace) constitutes the @dfn{value} (a
;; string).
;;
;; Note that if the first element of @var{ls} is an option line,
;; it will fail to be removed from @var{ls}.
;;
(define extract-options-deleting!
  (let ((rx (make-regexp "^-([^ :]+): +")))
    ;; extract-options-deleting!
    (lambda (ls)
      (let loop ((ls (cons #f ls)) (opts '()))
        (cond ((null? (cdr ls))
               opts)
              ((regexp-exec rx (cadr ls))
               => (lambda (m)
                    (set-cdr! ls (cddr ls))
                    (loop ls (acons (string->symbol (match:substring m 1))
                                    (string-trim-right (match:suffix m))
                                    opts))))
              (else
               (loop (cdr ls) opts)))))))

;; Return a new ar object with @var{coding} containing @var{snippets},
;; a list of ts objects.  This is a wrapper for @code{make-ar} that
;; ensures uniqueness of the filenames, modules and snippets.
;;
(define (ar<-snippets coding snippets)
  (define (unique proc ls)
    (delete-duplicates! (map proc ls)))
  (let ((all-files (unique ts:filename snippets)))
    (make-ar coding
             (unique car all-files)
             all-files
             (unique ts:module snippets)
             snippets)))

;; Return a procedure to measure the actual on-disk byte-length of up
;; to @var{count} strings (as output via @code{display}), and also
;; efficiently send a series of those strings to a port.  The procedure
;; takes one arg, the name of a sub-procedure from the closure to
;; return, one of:
;;
;; @table @code
;; @item stash @var{string}
;; Write @var{string} to a temporary file, recording its actual on-disk
;; byte-length.  Return its (integer) id.
;;
;; @item len @var{id}
;; Return the actual on-disk byte-length (an integer)
;; of the string associated with @var{id}.
;;
;; @item reflow @var{port} @var{list}
;; Send each string in the @var{list} to @var{port}.
;; @end table

(define (reality count)

  ;; Adapted from Guile 1.4.x ‘read-string!/partial/never-fewer’,
  ;; with ‘rd-offset’ required non-#f and ‘wr-offset’ hardcoded to 0.
  (define (r-s!/p/n-f buf port rd-offset len)
    (define (smore-please start)
      (read-string!/partial buf port start len))
    (seek port rd-offset SEEK_SET)
    (let loop ((this-time (smore-please 0))
               (so-far 0))
      (and (< (+ so-far this-time) len)
           (loop (smore-please so-far)
                 (+ so-far this-time)))))

  ;; Adapted from Guile 1.4.x ‘write-string/partial/never-fewer’,
  ;; with all args required, ‘start’ hardcoded to 0 and unspecified rv.
  (define (w-s/p/n-f str port end)
    (let ((b 0))
      (let loop ()
        (set! b (+ b (write-string/partial str port b end)))
        (or (= b end)
            (loop)))))

  (let ((p (temporary-file))
        (v (make-vector count))
        (biggest 0))

    (define serial
      (let ((n -1))
        (lambda ()
          (set! n (1+ n))
          n)))

    (define (stash s)
      (let* ((beg (ftell p))
             (len (begin (display s p)
                         (- (ftell p) beg)))
             (id (serial)))
        (vector-set! v id (cons beg len))
        (set! biggest (max len biggest))
        id))

    (define (len id)
      (cdr (vector-ref v id)))

    (define (reflow port ids)
      (let ((stage (make-string biggest)))

        (define (shuffle! id)
          (let* ((pair (vector-ref v id))
                 (beg (car pair))
                 (len (cdr pair)))
            ;; breathe in
            (r-s!/p/n-f stage p beg len)
            ;; breathe out
            (w-s/p/n-f stage port len)))

        (for-each shuffle! ids)))

    ;; rv
    (lambda (proc)
      (case proc
        ((stash) stash)
        ((len) len)
        ((reflow) reflow)
        (else (error "bad proc:" proc))))))

;; Serialize @var{ar} to output @var{port}.
;;
(define (write-ar ar port)

  (define (fsp s . args)
    (apply simple-format port s args))

  (define (find-root-directory dirs)
    (if (null? dirs)
        ""
        (fold (lambda (dir root)
                (substring root 0 (string-prefix-length root dir)))
              (car dirs)
              (cdr dirs))))

  (define (write-block count ls)
    (fsp "~A~A~%" #\np count)
    (for-each (lambda (x)
                (fsp "~S~%" x))
              ls))

  (define (neck-info stash idx-of)
    (define (f-squashed ts)
      (idx-of (ts:filename ts)))
    (lambda (ts blurb-size)
      (stash (fs "~A~S~%"
                 #\np
                 (list blurb-size
                       (f-squashed  ts)
                       (ts:category ts)
                       (ts:sig      ts)
                       (ts:at       ts)
                       (ts:options  ts))))))

  (let* ((dirs    (ar:dirs ar))
         (files   (ar:files ar))
         (modules (ar:modules ar))
         (d-count (length dirs))
         (f-count (length files))
         (m-count (length modules))
         (idx-of  (let ((ht (make-hash-table)))
                    (define (idx! len ls)
                      (for-each (lambda (x i)
                                  (hash-set! ht x i))
                                ls (iota len)))
                    (idx! d-count dirs)
                    (idx! f-count files)
                    (idx! m-count modules)
                    (lambda (x)
                      (hash-ref ht x))))
         (root    (find-root-directory dirs))
         (items   (ar:items ar))
         (i-count (length items))
         (on-disk (reality (+ 2 (* 2 i-count))))
         (stash   (on-disk 'stash))
         (o-d-len (on-disk 'len))
         (b-ids   (map stash (map ts:blurb items)))
         (b-len   (map o-d-len b-ids))
         (n-ids   (map (neck-info stash idx-of) items b-len)))

    (fsp "~A~A ; -*- mode: ~A; coding: ~A; -*-~%"
         MAGIC (integer->char 1)
         'text (ar:coding ar))
    (fsp "~S~%" root)
    (write-block d-count (map (let ((redundant (o-d-len (stash root))))
                                (lambda (s)
                                  (substring s redundant)))
                              dirs))
    (write-block f-count (map (lambda (split)
                                (cons (idx-of (car split))
                                      (cdr split)))
                              files))
    (write-block m-count modules)
    (write-block i-count
                 (map (lambda (ofs ts)
                        (cons* ofs
                               (idx-of (ts:module ts))
                               (ts:name ts)))
                      ;; ofs
                      (reverse!
                       (cdr (fold (lambda (n-id b-len so-far)
                                    (cons (+ (car so-far)
                                             (o-d-len n-id)
                                             b-len
                                             FINISH-LEN)
                                          so-far))
                                  (list 0)
                                  n-ids b-len)))
                      ;; ts
                      items))
    ((on-disk 'reflow)
     port (append-map!
           list
           n-ids b-ids (make-list i-count (stash FINISH))))))

;;; ts-output.scm ends here
