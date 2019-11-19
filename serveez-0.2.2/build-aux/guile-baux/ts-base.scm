;;; ts-base.scm

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

(define-module (guile-baux ts-base)
  #:export (split-filename
            unsplit

            make-ts
            ts:name ts:module ts:filename ts:blurb
            ts:category ts:sig ts:at ts:options

            make-ar
            ar:coding ar:dirs ar:files ar:modules ar:items

            MAGIC FINISH FINISH-LEN
            read-ar-file)

  #:use-module ((srfi srfi-9) #:select (define-record-type))
  #:use-module ((ice-9 and-let-star) #:select (and-let*))
  #:use-module ((ice-9 rdelim) #:select (read-line))
  #:use-module ((ice-9 regex) #:select (match:substring))
  #:use-module ((ice-9 rw) #:select (read-string!/partial)))

;; Split @var{filename} into its directory and non-directory portions.
;; Return a pair @code{(@var{dir} . @var{non-dir})}, where @var{dir}
;; always ends with @samp{/} (slash).
;;
(define (split-filename filename)
  (cons (string-append (dirname filename) "/")
        (basename filename)))

;; Return a new string made from appending the @sc{car}
;; and @sc{cdr} of @var{pair} (both strings).
;;
(define (unsplit pair)
  (string-append (car pair) (cdr pair)))

;; {texinfo snippet}
;;
;; The @dfn{texinfo snippet} record type---@samp{ts} for short---has
;; fields oriented towards procedure (and ilk, which ``take arguments'')
;; definitions, but capable of handling also variable (and ilk, which
;; are values, and do not take arguments) definitions and @dfn{titled
;; text blocks}.  Fields are:
;;
;; @table @code
;; @item name
;; symbol or string
;;
;; @item module
;; module name (i.e., list of symbols)
;;
;; @item filename
;; pair whose @sc{car} is the @code{dirname} and @sc{cdr}
;; the @code{basename} of the file where the snippet originates
;;
;; @item blurb
;; snippet itself (string)
;;
;; @item category
;; symbol, string, or @code{#f}
;;
;; @item sig
;; (if arguments) either a proper list of symbols, or the vector
;; @code{#(@var{req} @var{opt} @var{var} [@var{name}@dots{}])},
;; where @var{req}, @var{opt} and @var{var} are non-negative integers
;; whose sum corresponds with the number of @var{name} symbols
;; (or list of symbols aggregating all ``keyword argument'' names)
;;
;; (if no arguments) @code{#f}
;;
;; @item at
;; vector @code{#(@var{lno} @var{col} @var{beg} @var{end})},
;; where @var{lno} and @var{col} are line and column numbers,
;; respectively, and @var{beg} and @var{end} are file positions
;; (non-negative byte offsets) that delimit the documented form
;;
;; @item options
;; alist of options scanned by @samp{tsar}
;; @end table
;;
;; There is one constructor, an accessor for each field, and no modifier
;; procedures.

(define-record-type ts (make-ts name module filename blurb
                                category sig at options) ts?
  (name     ts:name)
  (module   ts:module)
  (filename ts:filename)
  (blurb    ts:blurb)
  (category ts:category)
  (sig      ts:sig)
  (at       ts:at)
  (options  ts:options))

;; {archive}
;;
;; The @dfn{archive} record type (@samp{ar} for short) holds texinfo snippets
;; and their metadata.  Its contents are saved on disk.
;;
;; @table @code
;; @item coding
;; symbol
;; @item dirs
;; list of directory names, each ending with @samp{/} (slash)
;; @item files
;; list of pairs whose @sc{car} is a directory
;; and whose @sc{cdr} is a basename
;; @item modules
;; list of module names (each a list of symbols)
;; @item items
;; list of texinfo snippets
;; @end table
;;
;; There is one constructor, an accessor for each field, and no modifier
;; procedures.

(define-record-type ar (make-ar coding dirs files modules items) ar?
  (coding  ar:coding)
  (dirs    ar:dirs)
  (files   ar:files)
  (modules ar:modules)
  (items   ar:items))

;; The four-byte string "^T^S^A^R".
;;
;;-category: constant string
;;
(define MAGIC (list->string
               (map (lambda (c)
                      (integer->char (- (char->integer c)
                                        (char->integer #\a)
                                        -1)))
                    (string->list "tsar"))))

;; The two-byte string "^_\n".
;;
;;-category: constant string
;;
(define FINISH (string #\us #\newline))

;; The byte length of @code{FINISH}.
;;
;;-category: constant integer
;;
(define FINISH-LEN (string-length FINISH))

(define (open-input-file/fully-buffered filename)
  ;; This works around a bug in Guile 1.8.7 whereby ‘read-string!/partial’
  ;; for synching ‘FINISH’ gets confused, omits the newline and returns 1.
  ;; Another solution is to completely unbuffer, but we prefer to be lame
  ;; in the other direction, risking failure on oversize files, for speed.
  (let ((port (open-input-file filename)))
    (setvbuf port _IOFBF (stat:size (stat port)))
    port))

(define check-magic
  (let ((want (string-length MAGIC)))
    (lambda (port)
      (let loop ((acc '()) (count 0))
        (if (= want count)
            (string=? MAGIC (apply string (reverse! acc)))
            (let ((c (read-char port)))
              (and (not (eof-object? c))
                   (loop (cons c acc) (1+ count)))))))))

(define get-coding
  (let ((rx (make-regexp "coding: ([^;]+);" regexp/extended)))
    (lambda (port)
      (and-let* ((line (read-line port))
                 ((string? line))
                 (m (regexp-exec rx line)))
        (string->symbol (match:substring m 1))))))

(define (read-top port)
  (cond ((not (check-magic port)) "not a tsar file (bad magic)")
        ((get-coding port))
        (else "could not find coding")))

;; Read @var{filename} as a texinfo snippet archive.
;; The file must declare its encoding to be @var{expected-coding}.
;; On encoding mismatch or other error, call @var{bummer} with
;; a format string and args.
;;
;; If @var{for-merge?}, return four values (each a list of):
;; directories (string), files (split), modules (list of symbols) and items
;; (texinfo snippet object).
;;
;; Otherwise, return a single @code{ar} object, whose @code{modules} and
;; @code{items} members are hash tables.
;;
(define (read-ar-file bummer expected-coding for-merge? filename)
  ;; TODO: Change ‘for-merge?’ to vector of merge procs.
  ;; TODO: Partial curry: ((read-ar-file ....) filename).
  (let* ((synch (make-string FINISH-LEN))
         (p (open-input-file/fully-buffered filename))
         (coding (read-top p)))

    (define (corruption! back what)
      (bummer "~A:~A: tsar corruption, byte ~A (~A)"
              filename (1+ (port-line p))
              (- (ftell p) back) what))

    (define (read/nl)
      (let ((x (read p)))
        ;; Check newline.
        (or (char=? #\newline (read-char p))
            (corruption! 1 "missing LF"))
        x))

    (define (read-block munge)
      (let* ((count (read/nl))
             (v (make-vector count)))
        (do ((i 0 (1+ i)))
            ((= i count) v)
          (vector-set! v i (munge (read/nl))))))

    (define (read-block-directly)
      (read-block identity))

    (define (munge-prefixing root)
      (lambda (rel-dir)
        (string-append root rel-dir)))

    (define (munge-file-pair-proc dirs)
      (lambda (pair)
        (set-car! pair (vector-ref dirs (car pair)))
        pair))

    (define (munge-item-head-proc modules)
      (lambda (form)
        ;; Discard body offset ‘(car form)’.
        ;; TODO: Stash it if not ‘for-merge?’.
        (let ((pair (cdr form)))
          (set-car! pair (vector-ref modules (car pair)))
          pair)))

    ;; Some sanity checks.
    (or (symbol? coding)
        (bummer "~A: ~A" filename coding))
    (or (eq? expected-coding coding)
        (bummer "~A:1: coding mismatch (expecting ~S, got ~S)"
                filename expected-coding coding))
    (let* ((root (read/nl))
           (dirs (read-block (munge-prefixing root)))
           (files (read-block (munge-file-pair-proc dirs)))
           (modules (read-block-directly))
           (items (read-block (munge-item-head-proc modules)))
           (item-count (vector-length items))
           (ht (or for-merge? (make-hash-table))))

      (define (f-ref n)
        (vector-ref files n))

      (do ((i 0 (1+ i)))
          ((= item-count i))
        (let* ((pair (vector-ref items i))
               (module (car pair))
               (name (cdr pair))
               (neck (read/nl))
               (blurb-len (car neck))
               ;; TODO: Save promise if not ‘for-merge?’.
               (blurb (make-string blurb-len)))
          (let loop ((wpos 0))
            (and (< wpos blurb-len)
                 (and=> (read-string!/partial blurb p wpos)
                        (lambda (more)
                          (loop (+ wpos more))))))
          ;; Check ‘FINISH’.
          (or (and (eq? FINISH-LEN (read-string!/partial synch p))
                   (string=? FINISH synch))
              (corruption! 2 "bad blurb finish"))
          (let* ((rest (cdr neck))
                 (ts (apply make-ts name module (f-ref (car rest))
                            blurb (cdr rest))))
            (if for-merge?
                (vector-set! items i ts)
                (hash-set! ht (cons name module) ts)))))

      ;; TODO: Don't close if not ‘for-merge?’.
      (close-port p)
      (set! dirs    (vector->list dirs))
      (set! files   (vector->list files))
      (set! modules (vector->list modules))
      (if for-merge?
          (values dirs files modules (vector->list items))
          (make-ar expected-coding dirs files modules ht)))))

;;; ts-base.scm ends here
