;; -*-scheme-*-
;;
;; icecast-server.scm - very simplified icecast server
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

(use-modules
 ((ice-9 and-let-star) #:select (and-let*)))

;; server reset callback
(define (icecast-reset server)
  (println "icecast: resetting server")
  0)

(define available (make-object-property))

;; server initialisation
(define (icecast-init server)
  (let* ((files '())
         (directory (svz:server:config-ref server "directory")))
    ;; check directory
    (if (not (icecast-is-directory? directory))
        (begin
          (println "icecast: no such directory `" directory "'")
          -1)
        (begin
          ;; collect files
          (set! files (icecast-find-files directory files))

          ;; state loaded files
          (if (<= (length files) 0)
              (begin
                (println "icecast: no files found in `" directory "'")
                -1)
              (begin
                (println "icecast: " (length files) " file(s) found in `"
                         directory "'")
                (set! (available server) files)
                0)))
        )))

;; checks whether given string argument a directory
(define (icecast-is-directory? directory)
  (catch 'system-error
         (lambda ()
           (eq? (stat:type (stat directory)) 'directory))
         (lambda args
           #f)))

;; checks whether given string argument a regular file
(define (icecast-is-regular? file)
  (catch 'system-error
         (lambda ()
           (eq? (stat:type (stat file)) 'regular))
         (lambda args
           #f)))

;; checks whether given string argument can be opened
(define (icecast-opendir directory)
  (catch 'system-error
         (lambda ()
           (opendir directory))
         (lambda args
           (println "icecast: skipping directory `" directory "'")
           #f)))

;; checks whether given string is a real sub directory
(define (icecast-is-subdirectory? directory base)
  (and (icecast-is-directory? directory)
       (not (equal? base "."))
       (not (equal? base ".."))))

;; check if given file is a mp3 stream
(define (icecast-is-file? file)
  (and (> (string-length file) 4)
       (or (equal? (substring file (- (string-length file) 4)) ".mp3")
           (equal? (substring file (- (string-length file) 4)) ".MP3"))))

;; recurse into directories and find mp3 files
(define (icecast-find-files directory files)
  ;; open directory
  (and-let* ((dir (icecast-opendir directory)))
    (let loop ((file (readdir dir)))
      (or (eof-object? file)
          (let ((full (in-vicinity directory file)))
            (cond ((icecast-is-subdirectory? full file)
                   ;; recurse into directories
                   (set! files (icecast-find-files full files)))
                  ((icecast-is-file? file)
                   ;; filter MP3 files
                   (set! files (cons full files))))
            (loop (readdir dir)))))
    ;; close directory
    (closedir dir))
  files)

;; protocol detection
(define (icecast-detect-proto server sock)
  (or (and-let* ((idx (binary-search (svz:sock:receive-buffer sock) "GET "))
                 ((zero? idx)))
        (println "icecast: client detected at "
                 (inet-ntoa (car (svz:sock:remote-address sock))))
        -1)
      0))

;; per-socket data
(define interesting
  ;; TODO: when B0020 is fixed, this can be simply ‘(make-object-property)’.
  (let ((ht (make-hash-table)))
    ;; interesting
    (lambda (sock . rest)
      (let ((key (svz:sock:ident sock)))
      (if (null? rest)
          (hash-ref ht key)
          (case (car rest)
            ((put) (hash-set! ht key (cadr rest)))
            ((forget) (hash-remove! ht key))))))))

(define (bye-bye sock . reason)
  (interesting sock 'forget)
  0)

;; connecting a client
(define (icecast-connect-socket server sock)
  (let* ((reply (string-append
                 "HTTP/1.0 200 OK\r\n"
                 "Content-Type: audio/x-mp3stream\r\n"
                 "Cache-Control: no-cache\r\n"
                 "Pragma: no-cache\r\n"
                 "Connection: close\r\n"
                 "x-audiocast-name: "
                 (svz:server:config-ref server "server")
                 "\r\n\r\n"))
         (data (make-hash-table 3)))

    ;; save state in socket
    (hash-set! data "seed" (seed->random-state (current-time)))
    ;; TODO: When B0020 is fixed, change to: ‘(set! (interesting sock) data)’
    ;;       and don't bother setting "disconnected" and "kicked" callbacks.
    (interesting sock 'put data)
    (svz:sock:disconnected sock bye-bye)
    (svz:sock:kicked sock bye-bye)

    ;; resize send buffer
    (svz:sock:send-buffer-size sock
                               (svz:server:config-ref server "buffer-size"))

    ;; start coserver callbacks
    (let ((ident (svz:sock:ident sock)))

      (define ((save! k) v)
        (and-let* ((sock (svz:sock:find ident)))
          (hash-set! (interesting sock) k v)))

      (define (kick! key input coserver)
        (coserver input (save! key)))

      (kick! "host" (car (svz:sock:remote-address sock))
             svz:coserver:reverse-dns)
      (kick! "user" sock svz:coserver:ident))

    ;; get first file
    (icecast-next-file sock)

    ;; setup the trigger functionality for mpeg streaming
    (svz:sock:trigger-condition sock icecast-trigger-condition)
    (svz:sock:trigger sock icecast-trigger)

    ;; send initial HTTP header
    (svz:sock:print sock reply)))

;; read part of a file
(define (icecast-readbuffer port size)
  (svz:read-file port size))

;; old *VERY SLOW* read
(define (icecast-readbuffer-snail port size)
  (let loop ((chars '()) (n 0))
    (let ((char (read-char port)))
      (if (or (>= n size) (eof-object? char))
          (list->string (reverse! chars))
          (loop (cons char chars) (1+ n))))))

;; close the currently streamed file
(define (icecast-close-file data)
  (let* ((port (hash-ref data "port")))
    (and (input-port? port) (close port))
    (hash-remove! data "port")
    (hash-remove! data "file")))

;; choose next mpeg file in stream
(define (icecast-next-file sock)
  (let* ((server (svz:sock:server sock))
         (data (interesting sock))
         (seed (hash-ref data "seed"))
         (files (available server))
         (n (random (length files) seed))
         (file (list-ref files n))
         (port '()))

    ;; close current file
    (icecast-close-file data)

    ;; open next file
    (catch #t
           (lambda ()
             (set! port (open-file file "rb")))
           (lambda args
             (println "icecast: unable to open `" file "'")))

    (and (input-port? port)
         (let* ((user (hash-ref data "user"))
                (host (hash-ref data "host"))
                (addr (svz:sock:remote-address sock)))
           (hash-set! data "file" file)
           (hash-set! data "port" port)
           (icecast-id3-tag data)
           (or host
               (set! host (inet-ntoa (car addr))))
           (and host user
                (set! host (fs "~A@~A" user host)))
           (println "icecast: uploading `" file "'"
                    (if host
                        (fs " to ~A" host)
                        ""))))))

;; ensure the send buffer is filled
(define (icecast-trigger-condition sock)
  (let* ((size (svz:sock:send-buffer-size sock))
         (read-bytes (- (car size) (cdr size))))
    (> read-bytes 0)))

;; stream mpeg file data
(define (icecast-trigger sock)
  (let* ((data (interesting sock))
         (size (svz:sock:send-buffer-size sock))
         (read-bytes (- (car size) (cdr size))))

    (let* ((port (hash-ref data "port")) (buffer '()))
      (if (not (input-port? port))
          (icecast-next-file sock)
          (begin
            (set! buffer (icecast-readbuffer port read-bytes))
            (if (eof-object? buffer)
                (icecast-next-file sock)
                (icecast-send-buffer sock buffer)))))
  0))

;; send mp3 data only, strip ID3 tag
(define (icecast-send-buffer sock buffer)
  (let* ((data (interesting sock))
         (pos (hash-ref data "position"))
         (size (hash-ref data "size")))

    (if size
        (set! size (- size pos))
        (set! size (binary-length buffer)))

    (and (> size 0)
         (begin
           (and (> (binary-length buffer) size)
                (set! buffer (binary-subset buffer 0 (- size 1))))
           (hash-set! data "position" (+ pos (binary-length buffer)))
           (svz:sock:print sock buffer)))))

;; detect whether the given file contains ID3 tags
(define (icecast-id3-tag data)
  (let* ((file (hash-ref data "file"))
         (port (hash-ref data "port"))
         (buffer #f) (size #f))
    (hash-set! data "position" 0)
    (hash-set! data "size" size)

    ;; check if mp3 is regular file with at least 128 bytes
    (if (icecast-is-regular? file)
        (begin
          (set! size (stat:size (stat file)))
          (and (> size 128)
               (begin
                 ;; read the last 128 bytes
                 (seek port -128 SEEK_END)
                 (set! buffer (svz:read-file port 128))
                 (seek port 0 SEEK_SET))))
        (println "icecast: `" file "'is not a regular file"))

    (and-let* (size
               buffer
               ((= 128 (binary-length buffer)))
               (found (binary-search buffer "TAG"))
               ((zero? found)))
      (hash-set! data "size" (- size 128))
      (println "icecast: stripping ID3 tag of `" file "'"))))

;; server type definitions
(define-servertype! `(
  (prefix          . "icecast")
  (description     . "guile icecast server")
  (detect-proto    . ,icecast-detect-proto)
  (init            . ,icecast-init)
  (reset           . ,icecast-reset)
  (connect-socket  . ,icecast-connect-socket)
  (configuration   . (
    (directory       . (string  #t "/"))
    (server          . (string  #t "serveez: guile icecast server"))
    (buffer-size     . (integer #t 8192))
  ))))

;; server instantiation
(define-server! 'icecast-server `(
         (directory   . "/usr/local/ftp/pub")
         (server      . "guile-serveez-ice cast server")
         (buffer-size . ,(* 64 1024))
         ))

;; default port configuration
(define-port! 'icecast-port '((proto . tcp)
                              (port  . 8000)))

;; bind server to port
(bind-server! 'icecast-port 'icecast-server)

;;
;; please note: voice-over-ip setup
;;
;; $ mkfifo voip.mp3
;; $ sox -t ossdsp -w -s /dev/audio -t wav - | lame -a -f -b 32 - > voip.mp3
;;
