;;; Copyright (C) 2019-2020 Rob Browning <rlb@defaultvalue.org>
;;; SPDX-License-Identifier: LGPL-2.1-or-later OR EPL-1.0+

;; Note: this currently cannot depend on core, since core pulls some
;; functions like read-line from here.

(define-module (lokke io)
  #:version (0 0 0)
  #:use-module ((ice-9 binary-ports) #:select (get-bytevector-all put-bytevector))
  #:use-module ((ice-9 rdelim) #:select ((read-line . %scm-read-line)))
  #:use-module ((ice-9 textual-ports) #:select (get-string-all))
  #:use-module (oop goops)
  #:use-module ((lokke base syntax) #:select (when-not))
  #:use-module ((lokke collection) #:select (cons lazy-seq))
  #:use-module ((lokke exception) #:select (with-open))
  #:use-module ((lokke hash-map) #:select (get hash-map))
  #:use-module ((lokke pr) #:select (*in* *out* str))
  #:duplicates (merge-generics replace warn-override-core warn last)
  #:re-export (delete-file)
  #:export (copy
            flush
            line-seq
            mkstemp
            read-line
            reader
            slurp
            slurp-bytes
            spit
            spit-bytes
            writer))

;; FIXME: support binary paths somehow, without having to set the
;; LC_CTYPE to ISO-8859-1.

(define (read-line)
  (%scm-read-line *in*))

(define-method (copy (input <string>) (output <string>))
  ;; We just ignore buffer-size for now
  ;; FIXME: support file descriptors?
  ;; FIXME: support ports?
  ;; FIXME: sendfile?
  (copy-file input output))

;; FIXME: provide some way to specify Guile's full mode to reader/writer?
;; FIXME: do we want some way to specify #:guess-encoding?

(define-method (reader (path <string>) . opts)
"Returns an open input port for the file specified by the path in
POSIX binary mode if :binary is true, text mode otherwise.  When in
binary mode the encoding will be ISO-8859-1.  If both :binary and
encoding are false (the default), guesses the encoding as decribed in
Guile's \"File Ports\" documentation."
  (let* ((opts (apply hash-map opts))
         (enc (get opts #:encoding #f))
         (bin? (get opts #:binary #f)))
    (open-input-file path #:encoding enc #:binary bin?)))

(define-method (reader (p <input-port>)) p)

(define-method (writer (path <string>) . opts)
  "Returns an open output port for the file specified by the path in
POSIX binary mode if :binary is true, text mode otherwise.  When in
binary mode the encoding will be ISO-8859-1.  If both :binary and
encoding are false (the default), guesses the encoding as decribed in
Guile's \"File Ports\" documentation.  Opens the file in POSIX append
mode if the :append option is set to true."
  (let* ((opts (apply hash-map opts))
         (enc (get opts #:encoding #f))
         (bin? (get opts #:binary #f))
         (append? (get opts #:append #f)))
    (open-file path
               (if bin?
                   (if append? "ab" "wb")
                   (if append? "a" "w"))
               #:encoding enc
               #:binary bin?)))

(define-method (writer (p <output-port>)) p)

(define (slurp f . opts)
  "Calls reader on f and then returns the content of the resulting
port as a string.  Closes the port."
  (with-open (in (apply reader f opts))
    (get-string-all in)))

(define (spit f content . opts)
  "Calls writer on f, writes (str content) on the resulting port, and
then closes it."
  (with-open (out (apply writer f opts))
    (display (str content) out)))

(define (slurp-bytes f . opts)
  "Returns a bytevector containing the contents of f, otherwise
exactly like slurp."
  (with-open (in (apply reader f opts))
    (get-bytevector-all in)))

(define (spit-bytes f bytevector . opts)
  "Writes the bytevector to f, otherwise exactly like spit."
  (with-open (out (apply writer f opts))
    (put-bytevector out bytevector)))

(define* (mkstemp template #:optional mode)
  "Behaves exactly like Guile's mkstemp! except that the template is
not modified.  The filename will be available via
guile.ice-9.ports/port-filename until the port is closed."
  (mkstemp! (string-copy template)))

(define (line-seq rdr)
  (let loop ()
    (lazy-seq
     (let ((l (%scm-read-line rdr)))
       (when-not (eof-object? l)
         (cons l (loop)))))))

(define (flush)
  (force-output))
