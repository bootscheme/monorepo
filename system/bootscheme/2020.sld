(define-library (bootscheme 2020)
  (export

   ;; R7RS-small punctuation
   + - * /
   < > <= >= =

   ;; R7RS-small alphanumeric
   and
   apply
   assoc
   caar
   cadr
   car
   case
   cdar
   cddr
   cdr
   char=?
   command-line
   cond
   cons
   current-error-port
   current-input-port
   current-output-port
   define
   display
   else
   eq?
   equal?
   eqv?
   error
   exit
   flush-output-port
   for-each
   get-environment-variable
   if
   lambda
   let
   let*
   list
   list-ref
   make-string
   map
   max
   member
   min
   newline
   not
   null?
   or
   parameterize
   quote
   reverse
   set!
   string
   string-append
   string-length
   string-ref
   string=?
   string?
   substring
   symbol?
   with-output-to-file
   write
   write-string

   ;; SRFIs that are not in R7RS-small
   string-contains
   command-args
   command-name

   ;; Not in RnRS or SRFIs
   write-line
   all-output-from-command
   first-line-from-command

   )
  (import (scheme base)
          (scheme file)
          (scheme process-context)
          (scheme read)
          (scheme write))

  (begin

    (define (write-line s)
      (write-string s)
      (newline))

    (define (string-contains big lil)
      (let ((lasti (- (string-length big) (string-length lil))))
        (and (>= lasti 0)
             (let outer ((i 0))
               (and (<= i lasti)
                    (let inner ((k 0))
                      (if (= k (string-length lil))
                          i
                          (if (char=? (string-ref big (+ i k))
                                      (string-ref lil k))
                              (inner (+ k 1))
                              (outer (+ i 1))))))))))

    (define (string-first-line s)
      (let loop ((i 0))
        (cond ((= i (string-length s)) s)
              ((char=? #\newline (string-ref s i)) (substring s 0 i))
              (else (loop (+ i 1)))))))

  (cond-expand
    (chibi
     (import (srfi 193))
     (import (chibi process))
     (begin
       (define (all-output-from-command command . args)
         (process->string (cons command args)))
       (define (first-line-from-command command . args)
         (string-first-line (apply all-output-from-command command args)))))
    (gambit
     (import (gambit))
     (begin
       (define (read-whole-string port)
         (let loop ((acc ""))
           (let ((line (read-line port)))
             (if (eof-object? line) acc
                 (loop (string-append acc line (string #\newline)))))))
       (define (command-name)
         (let ((name (car (command-line))))
           (and name (path-strip-extension (path-strip-directory name)))))
       (define (command-args) (cdr (command-line)))
       (define (all-output-from-command command . args)
         (let* ((p (with-exception-handler (lambda (err) #f)
                     (lambda ()
                       (open-process
                        (list (string->keyword "path") command
                              (string->keyword "arguments") args
                              (string->keyword "stdout-redirection") #t
                              (string->keyword "stderr-redirection") #t))))))
           (let ((s (read-whole-string p)))
             (close-port p)
             (and (equal? 0 (process-status p)) s))))
       (define (first-line-from-command command . args)
         (string-first-line (apply all-output-from-command command args)))))
    (gauche
     (import (srfi 13) (srfi 193))
     (import (gauche base) (gauche process))
     (begin
       (define (all-output-from-command command . args)
         (let ((p (run-process (cons command args)
                               (make-keyword 'redirects)
                               `((> 1 out) (> 2 ,(make-keyword 'null))))))
           (and (process-wait p)
                (port->string (process-output p 'out)))))
       (define (first-line-from-command command . args)
         (string-first-line (apply all-output-from-command command args)))))))
