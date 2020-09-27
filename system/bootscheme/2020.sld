(define-library (bootscheme 2020)
  (export + - * /
          < > <= >= =

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

          ;;

          command-args
          command-name

          ;;

          first-line-from-command

          )
  (import (scheme base)
          (scheme file)
          (scheme process-context)
          (scheme read)
          (scheme write))

  (begin
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
       (define (first-line-from-command command . args)
         (string-first-line
          (process->string (cons command args))))))
    (gambit
     (import (gambit))
     (begin
       (define (command-name)
         (let ((name (car (command-line))))
           (and name (path-strip-extension (path-strip-directory name)))))
       (define (command-args) (cdr (command-line)))
       (define (first-line-from-command command . args)
         (let* ((p (with-exception-handler (lambda (err) #f)
                     (lambda ()
                       (open-process
                        (list (string->keyword "path") command
                              (string->keyword "arguments") args
                              (string->keyword "stdout-redirection") #t
                              (string->keyword "stderr-redirection") #t))))))
           (and p
                (let ((line (read-line p)))
                  (close-port p)
                  (and (equal? 0 (process-status p))
                       line)))))))
    (gauche
     (import (srfi 13) (srfi 193))
     (import (gauche base) (gauche process))
     (begin
       (define (first-line-from-command command . args)
         (let ((p (run-process (cons command args)
                               (make-keyword 'redirects)
                               `((> 1 out) (> 2 ,(make-keyword 'null))))))
           (and (process-wait p)
                (string-first-line
                 (port->string (process-output p 'out))))))))))
