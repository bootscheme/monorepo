(define-library (bootscheme)
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
          for-each
          if
          lambda
          let
          let*
          list
          max
          member
          min
          newline
          not
          null?
          or
          parameterize
          quote
          string-append
          string-length
          string-ref
          string=?
          substring
          with-output-to-file
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

  (cond-expand
    (gambit
     (import (gambit))
     (begin
       (define (first-line-from-command command . args)
         (let* ((process (start-process command args))
                (line (read-line process))
                (status (wait-for-process process)))
           (and (exit-success? status) line)))))
    (gauche
     (import (srfi 13) (srfi 193))
     (import (gauche base) (gauche process))
     (begin
       (define (first-line-from-command command . args)
         (let ((p (run-process (cons command args)
                               (make-keyword 'redirects)
                               `((> 1 out) (> 2 ,(make-keyword 'null))))))
           (and (process-wait p)
                (let* ((out (port->string (process-output p 'out)))
                       (end (string-index out #\newline)))
                  (if end
                      (substring out 0 end)
                      (string-append out (string #\newline)))))))))))
