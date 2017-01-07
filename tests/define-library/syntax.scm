(define-library (syntax)
  (import (only (gambit)
                if not begin)) ;; required by expansions of when and unless

  (export when addn unless)

  (begin

    (define-syntax addn
      (macro (a b)
        `(##+ ,a ,b)))

    (define-syntax unless
      (lambda (x)
        (syntax-case x ()
          ((_ test e e* ...)
           #'(if (not test) (begin e e* ...))))))

    (define-syntax when
      (syntax-rules ()
        ((_ test expr expr* ...)
         (if test (begin expr expr* ...)))))))
