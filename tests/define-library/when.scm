(define-library (when)
  (import (only (gambit)
                if not begin)) ;; required by expansions of when and unless

  (export when)

  (begin

    (define-syntax when
      (syntax-rules ()
        ((_ test expr expr* ...)
         (if test (begin expr expr* ...)))))))
