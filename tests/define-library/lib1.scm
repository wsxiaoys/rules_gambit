(define-library (lib1)
                (import (gambit))
                (export lib1-func)
                (begin
                  (define (lib1-func)
                    (display "Hello from lib1-func")
                    (newline))))
