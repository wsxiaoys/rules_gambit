(define-library (main)
                (import (gambit))
                (import (tests define-library lib1))
                (import (tests define-library syntax))
                (begin 
                  (define run #t)
                  (define z 5)

                  (display "calling syntax-rules ")
                  (when run 
                    (lib1-func))

                  (display "calling syntax-case ")
                  (unless (not run)
                    (lib1-func))

                  (display "calling macro ")
                  (when run
                    (display (addn z 100))
                    (newline))))
