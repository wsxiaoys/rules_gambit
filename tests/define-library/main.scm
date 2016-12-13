(define-library (main)
                (import (gambit))
                (import (tests define-library lib1))
                (import (tests define-library when))
                (begin 
                  (define run #t)
                  (when run 
                    (lib1-func))))
