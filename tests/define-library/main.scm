(define-library (main)
                (import (tests define-library lib1))
                (begin (lib1-func)))
