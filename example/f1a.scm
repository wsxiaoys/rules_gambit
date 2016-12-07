(c-declare "extern void f1b_hello();")

(define f1b-hello
    (c-lambda () void "f1b_hello"))


(define (f1a-func)
    (display "f1a-func was called\n")
      (f1b-hello))
