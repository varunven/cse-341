#lang racket

;; This line exports all of your defined functions,
;; so you can call them in hw4tests.rkt and so we can
;; call them in our tests.
;; Don't remove or comment out this line!
(provide (all-defined-out))

(define ones (lambda () (cons 1 ones)))

(define nats-for-test
  (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

;1
(define (sequence spacing low high)
  (if(> low high) null
     (cons low (sequence spacing (+ low spacing) high)
      )
   )
)

;2
(define (string-append-map xs suffix)
  (map (lambda(x) (string-append x suffix)) xs
  )
)

;3
(define (list-nth-mod xs n)
  (cond((< n 0) (error "list-nth-mod: negative number"))
       ((null? xs) (error "list-nth-mod: empty list"))
  (#t (car(list-tail xs (remainder n (length xs)))))
  )
)

;4
(define (stream-first-k-such-that f k s)
  (if(<= k 0) null
  (cons (car (s)) (stream-first-k-such-that f (- k 1) (cdr (s))))
  )
)

;5
(define funny-number-stream
  (letrec ((f (lambda(x)
                (cons (if (= (remainder x 6) 0) (- x) x)
                      (lambda () (f(+ x 1)))
                )
               )
              )
           )
    (lambda () (f 1))
   )
 )

;6
(define dan-then-dog
  (letrec ((f (lambda (x)
                (cons x (lambda() (f (if (eq? x "dan.jpg") "dog.jpg" "dan.jpg")))))
               )
          )
    (lambda () (f "dan.jpg"))
  )
 )

;7
(define (stream-add-one s)
  (letrec ([f (lambda(x)
                (cons(cons 1 (car(x))) (lambda() (f (cdr (x)))))
               )
           ])
    (lambda () (f s))
   )
)

;8
(define (cycle-lists xs ys) 
  (define (helper n) (lambda () (cons (cons (list-nth-mod xs n) (list-nth-mod ys n))
                                      (helper (+ n 1)))))
  (helper 0)
)

;9
(define (vector-assoc v vec)
  (letrec ([ f (lambda (n)
                 (cond [(<= (vector-length vec) n) #f]
                       [(pair? (vector-ref vec n)) (letrec ([x (vector-ref vec n)])
                                                     (if (equal? v (car x))
                                                         x
                                                         (f (+ n 1))
                                                      ))
                        ]
                       [#t (f (+ n 1))]
                  )
                )
           ])
    (f 0)
   )
 )

;10
(define (caching-assoc xs n)
  (letrec([m (make-vector n #f)]
          [count 0])
    (lambda (x)
      (let ([result (vector-assoc x m)])
        (if result
            result
            (let ([nresult (assoc x xs)])
              (if (not nresult)
                  nresult
                  (begin
                    (vector-set! m count nresult)
                    (if (= count( - n 1))
                        (set! count 0)
                        (set! count (+ count 1)))
                    nresult
                    )
                  )
              )
            )
        )
      )
    )
  )

;11
(define (while-greater e1 do e2)
     (letrec (
              [result1 e1]
              [f (lambda() (let ([result2 e2])
                   (if (> result2 result1)
                       (f)
                       #t
                    ))
                  )
               ]
             )
       (f)
      )
  )