#lang racket

(require "hw5.rkt")

; This file uses Racket's unit-testing framework, which is convenient but not required of you.

; Note we have provided [only] 3 tests, but you can't run them until do some of the assignment.
; You will want more tests.

(require rackunit)

(define tests
  (test-suite
   "Homework 5 Tests"

   (check-equal? (eval-exp (add (int 2) (int 2))) (int 4) "add test")

   (check-exn (lambda (x) (string=? (exn-message x) "MUPL addition applied to non-number"))
              (lambda () (eval-exp (add (int 2) (munit))))
              "add bad argument")

   (check-equal? (mupllist->racketlist
                  (eval-exp (call (call mupl-all-gt (int 9))
                                  (racketlist->mupllist 
                                   (list (int 10) (int 9) (int 15))))))
                 (list (int 10) (int 15))
                 "provided combined test using problems 1, 2, and 4")

; Problem 2
  (check-equal? (eval-exp (add (int 2) (int 2))) (int 4) "add test")
 
  (check-equal? (eval-exp (isgreater (int 3) (int 2))) (int 1) "isgreater greater")

  (check-equal? (eval-exp (isgreater (int -4) (int -4))) (int 0) "isgreater equal")
 
  (check-equal? (eval-exp (isgreater (int 13) (int 18))) (int 0) "isgreater smaller")
 
  (check-equal? (eval-exp (ifnz (int -2) (int -1) (int -2))) (int -1) "ifnz test1")
 
  (check-equal? (eval-exp (ifnz (int 0) (int -1) (int -2))) (int -2) "ifnz test2")
 
  (check-equal? (eval-exp (mlet "foo" (int 1)
                            (add (var "foo") (int 1)))) (int 2) "mlet test1")

  (check-equal? (eval-exp (mlet "foo" (int 1)
                           (mlet "bar" (int 2)
                            (add (var "foo") (var "bar"))))) (int 3) "mlet test2")
 
  (check-equal? (eval-exp (first (apair (int 1) (int 2)))) (int 1) "apair/first test")

  (check-equal? (eval-exp (second (apair (int 1) (int 2)))) (int 2) "apair/second test")

  (check-equal? (eval-exp
                 (call (eval-exp (fun "plus" "delta" (add (int 1) (var "delta")))) (int 2)))
                 (int 3) "call/fun test1")

  (check-exn (lambda (x) (string=? (exn-message x) "MUPL addition applied to non-number"))
             (lambda () (eval-exp (add (int 2) (munit))))
             "add bad argument")

  ; Problem 3
  (check-equal? (eval-exp 
                 (ifmunit (int 1) (int 2) (int 3))) (int 3) "ifmunit test1")

  (check-equal? (eval-exp 
                 (ifmunit (munit) (int 6) (int 3))) (int 6) "ifmunit test2")

  (check-equal? (eval-exp 
                 (ifmunit
                   (second (apair (int 2) (munit)))
                   (int 2)
                   (int 3))) (int 2) "ifmunit test3")

  (check-equal? (eval-exp 
                 (mlet* (list (cons "a" (int 1)) (cons "b" (int 2)))
                         (add (var "a") (var "b")))) (int 3) "mlet* test1")

  (check-equal? (eval-exp 
                 (ifeq (int 1) (int 2) (int 3) (int 4)))
                 (int 4) "ifeq test1")

  ; Problem 4
  (check-equal? (eval-exp
                  (call (call mupl-filter
                          (fun null "item" (isgreater (var "item") (int 5))))
                        (racketlist->mupllist (list (int 19)))))
                (racketlist->mupllist (list (int 19)))
                "mulpl-filter test1")

  (check-equal? (eval-exp
                  (call (call mupl-filter
                          (fun null "item" (isgreater (var "item") (int 5))))
                        (racketlist->mupllist (list (int -1) (int 1) (int 4) (int 8) (int 10) (int 19)))))
                (racketlist->mupllist (list (int 8) (int 10) (int 19)))
                "mulpl-filter test2")

  (check-equal? (mupllist->racketlist
                 (eval-exp (call (call mupl-all-gt (int 9))
                                 (racketlist->mupllist 
                                  (list (int 10) (int 9) (int 15))))))
                (list (int 10) (int 15)) "mupl-all-gt test")

  (check-equal? (mupllist->racketlist
       (eval-exp (call (call mupl-mapAddN (int 7))
                       (racketlist->mupllist 
                       (list (int 3) (int 4) (int 9))))))
    (list (int 10) (int 11) (int 16))
    "combined test1")

  (check-equal? (mupllist->racketlist
       (eval-exp (call (call mupl-mapAddN (int 7))
                       (racketlist->mupllist 
                       (list)))))
   (list)
   "combined test2")))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)
