#lang racket

(require "hw6provided.rkt")
(require "hw6assignment.rkt")

(define mine (new my-tetris%))

(define mineboard (send mine get-board))

(println (send mineboard get-score))
(println (send mineboard get-is-cheating))
(send mineboard set-score! 1000)
(println (send mineboard get-score))
(println (send mineboard get-is-cheating))
;now cheat and continue
(println (send mineboard get-score))
(println (send mineboard get-is-cheating))