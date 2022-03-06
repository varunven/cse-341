#lang racket

;; This is the only file you turn in, so do not modify the other files as
;; part of your solution.

(require "hw6provided.rkt")
(provide my-tetris%)
(require racket/random)
; Uncomment this line if you do the challenge problem
;(provide my-tetris-challenge%)

;; Edit the two classes below to implement your enhancements.

(define my-tetris%
  (class tetris% 

   (super-new)
   (inherit set-board!)

   (define board (send this get-board))

    (define/override (reset-board)
      (set-board! (new my-board% [game this])))

    (define/augment (on-char event)
      (define keycode (send event get-key-code))
      (displayln keycode)
      (match keycode
        [#\u (begin
               (send board rotate-clockwise)
               (send board rotate-clockwise))]
        [#\c (send board set-is-cheating! #t)]
        [_ (inner #f on-char event)]))
    
    ;things to change in tetris: on-char event, set-board, reset-board
    ;things to use in tetris: board with new reset/set

))

(define my-board%
  (class board%
    (define is-cheating #f)
    (super-new)
    (inherit set-score!)
    (inherit set-delay!)
    (inherit remove-filled)

    (define/public (set-is-cheating! flag) (set! is-cheating flag))

    (define/public (cheat) 
      (let ([flag (and is-cheating (>= (send this get-score) 100))])
        (set-is-cheating! #f)
        (if flag (set-score! (- (send this get-score) 100)) #f)
        flag)
     )

    (define/override (select-shape)
      (if (cheat)
        (vector (vector '(0 . 0)))
        (random-ref 
          (vector-append all-shapes
            (vector
              (vector (vector '(0 . 0) '(-1 . 0) '(1 . 0) '(2 . 0) '(-2 . 0))
                      (vector '(0 . 0) '(0 . -1) '(0 . 1) '(0 . 2) '(0 . -2)))     ; OOOOO

              (rotations (vector '(0 . 0) '(-1 . 0) '(1 . 0) '(0 . 1) '(-1 . 1)))  ; OO
                                                                                   ; OOO
                                                                                   
              (rotations (vector '(0 . 0) '(1 . 0) '(0 . 1)))                      ; O
                                                                                   ; OO
          )))))

     (define/override (store-current)
      (define points (send (send this get-current-piece) get-points))
      (for ([idx (in-range (vector-length points))])  ; teehee, I hope all the pieces have 4 blocks.
        (match-define (cons x y) (vector-ref points idx))
        (when (>= y 0)
          (vector-set! (vector-ref (send this get-grid) y) x (send (send this get-current-piece) get-color))))
      (remove-filled)
      (set-delay! (max (- (send this get-delay) 2) 80)))

    ;things to change in board: select-shape, store-current
    ;things to use in board: getters and setters for current-piece, delay, score, grid

))