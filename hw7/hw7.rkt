#lang racket

; CSE341, Programming Languages, Homework 7, hw7.rkt (see also OCaml code)

(provide (all-defined-out)) ;; so we can put tests in a second file

; a little language for 2D geometry objects

; each subclass of geometry-expression%, including subclasses of geometry-value%,
;  needs to respond to messages preprocess-prog and eval-prog

; each subclass of geometry-value% additionally needs:
;   * shift
;   * intersect, which uses the double-dispatch pattern
;   * intersect-point, intersect-line, and intersect-vertical-line for 
;       for being called by intersect of appropriate classes and doing
;       the correct intersection calculuation
;   * (We would need intersect-no-points and intersect-line-segment, but these
;      are provided by geometry-value% and should not be overridden.)
;   *  intersect-with-segment-as-line-result, which is used by 
;      intersect-line-segment as described in the assignment
; you can define other helper methods, but will not find much need to

; Note: geometry objects should be immutable: do not set! private state

; Note: For eval-prog, represent environments as lists of pairs as described in the assignment

(define epsilon 0.00001)

(define (float-close f1 f2)
  (< (abs (- f1 f2)) epsilon))

(define (float-close-point x1 y1 x2 y2)
  (and (float-close x1 x2) (float-close y1 y2)))

; do not change this class definition
; just as a matter of style, we make all expression classes a subclass of
; geometry-expression% or geometry-value%
(define geometry-expression%
  (class object%
    (super-new)))

; do not change methods in this class definition *except*
; you can add methods to it if you wish
(define geometry-value%
  (class geometry-expression%
    (super-new)

    ; two-points-to-line could return a line% or a vertical-line%
    (define (two-points-to-line x1 y1 x2 y2)
      (if (float-close x1 x2)
          (new vertical-line% [x x1])
          (let* ([m (/ (- y1 y2) (- x1 x2))]
                 [b (- y2 (* m x2))])
            (new line% [m m][b b]))))

    ; we put intersect-no-points in this class so all subclasses inherit it:
    ; the intersection of a value with a no-points% is a no-points%
    (define/public (intersect-no-points np) np)

    ; we put intersect-line-segment in this class so all subclasses can inhert it:
    ; the intersection of this with a line-segment% is computed by
    ; first intersecting with the line containing the segment and then
    ; calling the result's intersect-with-segment-as-line-result with the segment
    (define/public (intersect-line-segment seg)
      (send (send this intersect (two-points-to-line (send seg get-x1)
                                                     (send seg get-y1)
                                                     (send seg get-x2)
                                                     (send seg get-y2)))
            intersect-with-segment-as-line-result seg))
    )
 )

; do *not* change the no-points% definition *except* to add methods if needed
; to implement eval-prog and preprocess-prog.
; everything else is done for you.
; although this is the easiest class, it shows what methods every subclass
; of geometry values needs
(define no-points%
  (class geometry-value%
    (super-new)
    
    (define/public (shift dx dy) this) ; shift a no-points% is a no-points%
    (define/public (intersect other)
      (send other intersect-no-points this)) ; will be a no-points% but follow double-dispatch pattern
    (define/public (intersect-point p) this) ; intersection of a point% and a no-points% is a no-points%
    (define/public (intersect-line line) this) ; intersection of a line% and a no-points% is a no-points%
    (define/public (intersect-vertical-line vline) this) ; intersection of a vertical-line% and a no-points% is a no-points%

    ; if this is the intersection of some shape s and the line containing seg, then we
    ; return the intersection of the shape s and the seg, where seg is an instance of
    ; line-segment%. as a no-points%, that means no intersection between s and the line
    ; containing seg, so must be no intersection between s and seg
    (define/public (intersect-with-segment-as-line-result seg) this)
    (define/public (preprocess-prog) this)
    (define/public (eval-prog env) this)
 ))

; *add* methods to point% class -- do *not* change given code and do not override any methods
(define point%
  (class geometry-value%
    (super-new)
    (init x y)
    (define _x x)
    (define _y y)
    (define/public (get-x) _x)
    (define/public (get-y) _y)

    (define/public (shift dx dy) (new point% (x (+ _x dx)) (y (+ _y dy))))
    (define/public (intersect other) (send other intersect-point this))
    (define/public (intersect-point p) (if (float-close-point _x _y (send p get-x) (send p get-y)) (this)(new no-points%)))
    (define/public (intersect-line line) (if (float-close _y (+ (* _x (send line get-m)) (send line get-b))) (new point% [x _x][y _y])(new no-points%)))
    (define/public (intersect-vertical-line vline) (if (float-close _x (send vline get-x)) (new point% [x _x][y _y])(new no-points%)))
    (define/public (intersect-with-segment-as-line-result seg)
      (if (and (isbetween _x (send seg get-x1) (send seg get-x2)) (isbetween _y (send seg get-y1) (send seg get-y2)))
          (new point% [x _x][y _y]) (new no-points%))
    )
    (define/public (isbetween val a1 a2) 
      (or (and (< val (+ a1 epsilon)) (>= val (- a2 epsilon))) (and (>= val (- a1 epsilon)) (< val (+ a2 epsilon))))
    )
    (define/public (preprocess-prog) this)
    (define/public (eval-prog env) this)
  )
)

; *add* methods to line% class -- do *not* change given code and do not override any methods
(define line%
  (class geometry-value%
    (super-new)
    (init m b)
    (define _m m)
    (define _b b)
    (define/public (get-m) _m)
    (define/public (get-b) _b)
    
    (define/public (shift dx dy) (new line% (b (- (+ _b dy) (* _m dx))) (m _m)))
    (define/public (intersect other) (send other intersect-line this))
    (define/public (intersect-point other) (send other intersect-line this))
    (define/public (intersect-line other)
       (let ([difmneg (- (send other get-m) _m) ]
            [difb (- _b (send other get-b))])
         (if (float-close (send other get-m) _m)
             (if (float-close (send other get-b) _b)
                 (new line% (m _m)(b _b))
                 (new no-points%))
        (new point% (x (/ difb difmneg)) (y (+ (* _m (/ difb difmneg)) _b))))
         )
    )
    (define/public (intersect-vertical-line other) (new point% (x (send other get-x)) (y (+ (* _m (send other get-x)) _b))))
    (define/public (intersect-with-segment-as-line-result seg)
      seg
    )
    (define/public (preprocess-prog) this)
    (define/public (eval-prog env) this)
))

; *add* methods to vertical-line% class -- do *not* change given code and do not override any methods
(define vertical-line%
  (class geometry-value%
    (super-new)
    (init x)
    (define _x x)
    (define/public (get-x) _x)
    
    (define/public (shift dx dy) (new vertical-line%(x (+ _x dx))))
    (define/public (intersect other) (send other intersect-vertical-line this))
    (define/public (intersect-point other) (send other intersect-vertical-line this))
    (define/public (intersect-line other)
      (send other intersect-vertical-line this))
    (define/public (intersect-vertical-line other)
      (if (float-close (send other get-x) _x)
          (new vertical-line% (x _x))
          (new no-points%))
     )
    (define/public (intersect-with-segment-as-line-result seg)
      seg
    )
    (define/public (preprocess-prog) this)
    (define/public (eval-prog env) this)
 ))

; *add* methods to line-segment% class -- do *not* change given code and do not override any methods
; Note: This is the most difficult class, particularly:
;         * preprocess-prog, although the sample solution is only several lines
;         * intersect-with-segment-as-line-result, about 35 lines in the sample solution
; Remember:
;   * (x1,y1) is to the /right/ of (x2,y2) or /above/ if x1 and x2 are float-close to each other
;   * The ML code has all the algorithms you need, you just need to port to Racket
(define line-segment%
  (class geometry-value%
    (super-new)
    (init x1 y1 x2 y2)
    (define _x1 x1)
    (define _y1 y1)
    (define _x2 x2)
    (define _y2 y2)
    (define/public (get-x1) _x1)
    (define/public (get-y1) _y1)
    (define/public (get-x2) _x2)
    (define/public (get-y2) _y2)
    
    (define (get-slope)
      (/ (- _y2 _y1) (- _x2 _x1))
    )

    (define/public (preprocess-prog)
      (let* (
         [minx (min _x1 _x2)]
         [miny (min _y1 _y2)]
         [maxx (max _x1 _x2)]
         [maxy (max _y1 _y2)]
          )
        (
         if(float-close-point _x1 _y1 _x2 _y2) (new point% (x _x1) (y _y1))
           (
            if(or (and (float-close _x1 _x2) (< _y2 _y1)) (< _x2 _x1)) (new line-segment% (x1 _x1) (y1 maxy) (x2 _x2) (y2 miny))
              (new line-segment% (x1 maxx) (y1 maxy) (x2 minx) (y2 miny))
            )
        )
       )
    )
    (define/public (eval-prog env) this)
    
    (define/public (shift dx dy)
      (new line-segment%
      (x1 (+ _x1 dx))
      (x2 (+ _x2 dx))
      (y1 (+ _y1 dy))
      (y2 (+ _y2 dy)))
    )
      
    (define/public (intersect other) (send other intersect-line-segment this))
    (define/public (intersect-point other) (send other intersect-line-segment this))
    (define/public (intersect-line other)
      (send other intersect-line-segment this))
    (define/public (intersect-vertical-line other)
      (send other intersect-line-segment this))
    (define/public (intersect-with-segment-as-line-result seg)
      (define ys (if(< (send this get-y2) (send seg get-y2)) (cons this seg)(cons seg this)))
      (define xs (if(< (send this get-x2) (send seg get-x2)) (cons this seg)(cons seg this)))

      (let (
       [firsty (car ys)]
       [secondy (cdr ys)]
       [firstx (car xs)]
       [secondx (cdr xs)])

       (if(float-close _x1 _x2)
          (if(float-close (send firsty get-y1) (send secondy get-y2))
             (new point% (x (send firsty get-x1)) (y (send firsty get-y1)))
             (if (< (send firsty get-y1) (send secondy get-y2))
                 (new no-points%)
                 (if (> (send firsty get-y1) (send secondy get-y1))
                     (new line-segment% (x1 (send secondy get-x1)) (y1 (send secondy get-y1)) (x2 (send secondy get-x2)) (y2 (send secondy get-y2)))
                     (new line-segment% (x1 (send firsty get-x1)) (y1 (send firsty get-y1)) (x2 (send secondy get-x2)) (y2 (send secondy get-y2)))
                 )
             )
          )
          (if(float-close(send firstx get-x1) (send secondx get-x2))
             (new point% (x (send firstx get-x1)) (y (send firstx get-y1)))
             (if (< (send firstx get-x1) (send secondx get-x2))
                 (new no-points%)
                 (if (> (send firstx get-x1) (send secondx get-x1))
                     (new line-segment% (x1 (send secondx get-x1)) (y1 (send secondx get-y1)) (x2 (send secondx get-x2)) (y2 (send secondx get-y2)))
                     (new line-segment% (x1 (send firstx get-x1)) (y1 (send firstx get-y1)) (x2 (send secondx get-x2)) (y2 (send secondx get-y2)))
                 )
             )
          )
       )
    ))
      
    (define (get_firstx a b)
      (if(< (send a get-y2) (send b get-y1)) (car a b)(car b a))
    )
    (define (get_secondx a b)
      (if(> (send a get-y1) (send b get-y1)) (car b a)(car a b))
    )
    (define (get_firsty a b)
      (if(< (send a get-x2) (send b get-x1)) (car a b)(car b a))
    )
    (define (get_secondy a b)
      (if(> (send a get-x1) (send b get-x1)) (car b a)(car a b))
    )
  ))

; Note: there is no need for getter methods for the non-value classes

; For each of the non-value classes, do not change the provided code but do add the
; small number of methods needed

(define intersect%
  (class geometry-expression%
    (super-new)
    (init e1 e2)
    (define _e1 e1)
    (define _e2 e2)

    (define/public (get-e1) _e1)
    (define/public (get-e2) _e2)

    (define/public (preprocess-prog)
      (new intersect% (e1 (send _e1 preprocess-prog)) (e2 (send _e2 preprocess-prog)))
     )
    (define/public (eval-prog env)
      (send (send _e1 eval-prog env) intersect (send _e2 eval-prog env))
    )
))

(define let%
  (class geometry-expression%
    (super-new)
    (init s e1 e2)
    (define _s s)
    (define _e1 e1)
    (define _e2 e2)

    (define/public (preprocess-prog)
      (new let% (s _s) (e1 (send _e1 preprocess-prog)) (e2 (send _e2 preprocess-prog)))
    )

    (define/public (eval-prog env)
      (send _e2 eval-prog (cons (cons _s (send _e1 eval-prog env)) env))
    )
))

(define var%
  (class geometry-expression%
    (super-new)
    (init s)
    (define _s s)
    (define/public (preprocess-prog) this)
    (define/public (eval-prog env)
      (let ([pr (assoc _s env)])
        (if pr (cdr pr) (error "var not found" _s))))
))

(define shift%
  (class geometry-expression%
    (super-new)
    (init dx dy e)
    (define _dx dx)
    (define _dy dy)
    (define _e e)

    (define/public (preprocess-prog)
      (new shift% (dx _dx) (dy _dy) (e (send _e preprocess-prog)))
    )
    (define/public (eval-prog env)
      (send (send _e eval-prog env) shift _dx _dy)
    )
))