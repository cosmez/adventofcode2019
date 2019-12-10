#lang racket/base
(require racket/match)
(require racket/list)
(require racket/set)
(require racket/syntax)
(require (for-syntax racket/base))


(module+ test
  (require rackunit))


(define-syntax (wires stx)
  (syntax-case stx ()
    [(_ wire ...)
     #`(list
      #,@(for/list ([x (syntax->list #'(wire ...))])
           (with-syntax ([direction (string->symbol (substring (symbol->string (syntax->datum x)) 0 1))]
                         [amount (string->number (substring (symbol->string (syntax->datum x)) 1))])
             #'(cons (quote direction)  amount))))]))

;;i changed the encoding of every step to be a pair
;;instead of using R75 its encoded  as (R . 75)
;;this way we can get the direction and steps with car and cdr

(struct coord (x y steps) #:transparent)

(define get-direction car) ; retrieve direction from move
(define get-amount cdr) ;retrieve amount from move


;;get manhattan distance between point (0,0) to point
(define (manhattan-distance point)
  (+ (abs (- 0 (coord-x point)))
     (abs (- 0 (coord-y point)))))

(define (step-xy position direction)
  (case direction
    ['U (coord (coord-x position) (+ 1 (coord-y position)) (+ 1 (coord-steps position)))]
    ['D (coord (coord-x position) (- (coord-y position) 1) (+ 1 (coord-steps position)))]
    ['R (coord (+ 1 (coord-x position)) (coord-y position) (+ 1 (coord-steps position)))]
    ['L (coord (- (coord-x position) 1) (coord-y position) (+ 1 (coord-steps position))) ]))

(module+ test
  (check-equal? (manhattan-distance (coord 3 3 0 )) 6)
  (check-equal? (step-xy (coord 0 0 0) 'U 0) (coord 0 1 0))
  (check-equal? (step-xy (coord 0 0 0) 'D 1) (coord 0 -1 1))
  (check-equal? (step-xy (coord 0 0 0) 'R 1) (coord 1 0 1))
  (check-equal? (step-xy (coord 0 0 0) 'L 2) (coord -1 0 2)))

;;this builds the list of steps taken by a move
;;just for a single move
(define (build-steps-list start-point direction amount)
  (for/fold ([steps '()] [point start-point])
            ([i amount])
    (define next-step (step-xy point direction))
    (values (cons next-step steps) next-step)))


(module+ test
  (define-values (steps last-point)
    (build-steps-list (coord 0 0 0) 'U 5))
  (check-equal? last-point (coord 0 5 5))
  (check-equal? steps (list (coord 0 5 5) (coord 0 4 4) (coord 0 3 3) (coord 0 2 2) (coord 0 1 1))))


(define (run-wire moves)
  (define start-point (coord 0 0 0)) ;starting point of the circuit
  (define-values (panel-wires last-point)
    (for/fold ([all-steps '()] [last-point start-point]) ([move moves])
      (define-values (move-steps new-last-point)
        (build-steps-list last-point (get-direction move) (get-amount move) 0 0))
      (values (append all-steps move-steps) new-last-point)))
  panel-wires)



;;not functional cache, make it run faster
(define cache-last-start-point-1 '())
(define cache-last-move-1 '())
(define cache-last-point-1 '())
(define cache-moves-1 '())

(define (find-intersection moves1 moves2)
  (displayln "=====MOVES 1====")
  (displayln moves1)
  (displayln "=====MOVES 2====")
  (displayln moves2))

(define (find-single-move-intersection start-point-1 move-1 start-point-2 move-2)
  (when (and (not (equal? cache-last-start-point-1 start-point-1))
             (not (equal? cache-last-move-1 move-1)))
    (define-values (moves-1 last-point-1)
      (build-steps-list start-point-1 (get-direction move-1) (get-amount move-1)))
    (set! cache-last-point-1 last-point-1)
    (set! cache-moves-1 moves-1))
  (define-values (moves-2 last-point-2)
    (build-steps-list start-point-2 (get-direction move-2) (get-amount move-2)))
  (find-intersection cache-moves-1 moves-2)
  (define intersects (set-intersect cache-moves-1 moves-2))
  (define distance
    (if (null? intersects) #f
        (car (sort (map manhattan-distance intersects) <))))
  (values distance cache-last-point-1 last-point-2))

(find-single-move-intersection (coord 0 5 0) '(R . 22) (coord 10 1 5) '(U . 20))


(define (find-move-wire-intersection start-point-1 move-1 moves)
  (for/fold ([start-point-2 (coord 0 0)] [end-point-1 (coord 0 0)] [distances '()]
                                      #:result (values start-point-2 end-point-1 (filter (λ (val) val) distances)))            
            ([move moves])
    (define-values (distance last-point-1 last-point-2)
      (find-single-move-intersection start-point-1 move-1 start-point-2 move))
    (values last-point-2 last-point-1 (cons distance distances))))



(define (find-closest-intersection wire-1 wire-2)
  (for/fold ([start-point-1 '(0 . 0)] [all-distances '()] #:result (car (sort all-distances <)))
            ([move-1 wire-1])
    (define-values (_ end-point-1 distances)
      (find-move-wire-intersection start-point-1 move-1 wire-2))
    (values end-point-1 (append all-distances distances))))

(module+ test   
  (check-equal?
   (find-closest-intersection (wires U7 R6 D4 L4) (wires R8 U5 L5 D3)) 6)
  (check-equal?
   (find-closest-intersection
    (wires R75 D30 R83 U83 L12 D49 R71 U7 L72)
    (wires U62 R66 U55 D71 R55 D58 R83))
   159)
  (check-equal?
   (find-closest-intersection
    (wires R98 U47 R26 D63 R33 U87 L62 D20 R33 U53 R51)
    (wires U98 R91 D20 R16 D67 R40 U7 R15 U6 R7))
   135))



