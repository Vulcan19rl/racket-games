#lang racket
(require 2htdp/universe)
(require 2htdp/image)
(require test-engine/racket-tests)

(define OUTLINE-COLOR 'black)
(define BOARD-DIMENSIONS 600)
(define ACTION-COLOR 'red)
(define DEFAULT-SQUARE (square (/ BOARD-DIMENSIONS 3) 'outline OUTLINE-COLOR))

;a tuple is a structure containing a first and second item
(struct tuple (first second))

;the world is a tuple with whose move and list of numbers with an action which one of the following:
;#false - no play
;x
;o

(define START-WORLD (tuple "x" (list #false #false #false #false #false #false #false #false #false)))
(define TEST-WORLD-1 (tuple "x" (list #false "o" "o" "x" #false "x" "x" "o" "x")))
(define TEST-WORLD-2 (tuple "x" (list "o" "x" "x" "o" "o" "x" "x" "o" "x")))

;list=?: list[x], list[y] -> bool
;returns true if the the two lists are equal
(define (list=? ls1 ls2)
  (cond [(and (empty? ls1) (empty? ls2)) #true]
        [(or (empty? ls1) (empty? ls2)) #false]
        [(equal? (first ls1) (first ls2)) (list=? (rest ls1) (rest ls2))]
        [else #false]))

(check-expect (list=? (list 1 2 3 4) (list 1 2 3 4) ) #true)
(check-expect (list=? (list 1 2 3 4) (list 2 2 3 4) ) #false)

;game-world=? tuple[action, ls] tuple[action, ls] -> bool
;returns true if the two game world tuples are equal
(define (game-world=? tuple1 tuple2)
  (and (or (and (boolean? (tuple-first tuple1))
                (boolean? (tuple-first tuple2)))
           (and (tuple-first tuple1) (tuple-first tuple2)))
       (string=? (tuple-first tuple1) (tuple-first tuple2))
       (list=? (tuple-second tuple1) (tuple-second tuple2))))

(check-expect (game-world=? START-WORLD (tuple "x" (list #false #false #false #false #false #false #false #false #false))) #true)



;index: list[x] int -> x
;returns the element at the specified index or #false if it's not there
(define (index ls n)
  (cond [(empty? ls) #false]
        [(= n 0) (first ls)]
        [else (index (rest ls) (- n 1))]))

(check-expect (index empty 0) #false)
(check-expect (index (list 1 2 3 4) 1) 2)
(check-expect (index (list 1 2 3 4) 3) 4)
(check-expect (index (list 1 2 3 4) 0) 1)

;get-range: list[x] int int -> list[x]
;returns a sublist of the given list from the starting integer for the given length
(define (get-range ls start length)
  (cond [(and (= start 0) (= length 0)) empty]
        [(empty? ls) (error "get-range: given too short of a list")]
        [(= start 0) (cons (first ls) (get-range (rest ls) start (- length 1)))]
        [else (get-range (rest ls) (- start 1) length)]))

(check-expect (get-range (list 1 2 3 4 5 6) 0 3)
              (list 1 2 3))
(check-expect (get-range (list 1 2 3 4 5 6) 2 3)
              (list 3 4 5))
(check-expect (get-range (list 1 2 3 4 5 6) 0 6)
              (list 1 2 3 4 5 6))
(check-error (get-range (list 1 2 3 4 5 6) 0 10)
             "get-range: given too short of a list")

;replace-at-index: list[x] int x -> list[x]
;replaces the element at the given index with the other element or returns the given list if it's too short
(define (replace-at-index ls n x)
  (cond [(empty? ls) empty]
        [(= n 0) (cons x (rest ls))]
        [else (cons (first ls)
                    (replace-at-index (rest ls) (- n 1) x))]))

(check-expect (replace-at-index empty 0 0) empty)
(check-expect (replace-at-index (list 1 2 3 4) 1  3) (list 1 3 3 4))
(check-expect (replace-at-index (list 1 2 3 4) 3 5)  (list 1 2  3 5)) 
(check-expect (replace-at-index (list 1 2 3 4) 0 10) (list 10 2 3 4))



;draw-square: symbol, action -> image
;draws the correct square for the given symbol
(define (draw-square outline-color action)
  (if (and (boolean? action) (not action))
      DEFAULT-SQUARE
      (overlay (text action (/ BOARD-DIMENSIONS 4) ACTION-COLOR)
               DEFAULT-SQUARE)))
                 
  

(check-expect (draw-square OUTLINE-COLOR #false) DEFAULT-SQUARE)
(check-expect (draw-square OUTLINE-COLOR "x")   (overlay (text "x" (/ BOARD-DIMENSIONS 4) ACTION-COLOR)
                                                         DEFAULT-SQUARE))
(check-expect (draw-square OUTLINE-COLOR "o")   (overlay (text "o" (/ BOARD-DIMENSIONS 4) ACTION-COLOR)
                                                         DEFAULT-SQUARE))
             

;draw-game: tuple(action, list[action]) -> image
;draws the game from a list of numbers
(define (draw-game t)
  (local [(define ls (tuple-second t))]
    (above
     (beside (draw-square OUTLINE-COLOR (first ls))
             (draw-square OUTLINE-COLOR (second ls))
             (draw-square OUTLINE-COLOR (third ls)))
     (beside (draw-square OUTLINE-COLOR (fourth ls))
             (draw-square OUTLINE-COLOR (fifth ls))
             (draw-square OUTLINE-COLOR (sixth ls)))
     (beside (draw-square OUTLINE-COLOR (seventh ls))
             (draw-square OUTLINE-COLOR (eighth ls))
             (draw-square OUTLINE-COLOR (ninth ls))))))

(check-expect (draw-game START-WORLD)
              (above
               (beside DEFAULT-SQUARE DEFAULT-SQUARE DEFAULT-SQUARE)
               (beside DEFAULT-SQUARE DEFAULT-SQUARE DEFAULT-SQUARE)
               (beside DEFAULT-SQUARE DEFAULT-SQUARE DEFAULT-SQUARE)))

;equal-ls?: list[x] ->bool
;returns true if all elements in the list are equal or empty
(define (equal-ls? ls)
  (or (empty? ls)
  (equal-ls-base? ls (first ls))))

(define (equal-ls-base? ls last-element)
  (cond [(empty? ls) #true]
        [(equal? (first ls) last-element)
         (and (equal-ls-base? (rest ls) (first ls)))]
        [else #false]))

(check-expect (equal-ls? (list 1 1 1 1)) #true)
(check-expect (equal-ls? (list 1 1 1 2 1)) #false)
(check-expect (equal-ls? empty) #true)

;not-booleans?: list[x] -> bool
;returns true if there is no boolean in the list
(define (not-booleans? ls)
  (not (ormap (lambda (x) (boolean? x)) ls)))

(check-expect (not-booleans? (list 1 2 3 4 5)) #true)
(check-expect (not-booleans? (list 1 2 3 4 #false)) #false)

;is-win?: list[action]->bool
;returns true if there is a three in a row win
(define (is-win? ls)
  (local [(define top-row  (get-range ls 0 3))
          (define middle-row  (get-range ls 3 3))
          (define bottom-row  (get-range ls 6 3))
          (define first-column  (list (index ls 0) (index ls 3) (index ls 6)))
          (define second-column  (list (index ls 1) (index ls 4) (index ls 7)))
          (define third-column  (list (index ls 2) (index ls 5) (index ls 8)))
          (define left-to-right-diagonal  (list (index ls 0) (index ls 4) (index ls 8)))
          (define right-to-left-diagonal  (list (index ls 2) (index ls 4) (index ls 6)))]
    (or (and (equal-ls? top-row) (not-booleans? top-row))
        (and (equal-ls? middle-row) (not-booleans? middle-row))
        (and (equal-ls? bottom-row) (not-booleans? bottom-row))
        (and (equal-ls? first-column) (not-booleans? first-column))
        (and (equal-ls? second-column) (not-booleans? second-column))
        (and (equal-ls? third-column) (not-booleans? third-column))
        (and (equal-ls? left-to-right-diagonal) (not-booleans? left-to-right-diagonal))
        (and (equal-ls? right-to-left-diagonal) (not-booleans? left-to-right-diagonal)))))

(check-expect (is-win? (list "o" "o" "o" "x" #false "x" "x" "o" "x")) #true)
(check-expect (is-win? (list "o" #false "o" "o" "o" "x" "x" "x" "x")) #true)
(check-expect (is-win? (list #false #false #false #false #false #false #false #false #false)) #false)

;game-over?: tuple(action, list[action]) -> bool
;returns true if the game is in a finished state
(define (game-over? t)
  (or (andmap (lambda (x) (not (boolean? x))) (tuple-second t))
      (is-win? (tuple-second t))))

(check-expect (game-over? (tuple "o" (list "o" "o" "o" "x" #false "x" "x" "o" "x"))) #true)
(check-expect (game-over? (tuple "o" (list #false #false #false #false #false #false #false #false #false))) #false)

;handle-mouse-down: tuple(action, list[action]) x y mouse-event -> tuple(action, list[action])
;handles the mouse down operation of the mouse
(define (handle-mouse-down t x y event)
  (local [(define ls (tuple-second t))
          (define next-move (if (string=? "x" (tuple-first t)) "o" "x"))
          (define editing-index (+ (* (floor (/ y (/ BOARD-DIMENSIONS 3))) 3)
                                   (floor (/ x (/ BOARD-DIMENSIONS 3)))))]
    (if (not (boolean? (index ls editing-index))) ;if action at position is not a boolean then somethings already been played there
        (tuple (tuple-first t) ls)
        (tuple next-move
               (replace-at-index ls editing-index (tuple-first t))))))


(check-expect (game-world=? (handle-mouse-down START-WORLD 10 10 "button-down")
                            (tuple "o" (replace-at-index (tuple-second START-WORLD) 0 "x")))
              #true)
  
;handle-mouse: tuple(action, list[action]) x y mouse-event -> tuple(action, list[action])
;handles the moving of the mouse on the board and returns an updated action list
(define (handle-mouse t x y event)
  (local [(define new-state (if (string=? event "button-down")
                                (handle-mouse-down t x y event)
                                t))]
    (if (game-over? new-state)
        START-WORLD
        new-state)))

(check-expect (game-world=? (handle-mouse START-WORLD 10 10 "button-down")
                            (handle-mouse-down START-WORLD 10 10 "button-down"))
              #true)






      
         


(test)

(big-bang START-WORLD
  [to-draw draw-game]
  [on-mouse handle-mouse])

