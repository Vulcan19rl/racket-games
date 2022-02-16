#lang racket
(require 2htdp/image)
(require 2htdp/universe)
(require test-engine/racket-tests)

;minesweeper made by Owen Russell-Lanning


;                                                                          
;                                                                          
;                                                                          
;                                                                          
;     ;;;;                                                                 
;    ;   ;                           ;                       ;             
;   ;        ;;;;   ;; ;;    ;;;;;  ;;;;;    ;;;;   ;; ;;   ;;;;;    ;;;;; 
;   ;       ;    ;   ;;  ;  ;    ;   ;      ;    ;   ;;  ;   ;      ;    ; 
;   ;       ;    ;   ;   ;   ;;;;    ;       ;;;;;   ;   ;   ;       ;;;;  
;   ;       ;    ;   ;   ;       ;   ;      ;    ;   ;   ;   ;           ; 
;    ;   ;  ;    ;   ;   ;  ;    ;   ;   ;  ;   ;;   ;   ;   ;   ;  ;    ; 
;     ;;;    ;;;;   ;;; ;;; ;;;;;     ;;;    ;;; ;; ;;; ;;;   ;;;   ;;;;;  
;                                                                          
;                                                                          
;                                                                          
;                                                                          


;board dimensions - change to make game bigger or smaller
(define CELLS-X 8) ;cells in the x dimension
(define CELLS-Y CELLS-X) ;cells in the y dimension

(define CELL-PIXELS 50) ;width and height for each cell
(define CELL (rectangle CELL-PIXELS CELL-PIXELS "outline" "black"))
(define HIDDEN-CELL-BACKGROUND (rectangle CELL-PIXELS CELL-PIXELS "solid" "gray"))
(define MARKED-CELL-BACKGROUND (rectangle CELL-PIXELS CELL-PIXELS "solid" "red"))
(define HIDDEN-CELL (overlay CELL HIDDEN-CELL-BACKGROUND))
(define MARKED-CELL (overlay CELL MARKED-CELL-BACKGROUND))






;game rules
(define MINES 10) ;total mines on the board




;                                                          
;                                                          
;                                                          
;                                                          
;    ;;; ;                                                 
;   ;   ;;   ;                               ;             
;   ;       ;;;;;   ;; ;;;  ;;  ;;   ;;; ;  ;;;;;    ;;;;; 
;    ;;;;    ;       ;;      ;   ;  ;   ;;   ;      ;    ; 
;        ;   ;       ;       ;   ;  ;        ;       ;;;;  
;        ;   ;       ;       ;   ;  ;        ;           ; 
;   ;;   ;   ;   ;   ;       ;  ;;  ;    ;   ;   ;  ;    ; 
;   ; ;;;     ;;;   ;;;;;     ;; ;;  ;;;;     ;;;   ;;;;;  
;                                                          
;                                                          
;                                                          
;                                                          

;a square represents a single square in game. it contains positional data, if it's a mine or not, and whether or not it's visible, and whether it's been marked
;x and y are integers specifying the cell pos
;mine? is a boolean specifying whether or not the square is a mine
;visible? is a boolean specifying if the square has been uncovered
;marked is a boolean specifying if the square has been marked by the player
;value is an int totalling the amount of mines next to the square. #false if mine or if not calculated or if zero
(define-struct square (x y mine? visible? value))

;true if two squares are the same
(define (square=? square1 square2)
  (and (= (square-x square1) (square-x square2))
       (= (square-y square1) (square-y square2))
       (boolean=? (square-mine? square1) (square-mine? square2))
       (boolean=? (square-visible? square1) (square-visible? square2))
       (or (and (number? (square-value square1)) (number? (square-value square2))
                (= (square-value square1) (square-value square2)))
           (and (boolean? (square-value square1)) (boolean? (square-value square2))
                (boolean=? (square-value square1) (square-value square2))))))




;returns a default mine square when given an x and y position
(define (get-mine-square x y)
  (make-square x y #true #false #false))

(check-expect (square=? (get-mine-square 0 0) (get-mine-square 0 0)) #true)
(check-expect (square=? (get-mine-square 0 0) (get-empty-square 0 0)) #false)

;returns a default empty square when given an x and y positions
(define (get-empty-square x y)
  (make-square x y #false #false #false))

;a posn is an x and y location
(define-struct posn (x y))


;the world is a 2d list with the dimension of the cell x and y. Each element is a square



;                                                          
;                                                          
;                                                          
;                                                          
;   ;;; ;;;           ;;                                   
;    ;   ;             ;                                   
;    ;   ;   ;;;;      ;    ;; ;;    ;;;;   ;; ;;;   ;;;;; 
;    ;;;;;  ;    ;     ;     ;;  ;  ;    ;   ;;     ;    ; 
;    ;   ;  ;;;;;;     ;     ;   ;  ;;;;;;   ;       ;;;;  
;    ;   ;  ;          ;     ;   ;  ;        ;           ; 
;    ;   ;  ;          ;     ;   ;  ;        ;      ;    ; 
;   ;;; ;;;  ;;;;;   ;;;;;   ;;;;    ;;;;;  ;;;;;   ;;;;;  
;                            ;                             
;                           ;;;                            
;                                                          
;


;index: list[x] int -> x
;returns the element as the position or false
(define (index ls i)
  (cond [(< i 0) #false]
        [(empty? ls) #false]
        [(= 0 i) (first ls)]
        [else (index (rest ls) (- i 1))]))


(check-expect (index (list 1 2 3 4 5) 0) 1)
(check-expect (index (list 1 2 3 4 5) 5) #false)

;replace-index: list[x] int x -> list[x]
;replaces the element at the specific index
(define (replace-index ls i x)
  (cond [(< i 0) (error "index: position is less than zero")]
        [(empty? ls) ls]
        [(= 0 i) (cons x (rest ls))]
        [else (cons (first ls) (replace-index (rest ls) (- i 1) x))]))

(check-expect (replace-index (list 1 2 3 4 5) 0 4) (list 4 2 3 4 5))
(check-expect (replace-index (list 1 2 3 4 5) 4 6) (list 1 2 3 4 6))
(check-expect (replace-index (list 1 2 3 4 5) 5 0) (list 1 2 3 4 5))


;get-number-cell int -> image
;creates a number cell from an integer
(define (get-number-cell i)
  (overlay (text (number->string i) 30 "black")
           CELL))

(check-expect (get-number-cell 1) (overlay (text "1" 30 "black") CELL))



;posn=?
;posn posn -> bool
;checks if two posns are equals
(define (posn=? a b)
  (and (= (posn-x a) (posn-x b))
       (= (posn-y a) (posn-y b))))

(check-expect (posn=? (make-posn 1 2) (make-posn 1 2)) #true)
(check-expect (posn=? (make-posn 4 2) (make-posn 6 2)) #false)

;posn-ls=?
;list[posn] list[posn] -> boolean
;true if two posn lists are equal
(define (posn-ls=? a b)
  (and (list? a)
       (list? b)
       (= (length a) (length b))
       (local [(define (helper-solver x y)
                 (or (and (empty? x) (empty? y))
                     (and (posn? (first x)) (posn? (first y))
                          (posn=? (first x) (first y))
                          (helper-solver (rest x) (rest y)))))]
         (helper-solver a b))))

(check-expect (posn-ls=? (list (make-posn 1 2) (make-posn 3 4))
                         (list (make-posn 1 2) (make-posn 3 4)))
              #true)

(check-expect (posn-ls=? (list (make-posn 1 2) (make-posn 3 4))
                         (list (make-posn 1 2) (make-posn 3 5)))
              #false)

(check-expect (posn-ls=? (list (make-posn 1 2) (make-posn 3 4))
                         (list (make-posn 1 2) ))
              #false)




;member-comp?
;(x,x->bool), x, list[x] -> boolean
;true if any of the elements in the list return true when plugged into a the function with x
(define (member-comp? f x ls)
  (ormap (lambda (y) (f x y)) ls))

(check-expect (member-comp? posn=? (make-posn 1 2) (list (make-posn 2 3) (make-posn 1 2) (make-posn 4 5))) #true)
(check-expect (member-comp? posn=? (make-posn 1 2) (list (make-posn 2 3) (make-posn 4 5))) #false)
(check-expect (member-comp? posn=? (make-posn 1 2) empty) #false)


;list-similar?
;list[x], list[x] (x, x -> boolean) -> boolean
;returns true if two lists have equal values ignoring order
(define (list-similar? ls1 ls2 comp?)
  (and (= (length ls1) (length ls2))
       (andmap identity (map (lambda (e) (member-comp? comp? e ls2)) ls1))
       (andmap identity (map (lambda (e) (member-comp? comp? e ls1)) ls2))))

(check-expect (list-similar? (list (make-posn 1 1) (make-posn 1 2)) (list (make-posn 1 2) (make-posn 1 1)) posn=?) #true)
(check-expect (list-similar? (list (make-posn 1 1) (make-posn 1 2)) (list (make-posn 1 2) (make-posn 1 1) (make-posn 2 4)) posn=?) #false)





;get-starting-board: int, int, int -> list[list[square]]
;takes the x cells, y cells and number of mines
;returns the randomized starting board
(define (get-starting-board x y mines)
  (reverse (build-board x y (get-mine-positions x y mines))))


;build-board
;int, int, list[posn] -> list[list[square]]
;builds a board when given the x width, y width and mine positions
(define (build-board x y mine-positions)
  (if (= 0 y) empty
      (cons (reverse (build-row x y mine-positions)) (build-board x (- y 1) mine-positions))))

(check-expect (local [(define board (build-board 2 2 (list (make-posn 0 0) (make-posn 1 1))))]
                (and (list-similar? (first board) (list (get-empty-square 0 1) (get-mine-square 1 1)) square=?)
                     (list-similar? (second board) (list (get-mine-square 0 0) (get-empty-square 1 0)) square=?)))
              #true)


;build-row
;int, int, list[posn] -> list[square]
;builds a row in the board when given the x width, the current y column and a list of mine posns
(define (build-row x y mine-positions)
  (cond [(= 0 x) empty]
        [(member-comp? posn=? (make-posn (- x 1) (- y 1)) mine-positions) ;take one from each row and column to account for random gen
         (cons (get-mine-square (- x 1) (- y 1)) (build-row (- x 1) y mine-positions))]
        [else  (cons (get-empty-square (- x 1) (- y 1)) (build-row (- x 1) y mine-positions))]))

(check-expect (list-similar? (build-row 3 1 (list (make-posn 2 0) (make-posn 0 0))) (list (get-mine-square 2 0) (get-empty-square 1 0) (get-mine-square 0 0)) square=?) #true)
         
    


;get-mine-positions: int, int, int -> list[posn]
;takes the x cells, y cells and number of mines
;generates a list of positions for mines randomly with not repetitions
(define (get-mine-positions x y mines)
  (if (= mines 0) empty
      (local [(define mine-positions (get-mine-positions x y (- mines 1)))]
        (cons (get-new-mine x y mine-positions) mine-positions))))

(check-expect (list-similar? (get-mine-positions 2 2 4) (list (make-posn 1 0) (make-posn 0 0) (make-posn 1 1) (make-posn 0 1)) posn=?) #true)





;get-new-mine: int, int, list[posn] -> posn
;takes the x cells, y cells and a list of prexisting positions
;generates a new mine position that dosen't overlap with the given list
(define (get-new-mine x y ls)
  (local [(define new-pos (make-posn (random x) (random y)))]
    (if (member-comp? posn=? new-pos ls) ;check if new pos already exists in list
        (get-new-mine x y ls)
        new-pos)))

(check-expect (posn=? (get-new-mine 2 2 (list (make-posn 1 0) (make-posn 0 0) (make-posn 1 1))) (make-posn 0 1)) #true)
  
  
;get-square-value: list[list[square]], int, int -> int
;calcualtes the value of a square from the board and the x and y of the square
(define (get-square-value ls x y)
  (+ (if (> y 0)
         ;;calculate top row
         (+ (if (and (> x 0) (square-mine? (get-square ls (- x 1) (- y 1))))
                1
                0)
            (if (and (< x (- CELLS-X 1)) (square-mine? (get-square ls (+ x 1) (- y 1))))
                ;;calculate right
                1
                0)
            ;;calculate top middle
            (if (square-mine? (get-square ls x (- y 1)))
                1
                0)
            )
         0)
     (if (< y (- CELLS-Y 1))
         ;;calculate bottom row
         (+ (if (and (> x 0) (square-mine? (get-square ls (- x 1) (+ y 1))))
                1
                0)
            (if (and (< x (- CELLS-X 1)) (square-mine? (get-square ls (+ x 1) (+ y 1))))
                ;;calculate right
                1
                0)
            ;;calculate bottom middle
            (if (square-mine? (get-square ls x (+ y 1)))
                1
                0)
            )
         0)
     ;;calculate row
     (+ (if (and (> x 0) (square-mine? (get-square ls (- x 1) y)))
            1
            0)
        (if  (and (< x (- CELLS-X 1)) (square-mine? (get-square ls (+ x 1) y)))
             ;;calculate right
             1
             0)
        )))


;get-square: list[list[square]], int, int -> square
;returns the square at the x and y of the board
(define (get-square ls x y)
  (index (index ls y) x))

(check-expect (get-square (list (list 1 2 3 4) (list 5 6 7 8)) 3 1) 8)


;has-won?: list[list[square]] -> boolean
;checks to see if the user has won
(define (has-won? ls)
  (andmap (lambda (row) (andmap (lambda (square) (or (square-visible? square) (square-mine? square))) row))
          ls))



     
;                                                                  
;                                                                  
;                                                                  
;                                                                  
;   ;;; ;;;                     ;;    ;;                           
;    ;   ;                       ;     ;                           
;    ;   ;   ;;;;   ;; ;;    ;;; ;     ;     ;;;;   ;; ;;;   ;;;;; 
;    ;;;;;  ;    ;   ;;  ;  ;   ;;     ;    ;    ;   ;;     ;    ; 
;    ;   ;   ;;;;;   ;   ;  ;    ;     ;    ;;;;;;   ;       ;;;;  
;    ;   ;  ;    ;   ;   ;  ;    ;     ;    ;        ;           ; 
;    ;   ;  ;   ;;   ;   ;  ;   ;;     ;    ;        ;      ;    ; 
;   ;;; ;;;  ;;; ;; ;;; ;;;  ;;; ;;  ;;;;;   ;;;;;  ;;;;;   ;;;;;  
;                                                                  
;                                                                  
;                                                                  
;                                                                  


;draw-game: list[list[square]] -> image
;draws the game when given the board
(define (draw-game ls)
  (cond [(boolean? ls) empty-image]
        [(empty? ls)
         empty-image]
        [else (above (draw-row (first ls))
                     (draw-game (rest ls)))]))

;draw-row: list[square] -> image
;draws a single row of squares
(define (draw-row ls)
  (cond [(empty? ls) empty-image]
        [(not (square-visible? (first ls)))
         (beside HIDDEN-CELL (draw-row (rest ls)))]
        [(number? (square-value (first ls)))
         (beside (get-number-cell (square-value (first ls))) (draw-row (rest ls)))]
        [else (beside CELL (draw-row (rest ls)))]))

(check-expect (draw-row (list (make-square 0 0 #false #true 1) (make-square 1 0 #true #false #false)))
              (beside (get-number-cell 1) HIDDEN-CELL empty-image))


;handle-click: list[list[square]], int, int, mouse event -> list[list[square]]
;handles player interaction with the game board
(define (handle-click ls x y event)
  (cond [(boolean? ls) ls]
        [(string=? event "button-down")
         (cell-click ls (floor (/ x CELL-PIXELS)) (floor (/ y CELL-PIXELS)))]
        [else ls]))

;cell-click: list[list[square]] x y -> list[list[square]]
;handles a player click on the cell at the x and y position
(define (cell-click ls x y)
  (local [(define clicked-row (index ls y))
          (define clicked-square (if (boolean? clicked-row) #false (index clicked-row x)))]
    (cond [(boolean? clicked-square) ls]
          [(square-mine? clicked-square)
           #false] ;;return false world
          [(square-visible? clicked-square) ls]
          [else (local [(define sq-val (if (square-mine? clicked-square) #false (get-square-value ls x y)))
                        (define replaced-board (replace-index ls y (replace-index clicked-row x (make-square (square-x clicked-square)
                                                                                                             (square-y clicked-square)
                                                                                                             (square-mine? clicked-square)
                                                                                                             #true
                                                                                                             sq-val))))
                        (define final-board (if (and (not (boolean? sq-val)) (= sq-val 0))
                                                ;;click surrounding tiles
                                                (click-surrounding replaced-board x y)
                                                replaced-board))]
                  (if (has-won? final-board)
                      "you win"
                      final-board))])))

;click-surrounding: list[list[square]], x, y -> list[list[squre]]
;clicks the surroudning tiles for the given coordinate
(define (click-surrounding ls x y)
  (foldr (lambda (a b) (cell-click b (posn-x a) (posn-y a)))
         ls
         (list (make-posn (- x 1) (- y 1)) (make-posn x (- y 1)) (make-posn (+ x 1) (- y 1))
               (make-posn (- x 1) y) (make-posn (+ x 1) y)
               (make-posn (- x 1) (+ y 1)) (make-posn x (+ y 1)) (make-posn (+ x 1) (+ y 1)))))
    


        
        
  




(test)


(big-bang (get-starting-board CELLS-X CELLS-Y MINES)
  (to-draw draw-game)
  (on-mouse handle-click)
  (stop-when string?))