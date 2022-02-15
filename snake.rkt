#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require test-engine/racket-tests)


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


;enable if debugging
(define DEBUG #true)

(define WIDTH 600)
(define HEIGHT WIDTH)

(define CELLS-X 10)
(define CELLS-Y 10)

(define CELL-WIDTH (/ WIDTH CELLS-X))
(define CELL-HEIGHT (/ HEIGHT CELLS-Y))

(define CELL (rectangle CELL-WIDTH CELL-HEIGHT "outline" "black"))
(define APPLE (rectangle CELL-WIDTH CELL-HEIGHT "solid" "red"))
(define SNAKE-SEGMENT (rectangle CELL-WIDTH CELL-HEIGHT "solid" "green"))

(define TICK-RATE 0.2)




;;int, int, int, int -> image
;;makes the background when given a cell-x and cell-y
(define (create-background cells-x cells-y)
  (duplicate-y (duplicate-x CELL cells-x) cells-y))

(check-expect (create-background 2 2)
              (above (beside CELL CELL) (beside CELL CELL)))




;                                          
;                                          
;                                          
;                                          
;   ;;; ;;;   ;;;   ;;;;;   ;;;     ;;;;   
;    ;   ;   ;   ;   ;   ;   ;       ;  ;  
;    ;   ;  ;     ;  ;   ;   ;       ;   ; 
;    ; ; ;  ;     ;  ;   ;   ;       ;   ; 
;    ; ; ;  ;     ;  ;;;;    ;       ;   ; 
;    ; ; ;  ;     ;  ;  ;    ;   ;   ;   ; 
;    ; ; ;   ;   ;   ;   ;   ;   ;   ;  ;  
;     ; ;     ;;;   ;;;   ; ;;;;;;  ;;;;   
;                                          
;                                          
;                                          
;



(define-struct posn (x y))

;the world is a round structure
;the snake is a list of a posns describing the position of the snake. The first element is the head
;the snake dir is a direction ('up 'down 'left 'right) in the form of a symbol
;apple is a posn of false describing the current position of the apple
(define-struct round (snake dir apple))

(define SNAKE-STARTING-CELL (make-posn 1 1))

(define STARTING-WORLD (make-round (list SNAKE-STARTING-CELL) 'right #false))
(define TEST-WORLD-1 (make-round (list SNAKE-STARTING-CELL) 'right (make-posn 5 5)))

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


;round round -> boolean
;checks if two rounds are equal
(define (round=? x y)
  (and (posn-ls=? (round-snake x) (round-snake y))
       (symbol=? (round-dir x) (round-dir y))
       (or (and (boolean? (round-apple x)) (boolean? (round-apple y)) (boolean=? (round-apple x) (round-apple y)))
           (and (posn? (round-apple x)) (posn? (round-apple y)) (posn=? (round-apple x) (round-apple y))))))

(check-expect (round=? STARTING-WORLD STARTING-WORLD) #true)
(check-expect (round=? STARTING-WORLD (make-round '() 'up #false)) #false)

;;image, int -> image
;;duplicates and stacks an image y amount of times
(define (duplicate-y image y)
  (if (= y 0) empty-image
      (above image (duplicate-y image (- y 1)))))

(check-expect (duplicate-y (rectangle 10 10 "solid" "red") 3)
              (above (rectangle 10 10 "solid" "red") 
                     (rectangle 10 10 "solid" "red")
                     (rectangle 10 10 "solid" "red")))



;;image, int, -> image
;;duplicates and places images beside each other x amount of times
(define (duplicate-x image x)
  (if (= x 0) empty-image
      (beside image (duplicate-x image (- x 1)))))


(check-expect (duplicate-x (rectangle 10 10 "solid" "red") 3)
              (beside (rectangle 10 10 "solid" "red") 
                      (rectangle 10 10 "solid" "red")
                      (rectangle 10 10 "solid" "red")))


(define BACKGROUND (create-background CELLS-X CELLS-Y))

;posn posn -> bool
;checks if two posns are equals
(define (posn=? a b)
  (and (= (posn-x a) (posn-x b))
       (= (posn-y a) (posn-y b))))

(check-expect (posn=? (make-posn 1 2) (make-posn 1 2)) #true)
(check-expect (posn=? (make-posn 4 2) (make-posn 6 2)) #false)

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

;posn -> posn
;converts a cell posn to the top left position in pixels
(define (cell->pixels pos)
  (make-posn (* (posn-x pos) CELL-WIDTH)
             (* (posn-y pos) CELL-HEIGHT)))

(check-expect (posn=? (cell->pixels (make-posn 10 10))
                      (make-posn (* 10 CELL-WIDTH)
                                 (* 10 CELL-HEIGHT)))
              #true)

;list[x] -> list[x]
;removes the last x from the list
(define (remove-last ls)
  (cond [(empty? ls) ls]
        [(empty? (rest ls)) '()]
        [else (cons (first ls) (remove-last (rest ls)))]))

(check-expect (remove-last'(1 2 3 4 5)) '(1 2 3 4 ))
(check-expect (remove-last '(1)) '())
(check-expect (remove-last '()) '())

;x list[x] -> boolean
;returns true if the given element is inside the list
(define (is-member? x ls)
  (cond [(empty? ls) #false]
        [(equal? x (first ls)) #true]
        [else (is-member? x (rest ls))]))

(check-expect (is-member? 3 '(1 2 3 4 5)) #true)
(check-expect (is-member? 5 '(1 2 3 4 5)) #true)
(check-expect (is-member? 6 '(1 2 3 4 5)) #false)
      


;round -> posn
;generates a new apple location
(define (get-new-apple round)
  (local [(define new-x (random CELLS-X))
          (define new-y (random CELLS-Y))]
    (if (is-member? (make-posn new-x new-y) (round-snake round))
        (get-new-apple round)
        (make-posn new-x new-y))))


(check-expect (posn? (get-new-apple STARTING-WORLD)) #true)


;list[posn] -> boolean
;returns true if the snake is valid, dosen't coincide with itself and is within the bounds
(define (valid-snake? snake)
  (local [(define (count-posn z ls)
            (foldr (lambda (x y) (if (posn=? x z) (+ 1 y) y)) 0 ls))]
    (andmap (lambda (x) (and (= (count-posn x snake) 1)
                             (<= 0 (posn-x x) (- CELLS-X 1))
                             (<= 0 (posn-y x) (- CELLS-Y 1)))) snake)))

(check-expect (valid-snake? (list (make-posn 1 2) (make-posn 1 3) (make-posn 1 2))) #false)
(check-expect (valid-snake? (list (make-posn 1 2) (make-posn 1 3) (make-posn 1 4))) #true)





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

;round -> image
;draws the game based on the round structure
(define (draw-round round)
  (foldr (lambda (x y) (draw-cell-element x SNAKE-SEGMENT y))
         (if (boolean? (round-apple round))
             BACKGROUND
             (draw-cell-element (round-apple round) APPLE BACKGROUND))
         (round-snake round)))

(check-expect (draw-round STARTING-WORLD)
              (draw-cell-element SNAKE-STARTING-CELL
                                 SNAKE-SEGMENT
                                 BACKGROUND))

;posn image image -> image
;draws a cell element on to the background
;takes the posn of the element, the image of the element and the background image
(define (draw-cell-element pos element background)
  (local [(define pixel-posn (cell->pixels pos))]
    (place-image/align element (posn-x pixel-posn) (posn-y pixel-posn) "left" "top" background)))

(check-expect  (draw-cell-element (make-posn 1 1) APPLE BACKGROUND)
               (place-image/align APPLE CELL-WIDTH CELL-HEIGHT "left" "top" BACKGROUND))


;round -> round
;handles tick processing
(define (tick-handle round)
  (if (not (valid-snake? (round-snake round))) ;;make sure snake is valid
      round
      (local [(define new-snake (update-snake-position (round-snake round) (round-dir round) (round-apple round)))]
        (make-round new-snake
                    (round-dir round)
                    (if (or (boolean? (round-apple round)) (posn=? (round-apple round) (first new-snake)))  ;;generate new apple
                        (get-new-apple round)
                        (round-apple round))))))

(check-expect (round=? (tick-handle TEST-WORLD-1)
                       (make-round (update-snake-position (round-snake TEST-WORLD-1) (round-dir TEST-WORLD-1) (round-apple TEST-WORLD-1))
                                   (round-dir TEST-WORLD-1)
                                   (round-apple TEST-WORLD-1)))
              #true)
                          
              

;list[posn], symbol -> list[posn]
;updates the snakes position, does not care about bounds
(define (update-snake-position snake direction apple)
  (local [(define new-snake (cond [(symbol=? direction 'up)
                                   (cons (make-posn (posn-x (first snake)) (- (posn-y (first snake)) 1))
                                         snake)]
                                  [(symbol=? direction 'down)
                                   (cons (make-posn (posn-x (first snake)) (+ (posn-y (first snake)) 1))
                                         snake)]
                                  [(symbol=? direction 'left)
                                   (cons (make-posn (-  (posn-x (first snake)) 1)  (posn-y (first snake)))
                                         snake)]
                                  [(symbol=? direction 'right)
                                   (cons (make-posn (+ (posn-x (first snake)) 1)  (posn-y (first snake)))
                                         snake)]
                                  [else (error "update-snake-position: not given a valid direction")]))]
    (if (and (posn? apple) (posn=? (first new-snake) apple)) ;;check if snake need to be longer
        new-snake
        (remove-last new-snake))))

(check-expect (posn-ls=? (update-snake-position (list (make-posn 1 1) (make-posn 1 2)) 'up #false)
                         (list (make-posn 1 0) (make-posn 1 1)))
              #true)

(check-expect (posn-ls=? (update-snake-position (list (make-posn 1 1) (make-posn 1 2)) 'down #false)
                         (list (make-posn 1 2) (make-posn 1 1)))
              #true)

(check-expect (posn-ls=? (update-snake-position (list (make-posn 1 1) (make-posn 1 2)) 'left #false)
                         (list (make-posn 0 1) (make-posn 1 1)))
              #true)
(check-expect (posn-ls=?  (update-snake-position (list (make-posn 1 1) (make-posn 1 2)) 'right #false)
                          (list (make-posn 2 1) (make-posn 1 1)))
              #true)


;round->round
;handles player movment
(define (key-handle round key)
  (make-round (round-snake round)
              (cond [(and (not (symbol=? (round-dir round) 'down)) (or (string=? "w" key)  (string=? "up" key))) 'up] ;also make sure the new motion isn't opposite the old one
                    [(and (not (symbol=? (round-dir round) 'up))(or (string=? "s" key)  (string=? "down" key))) 'down]
                    [(and (not (symbol=? (round-dir round) 'right))(or (string=? "a" key)  (string=? "left" key))) 'left]
                    [(and (not (symbol=? (round-dir round) 'left)) (or (string=? "d" key)  (string=? "right" key))) 'right]
                    [else (round-dir round)])
              (round-apple round)))


(check-expect (round-dir (key-handle STARTING-WORLD "up")) 'up)
(check-expect (round-dir (key-handle STARTING-WORLD "w")) 'up)
(check-expect (round-dir (key-handle STARTING-WORLD "down")) 'down)
(check-expect (round-dir (key-handle STARTING-WORLD "s")) 'down)
(check-expect (round-dir (key-handle STARTING-WORLD "a")) 'right)
(check-expect (round-dir (key-handle STARTING-WORLD "left")) 'right)
(check-expect (round-dir (key-handle STARTING-WORLD "d")) 'right)
(check-expect (round-dir (key-handle STARTING-WORLD "right")) 'right)


  

(if DEBUG (test) #false)
     
  



;                                                  
;                                                  
;                                                  
;                                                  
;   ;;; ;;;                 ;;                     
;    ;   ;                   ;                     
;    ;   ;   ;;;;    ;;;;    ; ;;;; ;;  ;;  ;; ;;  
;    ;;;;;  ;    ;  ;    ;   ;  ;    ;   ;   ;;  ; 
;    ;   ;  ;    ;  ;    ;   ;;;     ;   ;   ;   ; 
;    ;   ;  ;    ;  ;    ;   ; ;     ;   ;   ;   ; 
;    ;   ;  ;    ;  ;    ;   ;  ;    ;  ;;   ;   ; 
;   ;;; ;;;  ;;;;    ;;;;   ;;  ;;;   ;; ;;  ;;;;  
;                                            ;     
;                                           ;;;    
;                                                  
;                                                  

(big-bang STARTING-WORLD
  (on-tick tick-handle TICK-RATE)
  (to-draw draw-round)
  (on-key key-handle))
