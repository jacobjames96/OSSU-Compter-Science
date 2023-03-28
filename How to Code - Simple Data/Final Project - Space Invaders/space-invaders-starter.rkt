;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders

;; ==========
;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 2)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 2)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define INVADER-HEIGHT/2 (/ (image-height INVADER) 2))
(define INVADER-WIDTH/2 (/ (image-width INVADER) 2))

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))

(define MTS (empty-scene WIDTH HEIGHT))

(define BLANK (square 1 "solid" "white"))

;; ==========
;; Data Definitions:

(define-struct game (invaders missiles tank clock))
;; Game is (make-game  (listof Invader) (listof Missile) Tank Natural)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position, and clock is how many ticks have passed since the start of the game

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loi (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))
       (game-clock s)))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left
(define T3 (make-tank (+ (/ WIDTH 2) (* 1 TANK-SPEED)) 1)) ; T0 after moving

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number[-1, 1])
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader moves INVADER-Y-SPEED down per clock tick, and INVADER-X-SPEED left if dx is -1 or right if dx is 1

(define I1 (make-invader 150 100 1))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -1))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 1)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))

;; ListOfInvader is one of:
;; - empty
;; (cons invader ListOfInvader)
;; Interp. as a list of the space invaders currently on the screen
(define LOI1 empty)
(define LOI2 (cons I1 (cons I2 empty)))

#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else (... (fn-for-invader (first loi))
                   (fn-for-loi (rest loi)))]))

;; Tempalte rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons invader ListOfInvader)
;; - reference: (first loi) is invader
;; - self-reference: (rest loi) is ListOfInvader


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))


;; ListOfMissile is one of:
;; - empty
;; - (cons missile ListOfMissile)
;; Interp. as a list of missiles currently on the screen
(define LOM1 empty)
(define LOM2 (cons M1 (cons M2 empty)))

(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else (... (fn-for-missile (first lom))
                   (fn-for-lom (rest lom)))]))

;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons missile ListOfMissile)
;; - reference: (first lom) is Missile
;; - self-reference: (rest lom) is ListOfMissile




(define G0 (make-game empty empty T0 0))
(define G1 (make-game empty empty T1 0))
(define G2 (make-game (list I1) (list M1) T1 0))
(define G3 (make-game (list I1 I2) (list M1 M2) T1 0))
(define G4 (make-game empty empty T2 0))

;; ==========
;; Functions:

;; game -> game
;; start the world with (main G0)
;;
(define (main g)
  (big-bang g                           ; game
    (on-tick   tock)            ; game -> game
    (to-draw   render)          ; game -> Image
    (stop-when stop)            ; game -> Boolean
    (on-key    handle-key)))    ; game KeyEvent -> game

;; game -> game
;; produce the next game state. This function should:
;; - move the tank left/right
;; - move any missiles upwards
;; - move any invaders
;; - check for collisions between missiles & invaders and remove accordingly
;; - remove missiles that have gone above the top of the screen
;; - spawn new invaders
(check-expect (tock (make-game                             ; Empty game, just the tank going rightwards
                     empty
                     empty
                     (make-tank (/ WIDTH 2) 1)
                     1))
              (make-game
               empty
               empty
               (make-tank (+ (/ WIDTH 2) (* 1 TANK-SPEED)) 1)
               2))
(check-expect (tock (make-game                                        ;Tank going rightwards with one missile on the screen
                     empty
                     (list M1)
                     T0
                     1))
              (make-game
               empty
               (list (make-missile 150 (- 300 MISSILE-SPEED)))
               T3
               2))
(check-expect (tock (make-game                                      ; Tank going right, one missile and one invader on the screen not colliding
                     (list I1)
                     (list M1)
                     (make-tank (/ WIDTH 2) 1)
                     1))
              (make-game
               (list (make-invader (+ 150 (* 1 INVADER-X-SPEED)) (+ 100 (* 1 INVADER-Y-SPEED)) 1))
               (list (make-missile 150 (- 300 MISSILE-SPEED)))
               T3
               2))
(check-expect (tock (make-game                                     ; Tank goign right, one missile and invader that have collided after moving
                     (list (make-invader 150 150 1))
                     (list (make-missile 150 160))
                     T0
                     1))
              (make-game
               empty
               empty
               T3
               2))
(check-expect (tock (make-game                               ;Tank going right, one missile that is at the top of the screen (y = 1) after it moves
                     empty
                     (list (make-missile 150 1))
                     T0
                     1))
              (make-game
               empty
               empty
               T3
               2))
(check-random (tock (make-game                               ;Tank going right, one missile that is at the top of the screen (y = 1) after it moves. Clock at 100 so add an invader
                     empty
                     (list (make-missile 150 1))
                     T0
                     100))
              (make-game
               (add-invaders 100 (random WIDTH) empty)
               empty
               T3
               101))

;(define (tock g) g) ; stub
;; Template taken from Game
(define (tock g)
  (detect-collisions ; Game -> Game
   (make-game
    (add-invaders (game-clock g) (random WIDTH) ;ListOfInvaders -> ListOfInvaders
                  (move-invaders (game-invaders g))) ; ListOfInvaders -> ListOfInvaders
    (cleanup-missiles        ; ListOfMissiles -> ListOfMissiles
     (move-missiles (game-missiles g))) ; ListOfMissiles -> ListOfMissiles
    (move-tank (game-tank g))
    (add1 (game-clock g))))) ; Tank -> Tank

;; Natural Natural[0, WIDTH] ListOfInvaders -> ListOfInvaders
;; Adds a new invader to the list when the clock is a multiple of INVADE-RATE (modulo game-clock INVADE-RATE) is 0, at the x position specified
(check-expect (add-invaders 1 0 empty) empty)
(check-expect (add-invaders 1 0 (list (make-invader 100 100 1))) (list (make-invader 100 100 1)))
(check-expect (add-invaders 100 0 empty) (list (make-invader 0 0 1)))
(check-expect (add-invaders 100 250 (list (make-invader 100 100 1))) (list (make-invader 250 0 1) (make-invader 100 100 1)))
(check-expect (add-invaders 0 0 empty) (list (make-invader 0 0 1)))
(check-expect (add-invaders 200 250 empty) (list (make-invader 250 0 1)))

;(define (add-invaders clock loi) loi)

;; Template from ListOfInvaders plus two atomic non distinct
(define (add-invaders clock x loi)
  (if (= (modulo clock INVADE-RATE) 0)
      (cons (make-invader x 0 1) loi)
      loi))


;; ListOfInvaders -> ListOfInvaders
;; Takes a list of invaders and moves each of them to new x, y coordinates
(check-expect (move-invaders empty) empty)
(check-expect (move-invaders (list
                              (make-invader 100 100 1)
                              (make-invader 100 100 -1)))
              (list (make-invader (+ 100 (* 1 INVADER-X-SPEED)) (+ 100 INVADER-Y-SPEED) 1)
                    (make-invader (+ 100 (* -1 INVADER-X-SPEED)) (+ 100 INVADER-Y-SPEED) -1)))
(check-expect (move-invaders (list
                              (make-invader WIDTH 100 1) ;; at the right, trying to move right, should flip dx and bounce off
                              (make-invader 0 100 -1))) ;; at the left trying to move left, should flip dx and bounce off
              (list (make-invader (- WIDTH INVADER-X-SPEED) (+ 100 INVADER-Y-SPEED) -1)
                    (make-invader (+ 0 (* 1 INVADER-X-SPEED)) (+ 100 INVADER-Y-SPEED) 1)))
              
;(define (move-invaders loi) loi) ; stub
;; Template from ListOfInvaders
(define (move-invaders loi)
  (cond [(empty? loi) empty]
        [else (cons (move-invader (first loi))
                    (move-invaders (rest loi)))]))

;; invader -> invader
;; Takes an individual invader and moves it to it's new x, y coordinates, including bouncing off walls where necessary
(check-expect (move-invader (make-invader 100 100 1)) (make-invader (+ 100 (* 1 INVADER-X-SPEED)) (+ 100 INVADER-Y-SPEED) 1)) ; middle, going right, no wall collision
(check-expect (move-invader (make-invader 100 100 -1)) (make-invader (+ 100 (* -1 INVADER-X-SPEED)) (+ 100 INVADER-Y-SPEED) -1)) ; middle, going left, no wall collision
(check-expect (move-invader (make-invader (- WIDTH (/ INVADER-X-SPEED 2)) 150 1)) ; one pixel from the right border, will collide and bounce off
              (make-invader (- WIDTH (/ INVADER-X-SPEED 2)) (+ 150 INVADER-Y-SPEED) -1))
(check-expect (move-invader (make-invader (+ 0 (/ INVADER-X-SPEED 2)) 150 -1))
              (make-invader (+ 0 (/ INVADER-X-SPEED 2)) (+ 150 INVADER-Y-SPEED) 1))

;(define (move-invader i) i) ; stub
;; Template from Invader
(define (move-invader i)
  (cond [(< (+ (invader-x i) (* (invader-dx i) INVADER-X-SPEED)) 0)
         (make-invader (+ (invader-x i) (- INVADER-X-SPEED (* 2 (invader-x i)))) (+ (invader-y i) INVADER-Y-SPEED) (* -1 (invader-dx i)))]
        [(> (+ (invader-x i) (* (invader-dx i) INVADER-X-SPEED)) WIDTH)
         (make-invader (+ (invader-x i) (- (* 2 WIDTH) (* 2 (invader-x i)) INVADER-X-SPEED)) (+ (invader-y i) INVADER-Y-SPEED) (* -1 (invader-dx i)))]
        [else
         (make-invader (+ (invader-x i) (* (invader-dx i) INVADER-X-SPEED)) (+ (invader-y i) INVADER-Y-SPEED) (invader-dx i))]))


;; ListOfMissiles -> ListOfMissiles
;; Takes a list of missiles and moves each of them to their new y coordinate
(check-expect (move-missiles empty) empty)
(check-expect (move-missiles (list (make-missile 150 300) (make-missile 100 0) (make-missile 260 HEIGHT)))
              (list (make-missile 150 (- 300 MISSILE-SPEED)) (make-missile 100 (- 0 MISSILE-SPEED)) (make-missile 260 (- HEIGHT MISSILE-SPEED))))

;(define (move-missiles lom) lom) ; stub
;; Template from ListOfMissiles
(define (move-missiles lom)
  (cond [(empty? lom) empty]
        [else (cons (make-missile (missile-x (first lom))
                                  (- (missile-y (first lom)) MISSILE-SPEED))
                    (move-missiles (rest lom)))]))

;; Tank -> Tank
;; Takes a tank object and moves it to new x coordinate. If the tank is at the edge of the screen already, leave it there so it doesn't go off the screen
(check-expect (move-tank (make-tank (/ WIDTH 2) 1)) (make-tank (+ (/ WIDTH 2) (* 1 TANK-SPEED)) 1)) ; tank center moving right
(check-expect (move-tank (make-tank (/ WIDTH 2) -1)) (make-tank (+ (/ WIDTH 2) (* -1 TANK-SPEED)) -1)) ; tank center moving right
(check-expect (move-tank (make-tank WIDTH 1)) (make-tank WIDTH 1)) ; tank stuck at the right
(check-expect (move-tank (make-tank 0 -1)) (make-tank 0 -1)) ; tank stuck at the left
(check-expect (move-tank (make-tank WIDTH -1)) (make-tank (+ WIDTH (* -1 TANK-SPEED)) -1)) ; tank on the right edge going left
(check-expect (move-tank (make-tank 0 1)) (make-tank (+ 0 (* 1 TANK-SPEED)) 1)) ; tank on the left edge going right

;(define (move-tank t) t) ; stub
;; Template from tank
(define (move-tank t)
  (make-tank
   (cond [(<= (+ (tank-x t) (* (tank-dir t) TANK-SPEED)) 0) 0]
         [(>= (+ (tank-x t) (* (tank-dir t) TANK-SPEED)) WIDTH) WIDTH]
         [else (+ (tank-x t) (* (tank-dir t) TANK-SPEED))])
   (tank-dir t)))


;; Game -> Game
;; Takes the current state of a game (particularly missiles and invaders) and removes and invaders that have been hit by a missile (Also removing the missile)
(check-expect (detect-collisions (make-game empty empty T0 0)) (make-game empty empty T0 0))                           ; No invaders or missiles
(check-expect (detect-collisions (make-game empty (list (make-missile 100 100) (make-missile 250 250)) T0 0))          ; Only missiles, no invaders
              (make-game empty (list (make-missile 100 100) (make-missile 250 250)) T0 0))
(check-expect (detect-collisions (make-game (list (make-invader 150 150 1) (make-invader 175 175 1)) empty T0 0))       ; Only invaders, no missiles
              (make-game (list (make-invader 150 150 1) (make-invader 175 175 1)) empty T0 0))
(check-expect (detect-collisions (make-game (list (make-invader 150 150 1) (make-invader 175 175 1))                    ; Invaders and missiles, no collisions
                                            (list (make-missile 100 100) (make-missile 250 250)) T0 0))
              (make-game (list (make-invader 150 150 1) (make-invader 175 175 1))                    
                         (list (make-missile 100 100) (make-missile 250 250)) T0 0))
(check-expect (detect-collisions (make-game (list (make-invader 150 150 1) (make-invader 175 175 1))                    ; One exact collision
                                            (list (make-missile 150 150) (make-missile 250 250)) T0 0))
              (make-game (list (make-invader 175 175 1)) (list (make-missile 250 250)) T0 0))
(check-expect (detect-collisions (make-game (list (make-invader 150 150 1) (make-invader 175 175 1))                    ; inexact hit one missile
                                            (list (make-missile 155 146) (make-missile 250 250)) T0 0))
              (make-game (list (make-invader 175 175 1)) (list (make-missile 250 250)) T0 0))


;(define (detect-collisions g) g) ; stub

; Template from Game
(define (detect-collisions g)
  (make-game
   (collisions-invaders (game-invaders g) (game-missiles g)) ;; ListOfInvaders ListOfMissiles -> ListOfInvaders
   (collisions-missiles (game-missiles g) (game-invaders g)) ;; ListOfMissiles ListOfInvaders -> ListOfMissiles
   (game-tank g)
   (game-clock g)))


;; ListOfInvaders ListOfMissiles -> ListOfInvaders
;; Take a list of invaders and a list of missiles. Remove any invaders from the list where there is a collision with any missile in the missile list
(check-expect (collisions-invaders empty empty) empty)
(check-expect (collisions-invaders empty (list (make-missile 100 100) (make-missile 150 150))) empty)
(check-expect (collisions-invaders (list (make-invader 100 100 1) (make-invader 150 150 1)) empty) (list (make-invader 100 100 1) (make-invader 150 150 1)))
(check-expect (collisions-invaders (list (make-invader 100 100 1) (make-invader 200 200 1))
                                   (list (make-missile 150 150) (make-missile 50 250)))
              (list (make-invader 100 100 1) (make-invader 200 200 1)))
(check-expect (collisions-invaders (list (make-invader 100 100 1) (make-invader 200 200 1))
                                   (list (make-missile 100 100) (make-missile 50 250)))
              (list (make-invader 200 200 1)))
(check-expect (collisions-invaders (list (make-invader 100 100 1) (make-invader 200 200 1))
                                   (list (make-missile 95 100) (make-missile 200 200)))
              empty)

;(define (collisions-invaders loi lom) loi)

; Template from ListOfInvaders
(define (collisions-invaders loi lom)
  (cond [(empty? loi) empty]
        [else (if (colliding-missiles? (first loi) lom) ;; Invader ListOfMissiles
                  (collisions-invaders (rest loi) lom)
                  (cons (first loi) (collisions-invaders (rest loi) lom))
                  )]))

;; Invader ListOfMissiles -> Boolean
;; Takes an invader and a list of missiles and returns true if the invader collides with a missile in the list otherwise returns false
(check-expect (colliding-missiles? (make-invader 100 100 1) empty) false)
(check-expect (colliding-missiles? (make-invader 100 100 1) (list (make-missile 50 50) (make-missile 150 150))) (or false (or false false)))
(check-expect (colliding-missiles? (make-invader 100 100 1) (list (make-missile 100 100) (make-missile 150 150))) (or true (or false false)))
(check-expect (colliding-missiles? (make-invader 100 100 1) (list (make-missile 50 50) (make-missile 100 100))) (or false (or true false)))

;(define (colliding-missiles? i lom) false); stub

;; Template from ListOfMissiles with compound
(define (colliding-missiles? i lom)
  (cond [(empty? lom) false]
        [else (or (collides? i (first lom) ) ;; Missile Invader -> Boolean
                  (colliding-missiles? i (rest lom)))]))

;; Missile Invader -> Boolean
;; Takes a missile and an invader and returns true if they are colliding within the invaders hitbox, otherwise returns false
(check-expect (collides? (make-invader 100 100 1) (make-missile 50 50)) false)
(check-expect (collides? (make-invader 100 100 1) (make-missile 100 100)) true)
(check-expect (collides? (make-invader 100 100 1) (make-missile (- 100 HIT-RANGE) (- 100 HIT-RANGE))) true) ;; Top Left
(check-expect (collides? (make-invader 100 100 1) (make-missile (+ 100 HIT-RANGE) (- 100 HIT-RANGE))) true) ;; Top Right
(check-expect (collides? (make-invader 100 100 1) (make-missile (- 100 HIT-RANGE) (+ 100 HIT-RANGE))) true) ;; Bottom Left
(check-expect (collides? (make-invader 100 100 1) (make-missile (+ 100 HIT-RANGE) (+ 100 HIT-RANGE))) true) ;; Bottom Right

;(define (collides? m i) false) ; stub

;; Combine Templates for Invader and Missile
(define (collides? i m)
  (and (<= (missile-x m) (+ (invader-x i) HIT-RANGE))
       (>= (missile-x m) (- (invader-x i) HIT-RANGE))
       (>= (missile-y m) (- (invader-y i) HIT-RANGE))
       (<= (missile-y m) (+ (invader-y i) HIT-RANGE))))


;; ListOfMissiles ListOfInvaders -> ListOfMissiles
;; Take a list of invaders and a list of missiles. Remove any missiles from the list where there is a collision with any invader in the invader list
(check-expect (collisions-missiles empty empty) empty)
(check-expect (collisions-missiles (list (make-missile 100 100) (make-missile 150 150)) empty ) (list (make-missile 100 100) (make-missile 150 150)))
(check-expect (collisions-missiles empty (list (make-invader 100 100 1) (make-invader 150 150 1))) empty)
(check-expect (collisions-missiles (list (make-missile 150 150) (make-missile 50 250))
                                   (list (make-invader 100 100 1) (make-invader 200 200 1)))
              (list (make-missile 150 150) (make-missile 50 250)))
(check-expect (collisions-missiles (list (make-missile 100 100) (make-missile 50 250))
                                   (list (make-invader 100 100 1) (make-invader 200 200 1)))
              (list (make-missile 50 250)))
(check-expect (collisions-missiles (list (make-missile 95 100) (make-missile 200 200))
                                   (list (make-invader 100 100 1) (make-invader 200 200 1)))
              empty)

;(define (collisions-missiles lom loi) lom)

;; Template from ListOfMissiles
(define (collisions-missiles lom loi)
  (cond [(empty? lom) empty]
        [else (if (colliding-invaders? (first lom) loi) ;; Missile ListOfInvaders -> Boolean
                  (collisions-missiles (rest lom) loi)
                  (cons (first lom) (collisions-missiles (rest lom) loi)))]))

;; Missile ListOfInvaders -> Boolean
;; Takes an missile and a list of invaders and returns true if the missile collides with an invader in the list otherwise returns false
(check-expect (colliding-invaders? (make-missile 100 100) empty) false)
(check-expect (colliding-invaders? (make-missile 100 100) (list (make-invader 50 50 1) (make-invader 150 150 1))) (or false (or false false)))
(check-expect (colliding-invaders? (make-missile 100 100) (list (make-invader 50 50 1) (make-invader 100 100 1))) (or false (or true false)))

;(define (colliding-invaders? m loi) false) ; stub

;; Template from ListOfInvaders with compound
(define (colliding-invaders? m loi)
  (cond [(empty? loi) false]
        [else (or (collides? (first loi) m)
                   (colliding-invaders? m (rest loi)))]))


;; ListOfMissiles -> ListOfMissiles
;; Takes a list of missiles and removes any that are now at/above the top of the screen
(check-expect (cleanup-missiles empty) empty)
(check-expect (cleanup-missiles (list (make-missile 150 100) (make-missile 200 150))) ;; no missiles at/above top of screen
              (list (make-missile 150 100) (make-missile 200 150)))
(check-expect (cleanup-missiles (list (make-missile 150 0) (make-missile 200 300))) ;; one missile at top of screen (First missile)
              (list (make-missile 200 300)))
(check-expect (cleanup-missiles (list (make-missile 150 0) (make-missile 74 (- 0 10)))) ;; all missiles above top of screen
              empty)

;(define (cleanup-missiles lom) lom) ; empty
;; Template from ListOfMissiles
(define (cleanup-missiles lom)
  (cond [(empty? lom) empty]
        [else (if
               (off-screen? (first lom))
               (cleanup-missiles (rest lom))
               (cons (first lom) (cleanup-missiles (rest lom))))]))

;; Missile -> Boolean
;; Takes a missile and returns true if it's current x position
(check-expect (off-screen? (make-missile 200 0)) true)
(check-expect (off-screen? (make-missile 200 (- 0 10))) true)
(check-expect (off-screen? (make-missile 200 11)) false)

;(define (off-screen? m) false) ;stub
;; Template from Missile
(define (off-screen? m)
  (<= (missile-y m) 0))

;; game -> Image
;; render the current game state onto the MTS
(check-expect (render G0)
              (render-invaders (game-invaders G0)
                               (render-missiles (game-missiles G0)
                                                (render-tank (game-tank G0)))))
(check-expect (render G1)
              (render-invaders (game-invaders G1) (render-missiles (game-missiles G1) (render-tank (game-tank G1)))))
(check-expect (render G2)
              (render-invaders (game-invaders G2) (render-missiles (game-missiles G2) (render-tank (game-tank G2)))))
(check-expect (render G3)
              (render-invaders (game-invaders G3) (render-missiles (game-missiles G3) (render-tank (game-tank G3)))))

;(define (render g) MTS) ; stub

;; Template from Game
(define (render g)
  (render-invaders (game-invaders g) (render-missiles (game-missiles g) (render-tank (game-tank g)))))

;; Tank -> Image
;; Take a tank and render it onto the empty scene
(check-expect (render-tank T0) (place-image TANK (tank-x T0) (- HEIGHT TANK-HEIGHT/2) MTS))

;(define (render-tank t) MTS) ; stub

;; Template from Tank
(define (render-tank t)
  (place-image TANK (tank-x t) (- HEIGHT TANK-HEIGHT/2) MTS))


;; ListOfMissiles Image -> Image
;; Take a list of missiles and a base image, render the missiles onto the base image
(check-expect (render-missiles empty (render-tank T0)) (render-tank T0))
(check-expect (render-missiles (list (make-missile 50 50) (make-missile 100 100)) (render-tank T0))
              (place-image MISSILE 50 50 (place-image MISSILE 100 100 (render-tank T0))))

;(define (render-missiles lom b) MTS) ; stub

;; Take template from ListOfMissiles, add atomic distinct
(define (render-missiles lom b)
  (cond [(empty? lom) b]
        [else (place-image MISSILE (missile-x (first lom)) (missile-y (first lom)) (render-missiles (rest lom) b))]))


;; ListOfInvaders Image -> Image
;; Take a list of invaders and a base image, render the invaders onto the base image
(check-expect (render-invaders empty
                               (render-missiles (list (make-missile 50 50) (make-missile 100 100)) (render-tank T0)))
              (render-missiles (list (make-missile 50 50) (make-missile 100 100)) (render-tank T0)))
(check-expect (render-invaders (list (make-invader 75 130 1) (make-invader 256 13 -1))
                               (render-missiles (list (make-missile 50 50) (make-missile 100 100)) (render-tank T0)))
              (place-image INVADER 75 130 (place-image INVADER 256 13 (render-missiles (list (make-missile 50 50) (make-missile 100 100)) (render-tank T0)))))

;(define (render-invaders loi b) MTS) ; stub

;; Template from ListOfInvaders plus atomic distinct
(define (render-invaders loi b)
  (cond [(empty? loi) b]
        [else (place-image INVADER (invader-x (first loi)) (invader-y (first loi)) (render-invaders (rest loi) b))]))


;; game -> Boolean
;; stop the game when an invader is at the bottom of the screen
;; !!!
(check-expect (stop (make-game empty empty T0 1)) false)
(check-expect (stop (make-game (list (make-invader 100 100 1) (make-invader 200 100 1)) empty T0 1)) false)
(check-expect (stop (make-game (list (make-invader 100 100 1) (make-invader 200 HEIGHT 1)) empty T0 1)) true)
(check-expect (stop (make-game (list (make-invader 100 100 1) (make-invader 200 (+ HEIGHT 10) 1)) empty T0 1)) true)

;(define (stop g) false) ; stub

;; Template from game
(define (stop g)
  (at-bottom? (game-invaders g))) ;; ListOfInvaders -> Boolean


;; ListOfInvader -> Boolean
;; Takes a list of invaders and returns true if any of them are at the bottom of the screen (i.e. y >= HEIGHT)
(check-expect (at-bottom? empty) false)
(check-expect (at-bottom? (list (make-invader 100 100 1) (make-invader 200 100 1))) (or false (or false false)))
(check-expect (at-bottom? (list (make-invader 100 100 1) (make-invader 200 HEIGHT 1))) (or false (or true false)))
(check-expect (at-bottom? (list (make-invader 100 100 1) (make-invader 200 (+ HEIGHT 10) 1))) (or false (or true false)))

;(define (at-bottom? loi) false) ; stub

;; Template from ListOfInvader
(define (at-bottom? loi)
  (cond [(empty? loi) false]
        [else (or (>= (invader-y (first loi)) HEIGHT) (at-bottom? (rest loi)))]))

;; game KeyEvent -> game
;; handle key inputs: left/right control direction of the tank, pressing space shoots a bullet
(check-expect (handle-key G0 " ") (make-game
                                   (game-invaders G0)
                                   (cons (make-missile (tank-x (game-tank G0)) (- HEIGHT TANK-HEIGHT/2)) (game-missiles G0))
                                   (game-tank G0)
                                   (game-clock G0)))
(check-expect (handle-key G2 " ") (make-game
                                   (game-invaders G2)
                                   (cons (make-missile (tank-x (game-tank G2)) (- HEIGHT TANK-HEIGHT/2)) (game-missiles G2))
                                   (game-tank G2)
                                   (game-clock G2)))
(check-expect (handle-key G0 "left") (make-game
                                      (game-invaders G0)
                                      (game-missiles G0)
                                      (make-tank (tank-x (game-tank G0)) -1)
                                      (game-clock G0)))
(check-expect (handle-key G4 "left") (make-game
                                      (game-invaders G4)
                                      (game-missiles G4)
                                      (make-tank (tank-x (game-tank G4)) -1)
                                      (game-clock G4)))
(check-expect (handle-key G0 "right") (make-game
                                       (game-invaders G0)
                                       (game-missiles G0)
                                       (make-tank (tank-x (game-tank G0)) 1)
                                       (game-clock G0)))
(check-expect (handle-key G4 "right") (make-game
                                       (game-invaders G4)
                                       (game-missiles G4)
                                       (make-tank (tank-x (game-tank G4)) 1)
                                       (game-clock G4)))
(check-expect (handle-key G4 "a") G4)

;(define (handle-key g ke) g) ; stub

(define (handle-key g ke)
  (cond
    [(key=? ke " ") (make-game (game-invaders g) (cons (make-missile (tank-x (game-tank g)) (- HEIGHT TANK-HEIGHT/2)) (game-missiles g)) (game-tank g) (game-clock g))]
    [(key=? ke "left") (make-game (game-invaders g) (game-missiles g) (make-tank (tank-x (game-tank g)) -1) (game-clock g))]
    [(key=? ke "right") (make-game (game-invaders g) (game-missiles g) (make-tank (tank-x (game-tank g)) 1) (game-clock g))]
    [else g]))