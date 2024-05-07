;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-clone) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders game

;; =================
;; Constants:

(define WIDTH  300)
(define HEIGHT 500)
(define MTS (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer
(define INVADER-WIDTH (image-width INVADER))
(define INVADER-HEIGHT (image-height INVADER))
(define INVADER-X-MIN (/ INVADER-WIDTH 2))
(define INVADER-X-MAX (- WIDTH (/ INVADER-WIDTH 2)))
(define INVADER-Y-MIN (- (ceiling (/ INVADER-HEIGHT 2))))
(define INVADER-Y-MAX (- HEIGHT (floor (/ INVADER-HEIGHT 2))))
(define INVADER-X-SPEED 3)
(define INVADER-Y-SPEED 3)
(define INVADER-SPAWNRATE 100)

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body
(define TANK-WIDTH (image-width TANK))
(define TANK-HEIGHT (image-height TANK))
(define TANK-X-MIN (/ TANK-WIDTH 2))
(define TANK-X-MAX (- WIDTH (/ TANK-WIDTH 2)))
(define TANK-SPEED 2)
(define TANK-Y (- HEIGHT (floor (/ TANK-HEIGHT 2))))

(define MISSILE-WIDTH 5)
(define MISSILE-HEIGHT 15)
(define MISSILE-Y-MAX (- HEIGHT TANK-HEIGHT (floor (/ MISSILE-HEIGHT 2)) 1))
(define MISSILE-Y-MIN (- (ceiling (/ MISSILE-HEIGHT 2))))
(define MISSILE (ellipse MISSILE-WIDTH MISSILE-HEIGHT "solid" "red"))
(define MISSILE-SPEED 10)
(define MISSILE-GAP (* MISSILE-HEIGHT MISSILE-SPEED 0.8))

(define HIT-X-RANGE (floor (/ (+ INVADER-WIDTH MISSILE-WIDTH) 2)))
(define HIT-Y-RANGE (floor (/ (+ INVADER-HEIGHT MISSILE-HEIGHT) 2)))

;; =================
;; Data Definitions:

(define-struct tank (x dir))
;; Tank is (make-tank Integer Integer[-1, 1])
;; interp. the tank location is x, TANK-Y in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir = -1, right if dir = 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))

;; -----------------

(define-struct invader (x y dir))
;; Invader is (make-invader Integer Integer Integer)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader moves INVADER-X-SPEED pixels per clock tick left if dir = -1, right if dir = 1

(define I1 (make-invader 150 100                             1))  ;not landed, moving right
(define I2 (make-invader 100 INVADER-Y-MAX -1))                   ;landed, moving left
(define I3 (make-invader 250 INVADER-Y-MAX 1))                    ;landed, moving right

#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dir invader)))

;; -----------------

(define-struct missile (x y))
;; Missile is (make-missile Integer Integer)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150                            300))                            ;not hit I1
(define M2 (make-missile (invader-x I1)                 (+ (invader-y I1) HIT-Y-RANGE))) ;exactly hit I1
(define M3 (make-missile (+ (invader-x I1) HIT-X-RANGE) (+ (invader-y I1) HIT-Y-RANGE))) ;hit I1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))

;; -----------------

(define-struct game (invaders missiles tank))
;; Game is (make-game  (ListOfInvader) (ListOfMissile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))

#;
(define (fn-for-game g)
  (... (fn-for-loinvader (game-invaders g))
       (fn-for-lom (game-missiles g))
       (fn-for-tank (game-tank g))))

;; =================
;; Functions:

;; Game -> Game
;; start the world with (main G0)
;; 
(define (main g)
  (big-bang g                                    ; Game
            (on-tick   tock)                     ; Game -> Game
            (to-draw   render-game)              ; Game -> Image
            (stop-when last-world? render-final) ; Game -> Boolean
            (on-key    handle-key)))             ; Game KeyEvent -> Game

;; -----------------

;; Game -> Game
;; produce the next Game
;(define (tock g) g) ;stub

(define (tock g)
  (resolve-collisions (make-game (spawn-invader (advance-invaders (game-invaders g)))
                                 (advance-missiles (game-missiles g))
                                 (advance-tank (game-tank g)))))

;; -----------------

;; Game -> Game
;; remove invaders and missiles of they collided
;(define (resolve-collisions g) g) ;stub

(check-expect (resolve-collisions (make-game (cons I2 (cons I1 empty))
                                             (cons M2 (cons M1 empty))
                                             T0))
              (make-game (cons I2 empty)
                         (cons M1 empty)
                         T0))
(check-expect (resolve-collisions (make-game (cons I1 (cons I2 empty))
                                             (cons M2 (cons M1 empty))
                                             T0))
              (make-game (cons I2 empty)
                         (cons M1 empty)
                         T0))
(check-expect (resolve-collisions (make-game (cons I2 (cons I1 empty))
                                             (cons M1 (cons M2 empty))
                                             T0))
              (make-game (cons I2 empty)
                         (cons M1 empty)
                         T0))
(check-expect (resolve-collisions (make-game (cons I1 (cons I2 empty))
                                             (cons M1 (cons M2 empty))
                                             T0))
              (make-game (cons I2 empty)
                         (cons M1 empty)
                         T0))

(define (resolve-collisions g)
  (cond [(or (empty? (game-invaders g))
             (empty? (game-missiles g)))
         g]
        [else
         (if (collision? (first (game-missiles g)) (game-invaders g))
             (resolve-collisions (make-game (remove-invader (first (game-missiles g)) (game-invaders g))
                                            (rest (game-missiles g))
                                            (game-tank g)))
             (make-game (game-invaders (resolve-collisions (make-game (game-invaders g)
                                                                      (rest (game-missiles g))
                                                                      (game-tank g))))
                        (cons (first (game-missiles g))
                              (game-missiles (resolve-collisions (make-game (game-invaders g)
                                                                            (rest (game-missiles g))
                                                                            (game-tank g)))))
                        (game-tank g)))]))

;; -----------------

;; Missile ListOfInvader -> Boolean
;; return true if the input missile have collided with any invader from the input ListOfInvader, return false otherwise
;(define (collision? m loi) false) ;stub

(check-expect (collision? M1 empty) false)
(check-expect (collision? M1 (cons I1 empty)) false)
(check-expect (collision? M1 (cons I2 (cons I1 empty))) false)
(check-expect (collision? M2 (cons I1 empty)) true)
(check-expect (collision? M2 (cons I2 (cons I1 empty))) true)
(check-expect (collision? M2 (cons I1 (cons I2 empty))) true)

(define (collision? m loi)
  (cond [(empty? loi) false]
        [else
         (or (and (>= (missile-x m) (- (invader-x (first loi)) HIT-X-RANGE))
                  (<= (missile-x m) (+ (invader-x (first loi)) HIT-X-RANGE))
                  (>= (missile-y m) (- (invader-y (first loi)) HIT-Y-RANGE))
                  (<= (missile-y m) (+ (invader-y (first loi)) HIT-Y-RANGE)))
             (collision? m (rest loi)))]))

;; -----------------

;; Missile ListOfInvader -> ListOfInvader
;; returns ListOfInvader with item that collided with input Missile removed
;(define (remove-invader m loi) loi) ;stub

(check-expect (remove-invader M1 empty) empty)
(check-expect (remove-invader M1 (cons I1 empty)) (cons I1 empty))
(check-expect (remove-invader M1 (cons I2 (cons I1 empty))) (cons I2 (cons I1 empty)))
(check-expect (remove-invader M2 (cons I1 empty)) empty)
(check-expect (remove-invader M2 (cons I2 (cons I1 empty))) (cons I2 empty))
(check-expect (remove-invader M2 (cons I1 (cons I2 empty))) (cons I2 empty))

(define (remove-invader m loi)
  (cond [(empty? loi) empty]
        [else
         (if (and (>= (missile-x m) (- (invader-x (first loi)) HIT-X-RANGE))
                  (<= (missile-x m) (+ (invader-x (first loi)) HIT-X-RANGE))
                  (>= (missile-y m) (- (invader-y (first loi)) HIT-Y-RANGE))
                  (<= (missile-y m) (+ (invader-y (first loi)) HIT-Y-RANGE)))
             (remove-invader m (rest loi))
             (cons (first loi) (remove-invader m (rest loi))))]))

;; -----------------

;; ListOfInvader -> ListOfInvader
;; occasionally spawn a new invader
;(define (spawn-invader loi) loi) ;stub

(define (spawn-invader loi)
  (if (= 1 (random INVADER-SPAWNRATE))
      (cons (make-invader (+ INVADER-X-MIN (random (- INVADER-X-MAX INVADER-X-MIN))) INVADER-Y-MIN 1) loi)
      loi))

;; -----------------

;; ListOfInvader -> ListOfInvader
;; advance positions of invaders
;(define (advance-invaders loi) loi) ;stub

(define (advance-invaders loi)
  (cond [(empty? loi)
         loi]
        [else
         (cons (advance-invader (first loi)) (advance-invaders (rest loi)))]))

;; -----------------

;; Invader -> Invader
;; advance position of a single invader
;(define (advance-invader i) i) ;stub

(check-expect (advance-invader I1)
              (make-invader (+ (invader-x I1) (* (invader-dir I1) INVADER-X-SPEED))
                            (+ (invader-y I1) INVADER-Y-SPEED)
                            (invader-dir I1)))
(check-expect (advance-invader (make-invader (+ INVADER-X-MIN INVADER-X-SPEED)
                                             200
                                             -1))
              (make-invader INVADER-X-MIN
                            (+ 200 INVADER-Y-SPEED)
                            1))
(check-expect (advance-invader (make-invader (+ INVADER-X-MIN INVADER-X-SPEED -1)
                                             200
                                             -1))
              (make-invader INVADER-X-MIN
                            (+ 200 INVADER-Y-SPEED)
                            1))
(check-expect (advance-invader (make-invader (+ INVADER-X-MIN INVADER-X-SPEED 1)
                                             200
                                             -1))
              (make-invader (+ (+ INVADER-X-MIN INVADER-X-SPEED 1) (* -1 INVADER-X-SPEED))
                            (+ 200 INVADER-Y-SPEED)
                            -1))
(check-expect (advance-invader (make-invader (- INVADER-X-MAX INVADER-X-SPEED)
                                             200
                                             1))
              (make-invader INVADER-X-MAX
                            (+ 200 INVADER-Y-SPEED)
                            -1))
(check-expect (advance-invader (make-invader (- INVADER-X-MAX INVADER-X-SPEED -1)
                                             200
                                             1))
              (make-invader INVADER-X-MAX
                            (+ 200 INVADER-Y-SPEED)
                            -1))
(check-expect (advance-invader (make-invader (- INVADER-X-MAX INVADER-X-SPEED 1)
                                             200
                                             1))
              (make-invader (+ (- INVADER-X-MAX INVADER-X-SPEED 1) (* 1 INVADER-X-SPEED))
                            (+ 200 INVADER-Y-SPEED)
                            1))

(define (advance-invader i)
  (cond [(and (<= (invader-x i) (+ INVADER-X-MIN INVADER-X-SPEED))
              (= -1 (invader-dir i)))
         (make-invader INVADER-X-MIN
                       (+ (invader-y i) INVADER-Y-SPEED)
                       1)]
        [(and (>= (invader-x i) (- INVADER-X-MAX INVADER-X-SPEED))
              (= 1 (invader-dir i)))
         (make-invader INVADER-X-MAX
                       (+ (invader-y i) INVADER-Y-SPEED)
                       -1)]
        [else
         (make-invader (+ (invader-x i) (* (invader-dir i) INVADER-X-SPEED))
                       (+ (invader-y i) INVADER-Y-SPEED)
                       (invader-dir i))]))

;; -----------------

;; ListOfMissile -> ListOfMissile
;; advance positions of missiles
;(define (advance-missiles g) g) ;stub

(check-expect (advance-missiles (cons M1 empty))
              (cons (advance-missile M1) empty))
(check-expect (advance-missiles (cons M2 (cons M1 empty)))
              (cons (advance-missile M2) (cons (advance-missile M1) empty)))
(check-expect (advance-missiles (cons M1 (cons (make-missile 50 (- MISSILE-Y-MIN 1)) empty)))
              (cons (advance-missile M1) empty))
(check-expect (advance-missiles (cons (make-missile 50 (- MISSILE-Y-MIN 1)) (cons M1 empty)))
              (cons (advance-missile M1) empty))

(define (advance-missiles lom)
  (cond [(empty? lom) empty]
        [else
         (if (< (missile-y (first lom)) MISSILE-Y-MIN)
             (advance-missiles (rest lom))
             (cons (advance-missile (first lom))
                   (advance-missiles (rest lom))))]))

;; -----------------

;; Missile -> Missile
;; advance position of a single missile
;(define (advance-missile m) m) ;stub

(check-expect (advance-missile M1)
              (make-missile (missile-x M1)
                            (- (missile-y M1) MISSILE-SPEED)))
(check-expect (advance-missile M2)
              (make-missile (missile-x M2)
                            (- (missile-y M2) MISSILE-SPEED)))

(define (advance-missile m)
  (make-missile (missile-x m)
                (- (missile-y m) MISSILE-SPEED)))

;; -----------------

;; Tank -> Tank
;; advance position of tank
;(define (advance-tank t) t) ;stub

(check-expect (advance-tank T1)
              (make-tank (+ (tank-x T1) (* (tank-dir T1) TANK-SPEED)) (tank-dir T1)))
(check-expect (advance-tank T2)
              (make-tank (+ (tank-x T2) (* (tank-dir T2) TANK-SPEED)) (tank-dir T2)))
(check-expect (advance-tank (make-tank TANK-X-MIN -1))
              (make-tank TANK-X-MIN -1))
(check-expect (advance-tank (make-tank (- (+ TANK-X-MIN TANK-SPEED) 1) -1))
              (make-tank TANK-X-MIN -1))
(check-expect (advance-tank (make-tank TANK-X-MAX 1))
              (make-tank TANK-X-MAX 1))
(check-expect (advance-tank (make-tank (+ (- TANK-X-MAX TANK-SPEED) 1) 1))
              (make-tank TANK-X-MAX 1))

(define (advance-tank t)
  (cond [(and (< (tank-x t) (+ TANK-X-MIN TANK-SPEED))
              (= -1 (tank-dir t)))
         (make-tank TANK-X-MIN
                    -1)]
        [(and (> (tank-x t) (- TANK-X-MAX TANK-SPEED))
              (= 1 (tank-dir t)))
         (make-tank TANK-X-MAX
                    1)]
        [else
         (make-tank (+ (tank-x t) (* (tank-dir t) TANK-SPEED))
                    (tank-dir t))]))

;; -----------------

;; Game -> Image
;; render Game
;(define (render-game g) MTS) ;stub

(check-expect (render-game G0)
              (place-image TANK (tank-x T0) TANK-Y MTS))
(check-expect (render-game G1)
              (place-image TANK (tank-x T1) TANK-Y MTS))
(check-expect (render-game G2)
              (place-image INVADER (invader-x I1) (invader-y I1)
                           (place-image MISSILE (missile-x M1) (missile-y M1)
                                        (place-image TANK (tank-x T1) TANK-Y MTS))))
(check-expect (render-game G3)
              (place-image INVADER (invader-x I1) (invader-y I1)
                           (place-image INVADER (invader-x I2) (invader-y I2)
                                        (place-image MISSILE (missile-x M1) (missile-y M1)
                                                     (place-image MISSILE (missile-x M2) (missile-y M2)
                                                                  (place-image TANK (tank-x T1) TANK-Y MTS))))))

(define (render-game g)
  (render-invaders (game-invaders g)
                   (render-missiles (game-missiles g)
                                    (place-image TANK
                                                 (tank-x (game-tank g))
                                                 TANK-Y
                                                 MTS))))

;; -----------------

;; ListOfInvader Image -> Image
;; render Invaders on the input scene
;(define (render-invaders loi scn) scn) ;stub

(check-expect (render-invaders empty MTS) MTS)
(check-expect (render-invaders (cons I1 empty) MTS)
              (place-image INVADER
                           (invader-x I1)
                           (invader-y I1)
                           MTS))
(check-expect (render-invaders (cons I2 (cons I1 empty)) MTS)
              (place-image INVADER
                           (invader-x I2)
                           (invader-y I2)
                           (place-image INVADER
                                        (invader-x I1)
                                        (invader-y I1)
                                        MTS)))

(define (render-invaders loi scn)
  (cond [(empty? loi) scn]
        [else (place-image INVADER
                           (invader-x (first loi))
                           (invader-y (first loi))
                           (render-invaders (rest loi) scn))]))

;; -----------------

;; ListOfMissile Image -> Image
;; render Missiles on the input scene
;(define (render-missiles lom scn) scn) ;stub

(check-expect (render-missiles empty MTS) MTS)
(check-expect (render-missiles (cons M1 empty) MTS)
              (place-image MISSILE
                           (missile-x M1)
                           (missile-y M1)
                           MTS))
(check-expect (render-missiles (cons M2 (cons M1 empty)) MTS)
              (place-image MISSILE
                           (missile-x M2)
                           (missile-y M2)
                           (place-image MISSILE
                                        (missile-x M1)
                                        (missile-y M1)
                                        MTS)))

(define (render-missiles lom scn)
  (cond [(empty? lom) scn]
        [else (place-image MISSILE
                           (missile-x (first lom))
                           (missile-y (first lom))
                           (render-missiles (rest lom) scn))]))

;; -----------------

;; Game -> Boolean
;; return true if an invader has landed, indicating that the game is over
;(define (last-world? g) false) ;stub

(define (last-world? g)
  (landed? (game-invaders g)))

;; -----------------

;; ListOfInvader -> Boolean
;; return true if an invader has landed
;(define (landed? loi) false) ;stub

(define (landed? loi)
  (cond [(empty? loi)
         false]
        [else
         (or (>= (invader-y (first loi)) INVADER-Y-MAX)
             (landed? (rest loi)))]))

;; -----------------

;; Game -> Image
;; render Game Over message if the game is lost
;(define (render-final g) MTS) ;stub

(define (render-final g)
  (cond [(landed? (game-invaders g))
         (overlay (overlay (text "Game Over" 30 "black")
                           (rectangle 200 50 "outline" "black"))
                  (render-game g))]))

;; -----------------

;; Game KeyEvent -> Game
;; when left or right is pressed, the tank changes direction; when space is pressed, a missile is fired
;(define (handle-key g ke) g) ;stub

(check-expect (handle-key (make-game empty empty (make-tank 50 1)) "left")
              (make-game empty empty (make-tank 50 -1)))
(check-expect (handle-key (make-game empty empty (make-tank 50 1)) "right")
              (make-game empty empty (make-tank 50 1)))
(check-expect (handle-key (make-game empty empty (make-tank 100 -1)) "right")
              (make-game empty empty (make-tank 100 1)))
(check-expect (handle-key (make-game empty empty (make-tank 100 -1)) "left")
              (make-game empty empty (make-tank 100 -1)))

(define (handle-key g ke)
  (cond [(key=? ke "left")
         (make-game (game-invaders g)
                    (game-missiles g)
                    (make-tank (tank-x (game-tank g))
                               -1))]
        [(key=? ke "right")
         (make-game (game-invaders g)
                    (game-missiles g)
                    (make-tank (tank-x (game-tank g))
                               1))]
        [(key=? ke " ")
         (if (not (just-fired? (game-missiles g)))
             (make-game (game-invaders g)
                        (cons (make-missile (tank-x (game-tank g)) MISSILE-Y-MAX)
                              (game-missiles g))
                        (game-tank g))
             g)]
        [else 
         g]))

;; -----------------

;; ListOfMissile -> Boolean
;; return true if a missile has just been fired, false otherwise
;(define (just-fired? lom) false) ;stub

(check-expect (just-fired? (cons M1 empty)) false)
(check-expect (just-fired? (cons (make-missile 30 MISSILE-Y-MAX) empty)) true)
(check-expect (just-fired? (cons (make-missile 30 (+ (- MISSILE-Y-MAX MISSILE-GAP) 1)) empty)) true)

(define (just-fired? lom)
  (cond [(empty? lom)
         false]
        [else
         (or (> (missile-y (first lom)) (- MISSILE-Y-MAX MISSILE-GAP)) (just-fired? (rest lom)))]))

;; =================
(main G0)