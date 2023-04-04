;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ta-solver-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; ta-solver-starter.rkt
(require racket/list) ;gets list-ref, take and drop


;  PROBLEM 1:
;
;  Consider a social network similar to Twitter called Chirper. Each user has a name, a note about
;  whether or not they are a verified user, and follows some number of people.
;
;  Design a data definition for Chirper, including a template that is tail recursive and avoids
;  cycles.
;
;  Then design a function called most-followers which determines which user in a Chirper Network is
;  followed by the most people.
;

(define-struct user (name verified? following))
;; User is (make-user String Boolean listofUser)
;; Interp. as a user's name, true if they are verified otherwise false, and a list of users that they follow
(define U1 (make-user "J" true empty))
(define U2 (make-user "A" true (list (make-user "B" false empty))))
(define U3
  (shared ((-A- (make-user "A" true (list -B- -F-)))
           (-B- (make-user "B" false (list -A-)))
           (-C- (make-user "C" true (list -F- -D-)))
           (-D- (make-user "D" true (list -F- -C-)))
           (-E- (make-user "E" true (list -F- -A- -B-)))
           (-F- (make-user "F" false empty))
           (-G- (make-user "G" true (list -F-)))
           (-H- (make-user "H" false (list -B-)))
           (-I- (make-user "I" true (list -D-)))
           (-J- (make-user "J" true (list -B-))))
    -A-))

;; Template for an arb-arity tree, encapsulated by local, tail-recursive with worklist accumulator
;;                                          and context accumulator to track users already visited
#;
(define (fn-for-chirper u0)
  ;; todo is listof user; worklist accumulator for users still to process
  ;; visited is listof String; a context accumulator, listing users that have already been visited
  (local [(define (fn-for-user u todo visited)
            ;(user-name u)
            ;(user-verified? u)
            (if (member (user-name u) visited)
                (fn-for-lou todo visited)
                (fn-for-lou (append (user-following u) todo)
                            (cons (user-name u) visited))))
          
          (define (fn-for-lou todo visited)
            (cond [(empty? todo) (...)]
                  [else
                   (fn-for-user (first todo)
                                (rest todo)
                                visited)]))]

    (fn-for-user u0 empty empty)))


;; User -> User
;; Returns the user with the most followers. Assumes at least one user has followers
(check-expect (most-followers U2) (make-user "B" false empty))
(check-expect (most-followers U3) (shared ((-A- (make-user "A" true (list -B- -F-)))
                                           (-B- (make-user "B" false (list -A-)))
                                           (-C- (make-user "C" true (list -F- -D-)))
                                           (-D- (make-user "D" true (list -F- -C-)))
                                           (-E- (make-user "E" true (list -F- -A- -B-)))
                                           (-F- (make-user "F" false empty))
                                           (-G- (make-user "G" true (list -F-)))
                                           (-H- (make-user "H" false (list -B-)))
                                           (-I- (make-user "I" true (list -D-)))
                                           (-J- (make-user "J" true (list -B-))))
                                    -F-))

;(define (most-followers u) u)

(define (most-followers u0)
  ;; todo is listof user; worklist accumulator for users still to process
  ;; visited is listof String; a context accumulator, listing users that have already been visited
  ;; rsf is (listof usrFwrs); result so far, a list of users and the number of followers they've had so far
  (local [(define-struct usrFwrs (user followers))
          ;; usrFwrs is (make-usrFwrs (User Natural))
          ;; interp. as a user and the number of people following them

          (define (fn-for-user u todo visited rsf)
            ;(user-name u)
            ;(user-verified? u)
            (if (member (user-name u) visited)
                (fn-for-lou todo visited rsf)
                (fn-for-lou (append (user-following u) todo)
                            (cons (user-name u) visited)
                            (merge-user u rsf))))                      ;; (listof usrFwrs) User -> (listof usrFwrs)

          (define (merge-user u rsf)
            (foldr merge-one-user rsf (user-following u)))
          
          ; add a user to the louf (listof usrFwrs)
          (define (merge-one-user user louf)
            (cond [(empty? louf) (list (make-usrFwrs user 1))]
                  [else 
                   (if (string=? (user-name user) (user-name (usrFwrs-user (first louf))))
                       (cons (make-usrFwrs user (add1 usrFwrs-followers (first louf))) (rest louf))
                       (cons (first louf) (merge-one-user user (rest louf))))]))

          (define (fn-for-lou todo visited rsf)
            (cond [(empty? todo) rsf]
                  [else
                   (fn-for-user (first todo)
                                (rest todo)
                                visited
                                rsf)]))

          (define (max-followers louf)
            (usrFwrs-user (foldr (λ (u1 u2)
                                   (if (> (usrFwrs-followers u1) (usrFwrs-followers u2))
                                       u1
                                       u2))
                                 (first louf)
                                 (rest louf))))]

    (max-followers (fn-for-user u0 empty empty empty))))

;  PROBLEM 2:
;
;  In UBC's version of How to Code, there are often more than 800 students taking
;  the course in any given semester, meaning there are often over 40 Teaching Assistants.
;
;  Designing a schedule for them by hand is hard work - luckily we've learned enough now to write
;  a program to do it for us!
;
;  Below are some data definitions for a simplified version of a TA schedule. There are some
;  number of slots that must be filled, each represented by a natural number. Each TA is
;  available for some of these slots, and has a maximum number of shifts they can work.
;
;  Design a search program that consumes a list of TAs and a list of Slots, and produces one
;  valid schedule where each Slot is assigned to a TA, and no TA is working more than their
;  maximum shifts. If no such schedules exist, produce false.
;
;  You should supplement the given check-expects and remember to follow the recipe!



;; Slot is Natural
;; interp. each TA slot has a number, is the same length, and none overlap
(define SL0 (list 1 2 3 4))

(define-struct ta (name max avail))
;; TA is (make-ta String Natural (listof Slot))
;; interp. the TA's name, number of slots they can work, and slots they're available for

(define SOBA (make-ta "Soba" 2 (list 1 3)))
(define UDON (make-ta "Udon" 1 (list 3 4)))
(define RAMEN (make-ta "Ramen" 1 (list 2)))

(define NOODLE-TAs (list SOBA UDON RAMEN))



(define-struct assignment (ta slot))
;; Assignment is (make-assignment (TA or false) Slot)
;; interp. the TA is assigned to work the slot

;; Schedule is (listof Assignment)


;; ============================= FUNCTIONS


;; (listof TA) (listof Slot) -> Schedule or false
;; produce valid schedule given TAs and Slots; false if impossible

(check-expect (schedule-tas empty empty) empty)
(check-expect (schedule-tas empty (list 1 2)) false)
(check-expect (schedule-tas (list SOBA) empty) empty)

(check-expect (schedule-tas (list SOBA) (list 1)) (list (make-assignment SOBA 1)))
(check-expect (schedule-tas (list SOBA) (list 2)) false)
(check-expect (schedule-tas (list SOBA) (list 1 3)) (list (make-assignment SOBA 1)
                                                          (make-assignment SOBA 3)))

(check-expect (schedule-tas NOODLE-TAs (list 1 2 3 4))
              (list
               (make-assignment SOBA 1)
               (make-assignment RAMEN 2)
               (make-assignment SOBA 3)
               (make-assignment UDON 4)))

(check-expect (schedule-tas NOODLE-TAs (list 1 2 3 4 5)) false)

(define (schedule-tas tas slots)
  (local [(define (initial-schedule slots)
            (map (λ (n) (make-assignment false n)) slots))

          (define (solve--schedule s)
            (if (solved? s) ;; Schedule -> Boolean
                s ;; Schedule
                (solve--los 
                 (next-schedules s tas)))) ;; Schedule -> listof Schedule

          (define (solve--los los)
            (cond [(empty? los) false]
                  [else
                   (local [(define try (solve--schedule (first los)))]
                     (if (not (false? try))
                         try
                         (solve--los (rest los))))]))]
    
    (solve--schedule
     (initial-schedule slots))))

;; Schedule -> Boolean
;; Returns true if a schedule has no 'empty' slots
(check-expect (solved? empty) true)
(check-expect (solved? (list (make-assignment false 1) (make-assignment false 2))) false)
(check-expect (solved? (list (make-assignment SOBA 1) (make-assignment false 2))) false)
(check-expect (solved? (list (make-assignment SOBA 1) (make-assignment RAMEN 2))) true)

(define (solved? s)
  (cond [(empty? s) true]
        [else
         (and (ta? (assignment-ta (first s)))
              (solved? (rest s)))]))


;; Schedule (listof TA) -> (listof Schedule)
;; Returns a list of schedules where the next empty slot is filled with relevent TAs
(check-expect (next-schedules (list (make-assignment false 1) (make-assignment false 2))
                              NOODLE-TAs)
              (list (list (make-assignment SOBA 1) (make-assignment false 2))))
(check-expect (next-schedules (list (make-assignment SOBA 1) (make-assignment RAMEN 2) (make-assignment false 3))
                              NOODLE-TAs)
              (list (list (make-assignment SOBA 1) (make-assignment RAMEN 2) (make-assignment SOBA 3))
                    (list (make-assignment SOBA 1) (make-assignment RAMEN 2) (make-assignment UDON 3))))

;(define (next-schedules s tas) empty)
(define (next-schedules s tas)
  (filter-invalid tas                                         ; (listof Schedule) (listof TA) -> (listof Schedule)
                  (fill-false-with-tas s tas                  ; Schedule (listof TA) Natural -> (listof Schedule)
                                       (find-false s))))      ; Schedule -> Natural


;; Schedule -> Natural
;; Returns the position in a given schedule where the first assignment with a false TA can be found
(check-expect (find-false (list (make-assignment false 1) (make-assignment false 2))) 0)
(check-expect (find-false (list (make-assignment SOBA 1)
                                (make-assignment RAMEN 2)
                                (make-assignment UDON 3)
                                (make-assignment false 4)))
              3)

(define (find-false s0)
  ;; rsf: worklist accumulator; the position in the list we are currently at
  (local [(define (find-false s rsf)
            (cond [(empty? s) (error "No Empty Found")]
                  [else (if (false? (assignment-ta (first s)))
                            rsf
                            (find-false (rest s) (add1 rsf)))]))]

    (find-false s0 0)))

;; Schedule (listof TA) Natural -> (listof Schedule)
;; Returns a list of schedules with the first empty slot filled with each available TA.
;;                     Returns empty list if there are no available TAs for that slot
(check-expect (fill-false-with-tas (list (make-assignment false 12) (make-assignment false 5))
                                   NOODLE-TAs
                                   0)
              empty)
(check-expect (fill-false-with-tas (list (make-assignment false 1) (make-assignment false 2))
                                   NOODLE-TAs
                                   0)
              (list (list (make-assignment SOBA 1) (make-assignment false 2))))
(check-expect (fill-false-with-tas (list (make-assignment false 3) (make-assignment false 4))
                                   NOODLE-TAs
                                   0)
              (list (list (make-assignment SOBA 3) (make-assignment false 4))
                    (list (make-assignment UDON 3) (make-assignment false 4))))

;(define (fill-false-with-tas s tas posn) empty)
(define (fill-false-with-tas s tas posn)
  (local [(define slot-to-fill (assignment-slot (list-ref s posn)))]
    (cond [(empty? tas) empty]
          [else
           (if (member slot-to-fill (ta-avail (first tas)))
               (cons
                (fill-posn
                 s
                 (make-assignment (first tas) slot-to-fill)
                 posn) ;; Schedule TA Natural -> Schedule
                (fill-false-with-tas s (rest tas) posn))
               (fill-false-with-tas s (rest tas) posn))])))

;; Schedule TA Natural -> Schedule
;; Fills a given position in a schedule with an assignment for the given TA
(check-expect (fill-posn (list (make-assignment false 1) (make-assignment false 2))
                         (make-assignment UDON 1)
                         0)
              (list (make-assignment UDON 1) (make-assignment false 2)))
(check-expect (fill-posn (list (make-assignment false 1) (make-assignment false 2))
                         (make-assignment UDON 2)
                         1)
              (list (make-assignment false 1) (make-assignment UDON 2)))

;(define (fill-posn s ta posn) s)
(define (fill-posn s nv posn)
  (append (take s posn)
          (list nv)
          (drop s (add1 posn))))
        
;; (listof TA) (listof Schedule)  -> (listof Schedule)
;; Removes invalid schedules from the list of schedule. Schedule is invalid if it assigns more slots to a TA than their max allowed
;; !!!
(check-expect (filter-invalid NOODLE-TAs empty) empty)
(check-expect (filter-invalid NOODLE-TAs (list (list (make-assignment UDON 1) (make-assignment UDON 2) (make-assignment UDON 3))))
              empty)
(check-expect (filter-invalid NOODLE-TAs (list (list (make-assignment SOBA 1) (make-assignment SOBA 3))
                                               (list (make-assignment SOBA 1) (make-assignment RAMEN 2))))
              (list (list (make-assignment SOBA 1) (make-assignment SOBA 3))
                    (list (make-assignment SOBA 1) (make-assignment RAMEN 2))))
(check-expect (filter-invalid NOODLE-TAs (list (list (make-assignment UDON 1) (make-assignment UDON 2) (make-assignment UDON 3))
                                               (list (make-assignment SOBA 1) (make-assignment RAMEN 2) (make-assignment UDON 3))))
              (list (list (make-assignment SOBA 1) (make-assignment RAMEN 2) (make-assignment UDON 3))))


(define (filter-invalid tas losa)
  (local [(define (fn-for-s s tas)                          ;; Schedule (listof TAs) -> Boolean
            (cond [(empty? tas) true]
                  [else
                   (and (<= (count-slots (first tas) s)        ;; TA Schedule -> Natural
                            (ta-max (first tas)))
                        (fn-for-s s (rest tas)))]))

          (define (fn-for-los los)                      ;; (listof Schedule) -> (listof Schedule)
            (cond [(empty? los) empty]
                  [else
                   (if (fn-for-s (first los) tas)          ;; Schedule (listof TAs) -> Boolean
                       (cons (first los) (fn-for-los (rest los)))
                       (fn-for-los (rest los)))]))]

    (fn-for-los losa)))

;; TA Schedule -> Natural
;; Returns the number of slots in a given schedule that the TA is assigned to
(check-expect (count-slots UDON (list (make-assignment UDON 1) (make-assignment UDON 2) (make-assignment UDON 3))) 3)
(check-expect (count-slots RAMEN (list (make-assignment UDON 1) (make-assignment UDON 2) (make-assignment UDON 3))) 0)
(check-expect (count-slots UDON empty) 0)

;(define (count-slots ta s) 0)

(define (count-slots ta s0)
  ;; rsf is Natural; a result so far accumulator
  (local [(define (count-slots ta s rsf)
            (cond [(empty? s) rsf]
                  [(false? (assignment-ta (first s))) (count-slots ta (rest s) rsf)]
                  [else
                   (if (string=? (ta-name ta) (ta-name (assignment-ta (first s))))
                       (count-slots  ta (rest s) (add1 rsf))
                       (count-slots ta (rest s) rsf))]))]

    (count-slots ta s0 0)))
