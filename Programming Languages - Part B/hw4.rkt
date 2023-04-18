
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))


(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))


(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (let ([i (remainder n (length xs))])
              (car (list-tail xs i)))]))


(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (let ([pr (s)])
        (cons (car pr) (stream-for-n-steps (cdr pr) (- n 1))))))


(define funny-number-stream
  (letrec ([f (lambda (x) (cons (if (= (modulo x 5) 0)
                                    (- x)
                                    x)
                                (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))


(define dan-then-dog
  (letrec ([f (lambda (x) (cons x (lambda () (f (if (string=? x "dan.jpg")
                                                    "dog.jpg"
                                                    "dan.jpg")))))])
    (lambda () (f "dan.jpg"))))


(define (stream-add-zero s)
  (letrec ([f (lambda (x) (cons (cons 0 (car (x))) (lambda () (f (cdr (x))))))])
    (lambda () (f s))))


(define (cycle-lists xs ys)
  (letrec ([f (lambda (n) (cons (cons
                                 (list-nth-mod xs n)
                                 (list-nth-mod ys n))
                                (lambda () (f (+ n 1)))))])
    (lambda () (f 0))))


(define (vector-assoc v vec)
  (letrec ([len (vector-length vec)]
           [f (lambda (n)
                (cond [(>= n len) #f]
                      [(pair? (vector-ref vec n)) (if (equal? v (car (vector-ref vec n)))
                                                      (vector-ref vec n)
                                                      (f (+ n 1)))]
                      [#t (f (+ n 1))]))])
    (f 0)))


(define (cached-assoc xs n)
  (letrec ([cache (make-vector n #f)]
           [cache-slot 0]
           [f (lambda (v)
                (let ([ans (vector-assoc v cache)])
                  (if ans
                      ans
                      (let ([assoc-ans (assoc v xs)])
                        (if assoc-ans
                            (begin (vector-set! cache cache-slot assoc-ans)
                                   (set! cache-slot (+ cache-slot 1))
                                   assoc-ans)
                            #f)))))])
    f))


(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (let ([v1 e1])
       (letrec ([loop (lambda ()
                        (if (>= e2 v1)
                            #t 
                            (loop)))])
         (loop)))]))