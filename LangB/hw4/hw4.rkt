
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

(define ones (lambda () (cons 1 ones)))

(define powers-of-two
  (letrec ([f (lambda (x) (cons x (lambda () (f (* x 2)))))])
    (lambda () (f 2))))

;; put your code below
(define (sequence low high stride)
  (if (positive? (- (+ high 1) low))
      (cons low (sequence (+ low stride) high stride))
      null))

(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

(define (list-nth-mod xs n)
  (cond [(< n 0) ((error "list-nth-mod: negative number"))]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))
                    

(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

(define funny-number-stream
  (letrec ([f (lambda (num) (cons (if (= (remainder num 5) 0) (- num) num) (lambda () (f (+ num 1)))))])
    (lambda () (f 1))))

(define dan-then-dog
  (letrec ([f (lambda (i) (cons (if (= (remainder i 2) 0) "dan.jpg" "dog.jpg") (lambda () (f (+ i 1)))))])
    (lambda () (f 0))))

(define (stream-add-zero s)
 (lambda () (let ([pair (s)])
   (cons (cons 0 (car pair)) (stream-add-zero (cdr pair))))))
 
;(define (cycle-lists xs ys)
 ; (cons (car xs) (car ys) (cycle-list-helper xs)))

(define (cycle-lists xs ys)
  (letrec ([f (lambda (xs ys)
                (cons (cons (car xs) (car ys)) (cycle-lists (cycle-list-helper xs) (cycle-list-helper ys))))])
    (lambda () (f xs ys))))

(define (cycle-list-helper xs)
  (append (cdr xs) (list (car xs))))

(define (vector-assoc v vec)
  (cond [(= (vector-length vec) 0) #f]
        [(and (pair? (vector-ref vec 0)) (equal? v (car (vector-ref vec 0)))) (vector-ref vec 0)]
        [#t (vector-assoc v (vector-take-right vec (- (vector-length vec) 1)))]))

(define (cached-assoc xs n)
  (letrec ([memo (make-vector n (cons #f (cons 0 null)))]
           [count 0]
           [f (lambda(v xs)
                (let ([ans (assoc v (vector->list memo))])
                  (if ans
                      (cdr ans)
                      (let ([new-ans (assoc v xs)])
                        (begin
                          (vector-set! memo count new-ans)
                          (if (> count (- n 1))
                              (set! count 0)
                              (set! count (+ count 1)))
                          new-ans
                                )))))])
      (lambda (v) (f v xs))))