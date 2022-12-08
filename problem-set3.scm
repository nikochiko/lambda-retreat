; Problem 1 - repeat function

(define (repeat f n args)
  (if (= n 0)
      args
      (repeat f (- n 1) (f . args))))

; Problem 2 - iterative-process function

(define (last ls)
  (if (null? (cdr ls))
      (car ls)
      (last (cdr ls))))

(define (iterative-process n args next)
  (last (repeat next
                n
                args)))

; Problem 3 - Rewrite procedure f using iterative-process

(define (f n)
  (iterative-process n
                     (list 2 1 0)
                     (lambda (a b c) (list (+ a (* 2 b) (* 3 c))
                                           a
                                           b))))

; Problem 4 - Rewrite expt using iterative-process

(define (expt b n)
    (iterative-process n
                       (list b 1)
                       (lambda (b p) (list b (* b p)))))

; Problem 5 - Generalize iterative process into make-iterative-process

(define (make-iterative-process init next)
  (lambda (n)
    (iterative-process n
                       init
                       next)))
