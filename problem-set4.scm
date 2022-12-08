; Problem 1: Last

(define (last ls)
  (let ((head (car ls))
        (tail (cdr ls)))
    (if (null? tail)
      head
      (last tail))))

; Problem 2: Reverse

(define (reverse ls)
  (define (reverse-iter ls acc)
      (if (null? ls)
          acc
          (let ((head (car ls))
                (tail (cdr ls)))
            (reverse-iter tail (cons head acc)))))
  (reverse-iter ls '()))

; Problem 3: Map with iteration

(define (map proc items)
  (define (map-iter-rev proc items acc)
    (if (null? items)
        acc
        (let ((head (car items))
              (tail (cdr items)))
          (map-iter-rev proc
                        tail
                        (cons (proc head) acc)))))
  (reverse (map-iter-rev proc items '())))


; Problem 4: Flatten

(define nil '())

(define (id x) x)

(define (flatten ls)
  (let* ((head   (car ls))
         (tail   (cdr ls))
         (headop (if (pair? head) flatten list))
         (tailop (if (null? tail) id      flatten)))
    (append (headop head) (tailop tail))))

; Problem 5: tree-map

(define nil '())

(define (tree-map proc tree)
  (cond ((pair? tree) (cons (tree-map proc (car tree))
                            (tree-map proc (cdr tree))))
        ((null? tree) nil)
        (else (proc tree))))

; Problem 6: count-change with denominations as a list 

(define (count-change amount)
  (cc amount (list 1 5 10 25 50)))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0)
             (null? kinds-of-coins))
         0)
        (else
         (+ (cc amount (cdr kinds-of-coins))
            (cc (- amount (first-denomination
                           kinds-of-coins))
                kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (car kinds-of-coins))

; Problem 7: deep-reverse

(define (deep-reverse ls)
  (define (deep-rev-iter ls acc)
    (cond ((null? ls) acc)
          ((pair? (car ls))
           (deep-rev-iter (cdr ls)
                          (cons (deep-rev-iter (car ls) '()) acc)))
          (else (deep-rev-iter (cdr ls) (cons (car ls) acc)))))
  (deep-rev-iter ls '()))

; Problem 8: map, append, length using accumulate

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y))
              nil sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
    (accumulate (lambda (_ y) (+ y 1)) 0 sequence))
