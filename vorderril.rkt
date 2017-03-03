 #lang racket

; brute force countdown numbers game solver

; it's not too good at figuring out duplicates and other uninteresting variations on a theme

(define (remove-ref lst item)
  (cond ((zero? item) (drop lst 1))
        ( else
        (append (take lst item)
                (drop lst (+ item 1))))))

; always item2 > item1
(define (remove-refs lst item1 item2)
  (remove-ref (remove-ref lst item1) (- item2 1)))

(define (move2-to-head lst item1 item2)
  (let ((value1 (list-ref lst item1))
        (value2 (list-ref lst item2))
        (tail-values (remove-refs lst item1 item2)))
    (if (> value1 value2)
        (cons value1 (cons value2 tail-values))
        (cons value2 (cons value1 tail-values)))))

(define (head2-combinations lst)
  (let ((len (length lst)))
    (define (combinations-with-item1 item1)
      (let loop ((item2 (+ item1 1)))
        (let ((head2-combination (list (move2-to-head lst item1 item2))))          
          (if (= item2 (- len 1))
              head2-combination
              (append head2-combination (loop (+ item2 1)))))))            
    (let loop ((item1 0))
      (if (= item1 (- len 2))
          (combinations-with-item1 item1)
          (append (combinations-with-item1 item1) (loop (+ item1 1)))))))

(define (same-solution? sol1 sol2)
  (define (op->ordinal op)
    (cond ((eq? op +) 0)
          ((eq? op -) 1)
          ((eq? op *) 2)
          ((eq? op /) 3)
          (else 4)))
  (define (step<? s1 s2)
    (cond ((< (op->ordinal (car s1)) (op->ordinal (car s2))) #t)
          ((eq? (car s1) (car s2))
           (cond ((< (cadr s1) (cadr s2)) #t)
                 ((= (cadr s1) (cadr s2))
                  (< (caddr s1) (caddr s2)))
                 (else #f)))
          (else #f)))
  (define (sort-solution soln)
    (sort (unbox soln) step<?))
  (equal? (sort-solution sol1) (sort-solution sol2)))

(define (solve num-lst target prev-steps)
  (define (first-two-same? lst1 lst2)
    (equal? (take lst1 2) (take lst2 2)))
  (define (apply-operator op operands remaining target prev-steps)
    (let ((operand1 (car operands))
          (operand2 (cadr operands)))
      (if (or (and (eq? op -) (= operand1 operand2))
              (and (eq? op /) (or (< operand2 2) (not (zero? (remainder operand1 operand2)))))
              (and (eq? op *) (= operand2 1)))
          #f 
          (solve (cons (op operand1 operand2)
                       remaining)
                 target
                 (cons (list op operand1 operand2) prev-steps)))))
  (define (apply-operators num-lst)
    (let ((operands (sort (take num-lst 2) >))
          (remaining (drop num-lst 2)))
      (remove* '(#f)
               (list 
                (apply-operator + operands remaining target prev-steps)
                (apply-operator - operands remaining target prev-steps)
                (apply-operator * operands remaining target prev-steps)
                (apply-operator / operands remaining target prev-steps)))))
  (cond ((null? num-lst) #f)
        ((memq target num-lst) (box prev-steps))
        ((null? (cdr num-lst)) #f)
        (else
         (remove-duplicates 
          (flatten (map apply-operators 
                        (remove-duplicates (head2-combinations num-lst) 
                                           first-two-same?)))
          same-solution?))))

(define (display-solutions solutions)
  (define (op->symbol op)
    (cond ((eq? op +) '+)
          ((eq? op -) '-)
          ((eq? op *) '*)
          ((eq? op /) '/)
          (else '?)))
  (define (display-step step)
    (let ((ans (apply (car step) (cdr step))))
      (printf "~a~a~a=~a " (cadr step) (op->symbol (car step)) (caddr step) ans)))
  (define (display-solution number solution)
    (printf "Solution ~a: " number)
    (for-each display-step (reverse (unbox solution)))
    (newline))
  (for-each display-solution (range 1 (+ 1 (length solutions))) solutions))

;(display-solutions (solve '(25 75 100 50 6 8) 986 '()))

(let ((params (map string->number (vector->list (current-command-line-arguments)))))
  (display-solutions (solve (cdr params) (car params) '())))
