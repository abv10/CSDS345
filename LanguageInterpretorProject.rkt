#lang racket

(define mvalue
  (lambda (lis)
    (cond
      [(number? lis) lis]
      [(boolean? lis) lis]
      [(and (eq? (operator lis) '-)(null? (firstexpressioncdr lis))) (- (mvalue (firstexpression lis)))]
      [(eq? (operator lis) '*) (* (mvalue (firstexpression lis)) (mvalue (secondexpression lis)))]
      [(eq? (operator lis) '+) (+ (mvalue (firstexpression lis)) (mvalue (secondexpression lis)))]
      [(eq? (operator lis) '-) (- (mvalue (firstexpression lis)) (mvalue (secondexpression lis)))]
      [(eq? (operator lis) '/) (quotient (mvalue (firstexpression lis)) (mvalue (secondexpression lis)))]
      [(eq? (operator lis) '%) (modulo (mvalue (firstexpression lis)) (mvalue (secondexpression lis)))]
      [(eq? (operator lis) '==) (mboolean lis)]
      [(eq? (operator lis) '!=) (mboolean lis)]
      [(eq? (operator lis) '<) (mboolean lis)]
      [(eq? (operator lis) '>) (mboolean lis)]
      [(eq? (operator lis) '>=) (mboolean lis)]
      [(eq? (operator lis) '<=) (mboolean lis)]
      [(eq? (operator lis) '!) (mboolean lis)]
      [(eq? (operator lis) '||) (mboolean lis)]
      [(eq? (operator lis) '&&) (mboolean lis)]
      )))

; evaluates boolean expressions. I think it will only be called by mvalue
(define mboolean
  (lambda (lis)
    (cond
      [(number? lis) lis]
      [(eq? (operator lis) '==) (eq? (mvalue (firstexpression lis))(mvalue (secondexpression lis)))]
      [(eq? (operator lis) '!=) (not (eq? (mvalue (firstexpression lis))(mvalue (secondexpression lis))))]
      [(eq? (operator lis) '<) (< (mvalue (firstexpression lis)) (mvalue (secondexpression lis)))]
      [(eq? (operator lis) '>) (> (mvalue (firstexpression lis)) (mvalue (secondexpression lis)))]
      [(eq? (operator lis) '>=) (>= (mvalue (firstexpression lis)) (mvalue (secondexpression lis)))]
      [(eq? (operator lis) '<=) (<= (mvalue (firstexpression lis)) (mvalue (secondexpression lis)))]
      [(eq? (operator lis) '!) (not (mvalue (firstexpression lis)))]
      [(eq? (operator lis) '||) (or (mvalue (firstexpression lis)) (mvalue (secondexpression lis)))]
      [(eq? (operator lis) '&&) (and (mvalue (firstexpression lis))(mvalue (secondexpression lis)))]
      )))

; adds or changes the value of a variable in the state. Not sure if I should add abstraction for getting the car and cdr of the variables and values lists
(define add
  (lambda (variable value state)
    (cond
      [(null? (variables state)) (cons (cons variable (variables state)) (cons (cons value (values state)) (emptylist)))]
      [(eq? variable (car (variables state))) (cons (variables state) (cons (cons value (cdr (values state))) (emptylist)))]
      [else (cons
             (cons (car (variables state)) (variables (add variable value (cons (cdr (variables state)) (cons (cdr (values state)) (emptylist))))))
             (cons (cons (car (values state)) (values (add variable value (cons (cdr (variables state)) (cons (cdr (values state)) (emptylist)))))) (emptylist))
             )]
      )))

(define operator
  (lambda (lis)
    (car lis)))

(define firstexpression
  (lambda (lis)
    (cadr lis)))

(define secondexpression
  (lambda (lis)
    (caddr lis)))

; theres probably a better name for this
(define firstexpressioncdr
  (lambda (lis)
    (cddr lis)))

; I have the state as two separate lists. One for variables, one for values. The initial state is then '(()())
(define initialstate
  (lambda ()
    '(()())))

(define variables
  (lambda (state)
    (car state)))

(define values
  (lambda (state)
    (cadr state)))

(define emptylist
  (lambda ()
    '()))