#lang racket

(define mvalue
  (lambda (lis state)
    (cond
      [(number? lis) lis]
      [(boolean? lis) lis]
      [(atom? lis) (get lis state)]
      [(and (eq? (operator lis) '-)(null? (firstexpressioncdr lis))) (- (mvalue (firstexpression lis) state))]
      [(eq? (operator lis) '*) (* (mvalue (firstexpression lis) state) (mvalue (secondexpression lis) state))]
      [(eq? (operator lis) '+) (+ (mvalue (firstexpression lis) state) (mvalue (secondexpression lis) state))]
      [(eq? (operator lis) '-) (- (mvalue (firstexpression lis) state) (mvalue (secondexpression lis) state))]
      [(eq? (operator lis) '/) (quotient (mvalue (firstexpression lis) state) (mvalue (secondexpression lis) state))]
      [(eq? (operator lis) '%) (modulo (mvalue (firstexpression lis) state) (mvalue (secondexpression lis) state))]
      [(eq? (operator lis) '==) (mboolean lis state)]
      [(eq? (operator lis) '!=) (mboolean lis state)]
      [(eq? (operator lis) '<) (mboolean lis state)]
      [(eq? (operator lis) '>) (mboolean lis state)]
      [(eq? (operator lis) '>=) (mboolean lis state)]
      [(eq? (operator lis) '<=) (mboolean lis state)]
      [(eq? (operator lis) '!) (mboolean lis state)]
      [(eq? (operator lis) '||) (mboolean lis state)]
      [(eq? (operator lis) '&&) (mboolean lis state)]
      [(eq? (operator lis) 'return) (mvalue (firstexpression lis) state)] ; not sure if this is the right place for it
      )))

; evaluates boolean expressions. I think it will only be called by mvalue
(define mboolean
  (lambda (lis state)
    (cond
      [(number? lis) lis]
      [(atom? lis) (get lis state)]
      [(eq? (operator lis) '==) (eq? (mvalue (firstexpression lis) state ) (mvalue (secondexpression lis) state))]
      [(eq? (operator lis) '!=) (not (eq? (mvalue (firstexpression lis) state) (mvalue (secondexpression lis) state)))]
      [(eq? (operator lis) '<) (< (mvalue (firstexpression lis) state) (mvalue (secondexpression lis) state))]
      [(eq? (operator lis) '>) (> (mvalue (firstexpression lis) state) (mvalue (secondexpression lis) state))]
      [(eq? (operator lis) '>=) (>= (mvalue (firstexpression lis) state) (mvalue (secondexpression lis) state))]
      [(eq? (operator lis) '<=) (<= (mvalue (firstexpression lis) state) (mvalue (secondexpression lis) state))]
      [(eq? (operator lis) '!) (not (mvalue (firstexpression lis) state))]
      [(eq? (operator lis) '||) (or (mvalue (firstexpression lis) state) (mvalue (secondexpression lis) state))]
      [(eq? (operator lis) '&&) (and (mvalue (firstexpression lis) state)(mvalue (secondexpression lis) state))]
      )))

; adds or changes the value of a variable in the state. Not sure if I should add abstraction for getting the car and cdr of the variables and values lists
(define add
  (lambda (variable value state)
    (cond
      [(null? (variables state)) (cons (cons variable (variables state)) (cons (cons value (values state)) (emptylist)))]
      [(eq? variable (car (variables state))) (cons (variables state) (cons (cons value (cdr (values state))) (emptylist)))]
      [else (cons
             (cons (car (variables state)) (variables (add variable value (statecdr state))))
             (cons (cons (car (values state)) (values (add variable value (statecdr state)))) (emptylist))
             )]
    )))

; gets the value of a variable
(define get
  (lambda (variable state)
    (cond
      [(null? (variables state)) (error 'notdeclarederror)]
      [(eq? variable (car (variables state))) (car (values state))]
      [else (get variable (statecdr state))]
    )))

; changes the state
(define mstate
  (lambda (lis state)
    (cond
      [(null? lis) state]
      [(list? (operator lis)) (mstate (cdr lis) (mstate (operator lis) state))]
      [(eq? (operator lis) 'var) (declare lis state)]
      [(eq? (operator lis) '=) (assign lis state)]
      [(eq? (operator lis) 'return) (return lis state)]
      [(eq? (operator lis) 'if) (ifstatement lis state)]
      [else state]
    )))

; declares a variable. Format: (declare '(var x 5) state)
(define declare
  (lambda (lis state)
    (if (null? (firstexpressioncdr lis)) ; maybe change to a cond and return an error if the variable has already been declared
        (add (firstexpression lis) 'declared state)
        (add (firstexpression lis) (mvalue (secondexpression lis)) state)
     )))

; assigns a value to a variable
(define assign
  (lambda (lis state)
    (add (firstexpression lis) (mvalue (secondexpression lis) state) state)
    ))

; sets a variable called return in the state
(define return
  (lambda (lis state)
    (cond
      [(eq? (mvalue lis state) #t) (add 'return 'true state)]
      [(eq? (mvalue lis state) #f) (add 'return 'false state)]
      [else (add 'return (mvalue lis state) state)]
    )))

; executes an if statement
(define ifstatement
  (lambda (lis state)
    (cond
      [(mvalue (ifcondition lis) state) (mstate (thenstatement lis) (mstate (ifcondition lis) state))]
      [(null? (thenstatementcdr lis)) (mstate (ifcondition lis) state)]
      [else (mstate (elsestatement lis) state)]
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

(define ifcondition
  (lambda (lis)
    (cadr lis)))

(define thenstatement
  (lambda (lis)
    (caddr lis)))

(define elsestatement
  (lambda (lis)
    (cadddr lis)))

(define thenstatementcdr
  (lambda (lis)
    (cdddr lis)))

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

(define statecdr
  (lambda (state)
    (cons (cdr (variables state)) (cons (cdr (values state)) (emptylist)))))

(define emptylist
  (lambda ()
    '()))

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))