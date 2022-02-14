#lang racket
(require "simpleParser.rkt")

(define mvalue
  (lambda (lis state)
    (cond
      [(number? lis) lis]
      [(boolean? lis) lis]
      [(eq? lis 'true) #t]
      [(eq? lis 'false) #f]
      [(and (atom? lis) (eq? (get lis state) 'declared))(error 'notassignederror)]
      [(atom? lis) (get lis state)]
      [(and (eq? (operator lis) '-)(null? (firstexpressioncdr lis))) (- (mvalue (firstexpression lis) state))]
      [(eq? (operator lis) '*) (* (mvalue (firstexpression lis) state) (mvalue (secondexpression lis) state))]
      [(eq? (operator lis) '+) (+ (mvalue (firstexpression lis) state) (mvalue (secondexpression lis) state))]
      [(eq? (operator lis) '-) (- (mvalue (firstexpression lis) state) (mvalue (secondexpression lis) state))]
      [(eq? (operator lis) '/) (quotient (mvalue (firstexpression lis) state) (mvalue (secondexpression lis) state))]
      [(eq? (operator lis) '%) (modulo (mvalue (firstexpression lis) state) (mvalue (secondexpression lis) state))]
      [(eq? (operator lis) '=) (mvalue (secondexpression lis) state)]
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
;#########THIS ERRORS WITH SOMETHING LIKE '(> (= i (+ i 1)) 7) state) SINCE the assignment doesn't return a value right now
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
(define getnoerror
  (lambda (variable state)
    (cond
      [(eq? (variables state) '()) 'notdeclared]
      [(eq? variable (car (variables state))) (car (values state))]
      [else (getnoerror variable (statecdr state))]
    )))

; changes the state
(define mstate
  (lambda (lis state)
    (cond
      [(null? lis) state]
      [(atom? lis) state]
      [(list? (operator lis)) (mstate (cdr lis) (mstate (operator lis) state))]
      [(eq? (operator lis) 'var) (declare lis state)]
      [(eq? (operator lis) '=) (assign lis state)]
      [(eq? (operator lis) 'return) (return lis state)]
      [(eq? (operator lis) 'if) (ifstatement lis state)]
      [(eq? (operator lis) 'while) (whileloop lis state)]
      [(equalityoperator? (operator lis)) (mstate (firstexpression lis) (mstate (secondexpression lis) state))] 
      [else state]
    )))

(define equalityoperator?
  (lambda (operator)
    (cond
      [(eq? '< operator) #t]
      [(eq? '> operator) #t]
      [(eq? '!= operator) #t]
      [(eq? '== operator) #t]
      [(eq? '<= operator) #t]
      [(eq? '>= operator) #t]
      [(eq? '! operator) #t]
      [(eq? '|| operator) #t]
      [(eq? '&& operator) #t]
      [else #f]
      )))

; declares a variable. Format: (declare '(var x 5) state)
(define declare
  (lambda (lis state)
    (cond
      [(not (eq? (getnoerror (firstexpression lis) state) 'notdeclared)) (error 'redeclarederror)] 
      [(null? (firstexpressioncdr lis))(add (firstexpression lis) 'declared state)]
      [else  (add (firstexpression lis) (mvalue (secondexpression lis) (mstate (secondexpression lis) state)) (mstate (secondexpression lis) state))]
     )))

; assigns a value to a variable #COULD ADD NESTED ASSIGNMENTS IF WE WANT
(define assign
  (lambda (lis state)
    (add (firstexpression lis) (mvalue (secondexpression lis) (mstate (secondexpression lis) state)) (mstate (secondexpression lis) state))
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

(define whileloop
  (lambda (lis state)
    (cond
      [(mvalue (whilecondition lis) state) (whileloop lis (mstate (whileloopbody lis) (mstate (whilecondition lis) state)))]
      [else (mstate (whilecondition lis) state)]
    )))

(define whilecondition
  (lambda (lis)
    (cadr lis)))

(define whileloopbody
  (lambda (lis)
    (caddr lis)))

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
;###FOR SOME REASON THIS DOESN'T WORK
(define initialstate
  (lambda ()
    '(()())))
;_______STATE CHANGE RELATED FUNCTIONS, like ADD, REMOVE, GET
(define variables
  (lambda (state)
    (car state)))

(define values
  (lambda (state)
    (cadr state)))

(define statecdr
  (lambda (state)
    (cons (cdr (variables state)) (cons (cdr (values state)) (emptylist)))))

;_______WIDELY USED HELPER FUNCTIONS____________
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
(define emptylist
  (lambda ()
    '()))

;_______The ENTRY POINT TO INTERPRETING THE PROGRAM________
(define interpret
  (lambda (filename)
    (get 'return (mstate (parser filename) (initialstate)))))

;__________TESTS_____________
(interpret "test1.txt")
(interpret "test2.txt")
(interpret "test3.txt")
(interpret "test4.txt")
(interpret "test5.txt")
(interpret  "test6.txt")
(interpret  "test7.txt")
(interpret  "test8.txt")
(interpret  "test9.txt")
(interpret  "test10.txt")
;(interpret  "test11.txt")
;(interpret  "test12.txt")
(interpret  "test13.txt")
(interpret  "test14.txt")
(interpret  "test15.txt")
(interpret  "test16.txt")
(interpret  "test17.txt")
(interpret  "test18.txt")
(interpret  "test19.txt")
(interpret  "test20.txt")

(interpret "etest21.txt")
(interpret "etest22.txt")
(interpret "etest24.txt")
(interpret "etest25.txt")
(interpret "etest26.txt")
;(interpret "etest23.txt")