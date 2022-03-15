#lang racket
;Alex Vaslow
;Parker Griffin

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
      [(and (eq? (operator lis) '-)(null? (firstexpressioncdr lis))) (- (mvalue (firstexpression lis) (mstate (firstexpression lis) state)))]
      [(eq? (operator lis) '*) (* (mvalue (firstexpression lis) state) (mvalue (secondexpression lis) (mstate (firstexpression lis) state)))]
      [(eq? (operator lis) '+) (+ (mvalue (firstexpression lis) state) (mvalue (secondexpression lis) (mstate (firstexpression lis) state)))]
      [(eq? (operator lis) '-) (- (mvalue (firstexpression lis) state) (mvalue (secondexpression lis) (mstate (firstexpression lis) state)))]
      [(eq? (operator lis) '/) (quotient (mvalue (firstexpression lis) state) (mvalue (secondexpression lis) (mstate (firstexpression lis) state)))]
      [(eq? (operator lis) '%) (modulo (mvalue (firstexpression lis) state) (mvalue (secondexpression lis) (mstate (firstexpression lis) state)))]
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
      

      [(and (null? (cdr state)) (null? (currentlayervariables state))) (cons (cons (cons variable (currentlayervariables state)) (cons (cons value (currentlayervalues state)) (emptylist)))(cdr state))]
      [(null? (currentlayervariables state))(cons (currentlayer state) (add variable value (cdr state)))]; no layer left
      [(eq? variable (car (currentlayervariables state))) (cons (cons (currentlayervariables state) (cons (cons value (cdr (currentlayervalues state))) (emptylist)))(cdr state))]
      [else (cons
             (cons (car (currentlayervariables state)) (currentlayervariables (add variable value (cdrcurrentlayer state))))
             (cons (cons (car (currentlayervalues state)) (currentlayervalues (add variable value (cdrcurrentlayer state)))) (emptylist))
             )]
    )))
;***********************MODIFYING FOR LAYER STATE***********
;********This works for a layered state but could probably use some abstraction***
; gets the value of a variable
(define get
  (lambda (variable state)
    (cond
      [(and (null? (cdr state)) (null? (currentlayervariables state))) (error 'notassigned)]
      [(null? (currentlayervariables state)) (get variable (nextlayers state))]
      [(eq? variable (car (currentlayervariables state))) (car (currentlayervalues state))]
      [else (get variable (cdrcurrentlayer state))]
    ))) 
(define getnoerror
  (lambda (variable state)
    (cond
      [(and (null? (cdr state)) (null? (currentlayervariables state))) ('notassigned)]
      [(null? (variables (currentlayer state))) (getnoerror variable (nextlayers state))]
      [(eq? variable (car (variables (currentlayer state)))) (car (values (currentlayer state)))]
      [else (getnoerror variable (cdrcurrentlayer state))]
    )))

(define currentlayer
  (lambda (state)
    (car state)))

(define currentlayervariables
  (lambda (state)
    (car (car state))))

(define currentlayervalues
  (lambda (state)
    (car (cdr (car state)))))

(define cdrcurrentlayer
  (lambda (state)
    (cons (list (cdr (currentlayervariables state)) (cdr (currentlayervalues state)))(nextlayers state))))

(define nextlayers
  (lambda (state)
    (cdr state)))



; changes the state

(define mstate
  (lambda (lis state) ;;;;Add next, which is a continuation function, ;add break
    (cond
      [(null? lis) state]
      [(atom? lis) state]
      [(list? (operator lis)) (mstate (cdr lis) (mstate (operator lis) state))]
      [(eq? (operator lis) 'var) (declare lis state)]
      [(eq? (operator lis) '=) (assign lis state)]
      [(eq? (operator lis) 'return) (return lis state)]
      [(eq? (operator lis) 'if) (ifstatement lis state)]
      [(eq? (operator lis) 'while) (whileloop lis state)]
      [(eq? (operator lis) 'begin) (block lis state)]
      ;[(eq? (operator lis) 'try do sometihg here)]
      [(equalityoperator? (operator lis)) (mstate (firstexpression lis) (mstate (secondexpression lis) state))]
      [(not (null? (cdr lis))) (mstate (cdr lis) state)] 
      [else state]
    )))
;---Scoping----------
;let: x,y,z are only in scope in body
;let * x is in scope in expression2, expression3, and body
;---name y is in scope in expresion3 and body
;---z is in scope in body, expressions are executed in order
;letrec everything is recursivive 
(define block
  (lambda (lis state)
    state))

(define addstatelayer
  (lambda (state) state))
  
;------TRY CATCH FINALLY---------
;Semantics of try/catch/finally
;mstate try <tryblock> catch (<type> <var>) <catcblock> finally <finallyblocl>, state, next, break, throw ...)]
;modify the break, next, and throw continuations to execute the finally block first and then execture the contituion
;---newbreak (lambda (s1) (mstate (finally ...) s1, break, break, throw)
;create a new throw continuation that runs the catch block followed by the finally block if the exception is thrown
;---finallycont ;= f(s1) --. Mstate(finallyblock, s1, next, break, throw)
;---mythrow ;= f(e, s1) --> Mstate(<catchblock>, AddBinding(,var>, e, s1), fianlly cont, newbreak, newthrow..)
;Execute the try block with the new continuations
;---Mstate(,trybloc>, state, finallycont, newbreak, mythrow, ....)
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
    (if (eq? (getnoerror (firstexpression lis) state) 'notdeclared)
        (error 'notdeclarederror)
        (add (firstexpression lis) (mvalue lis state) (mstate (secondexpression lis) state))
    )))

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

;From Lecture: create helper function called loop, if condition is met, (loop (whileloopbody lis) state (lambda (v) (mstate lis state next)))
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
    '((()()))))

(define newlayer
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
;(interpret "test1.txt")
;(interpret "test2.txt")
;(interpret "test3.txt")
;(interpret "test4.txt")
;(interpret "test5.txt")
;(interpret  "test6.txt")
;(interpret  "test7.txt")
;(interpret  "test8.txt")
;(interpret  "test9.txt")
;(interpret  "test10.txt")
;(interpret  "test11.txt")
;(interpret  "test12.txt")
;(interpret  "test13.txt")
;(interpret  "test14.txt")
;(interpret  "test15.txt")
;(interpret  "test16.txt")
;(interpret  "test17.txt")
;(interpret  "test18.txt")
;(interpret  "test19.txt")
;(interpret  "test20.txt")
;(interpret "etest21.txt")
;(interpret "etest22.txt")
;(interpret "etest23.txt")
;(interpret "etest24.txt")
;(interpret "etest25.txt")
;(interpret "etest26.txt")
;(interpret "etest27.txt")
;(interpret "etest28.txt")
(parser "flowtest1.txt")