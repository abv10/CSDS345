#lang racket
;Alex Vaslow
;Parker Griffin

(require "simpleParser.rkt")

;_______The ENTRY POINT TO INTERPRETING THE PROGRAM________
(define interpret
  (lambda (filename)
    (get 'return (mstate (parser filename) (initialstate) (lambda (s) s) (lambda (s) s) (lambda (s) s) (lambda (s) s) 'throw))))

;---------------------MVALUE AND MBOOLEAN FUNCTIONS----------------------
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
      [(eq? (operator lis) '=) (begin (add (firstexpression lis) (mvalue (secondexpression lis) state) state)(mvalue (secondexpression lis) state))]
      [(eq? (operator lis) '==) (mboolean lis state)]
      [(eq? (operator lis) '!=) (mboolean lis state)]
      [(eq? (operator lis) '<) (mboolean lis state)]
      [(eq? (operator lis) '>) (mboolean lis state)]
      [(eq? (operator lis) '>=) (mboolean lis state)]
      [(eq? (operator lis) '<=) (mboolean lis state)]
      [(eq? (operator lis) '!) (mboolean lis state)]
      [(eq? (operator lis) '||) (mboolean lis state)]
      [(eq? (operator lis) '&&) (mboolean lis state)]
      [(null? (operatorcdr lis)) (mvalue (operator lis) state)]
      )))

; evaluates boolean expressions.
(define mboolean
  (lambda (lis state)
    (cond
      [(number? lis) lis]
      [(eq? lis 'true) #t]
      [(eq? lis 'false) #f]
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

; -----------------------------MSTATE FUNCTIONS-----------------------------
; changes the state
(define mstate
  (lambda (lis state next break continue return throw) ; next is the continuation function
    (cond
      [(null? lis) (next state)]
      [(atom? lis) (next state)]
      [(list? (operator lis)) (mstate (operator lis) state (lambda (s) (mstate (operatorcdr lis) s next break continue return throw)) break continue return throw)]
      [(eq? (operator lis) 'var) (declare lis state next break continue return throw)]
      [(eq? (operator lis) '=) (assign lis state next break continue return throw)]
      [(eq? (operator lis) 'return) (returnfunction lis state next break continue return throw)]
      [(eq? (operator lis) 'if) (ifstatement lis state next break continue return throw)]
      [(eq? (operator lis) 'while) (whileloop lis state next (lambda (v) (next (nextlayers v))) continue return throw)]
      [(eq? (operator lis) 'break) (break state)]
      [(eq? (operator lis) 'continue) (continue state)]
      [(eq? (operator lis) 'begin) (block lis (addstatelayer state) next break continue return throw)]
      [(and (eq? (operator lis) 'throw) (eq? throw 'throw)) (error 'uncaughtthrow)]
      [(eq? (operator lis) 'throw) (throw state (mvalue (firstexpression lis) state))]
      [(eq? (operator lis) 'try) (trycatch lis state next break continue return throw)]
      [(not (null? (operatorcdr lis))) (mstate (operatorcdr lis) state next break continue return throw)] 
      [else (next state)]
    )))

; gets the value of a variable
(define get
  (lambda (variable state)
    (cond
      [(and (nonextlayer? state) (emptycurrentlayer? state)) (error 'notassigned)]
      [(emptycurrentlayer? state) (get variable (nextlayers state))]
      [(rightvariable? variable state) (unbox (selectedvalue state))]
      [else (get variable (remainderofstate state))]
    )))

; declares a variable. Format: (declare '(var x 5) state)
(define declare
  (lambda (lis state next break continue return throw)
    (cond
      [(isdeclared lis state) (error 'redeclarederror)] 
      [(isnovaluetoassign lis)(next (add (inputvariable lis) 'declared state))]
      [else (next (adddeclare (inputvariable lis) (mvalue (valuetoassign lis) state) state)) ]
     )))

; assigns a value to a variable
(define assign
  (lambda (lis state next break continue return throw)
    (if (not (isdeclared lis state))
        (error 'notdeclarederror)
        ;(mstate (valuetoassign lis) state (lambda (v) (add (inputvariable lis) (mvalue (valuetoassign lis) v) v)) break continue return throw)
        (next (add (inputvariable lis) (mvalue (valuetoassign lis) state) state))
    )))

; uses the return continuation to stop code execution
(define returnfunction
  (lambda (lis state next break continue return throw)
    (cond
      [(eq? (mvalue (operatorcdr lis) state) #t) (add 'return 'true state)]
      [(eq? (mvalue (operatorcdr lis) state) #f) (add 'return 'false state)]
      [else (mstate (operatorcdr lis) state (lambda (v) (add 'return (mvalue (operatorcdr lis) v) v)) break continue return throw)]
    )))

; executes an if statement (no side effects)
(define ifstatement
  (lambda (lis state next break continue return throw)
    (cond
      [(mvalue (ifcondition lis) state) (mstate (thenstatement lis) state next break continue return throw)]
      [(null? (thenstatementcdr lis)) (next state)]
      [else (mstate (elsestatement lis) state next break continue return throw)]
    )))

; while loop (no side effects)
(define whileloop
  (lambda (lis state next break continue return throw)
    (cond
      ((null? lis) state)
      ((mboolean (whilecondition lis) state) (mstate (whileloopbody lis) state (lambda (v) (mstate lis v next break continue return throw)) break continue return throw))
      (else (mstate (whilecondition lis) state next break continue return throw)))))

;------TRY CATCH return---------
;Semantics of try/catch/return
;mstate try <tryblock> catch (<type> <var>) <catcblock> return <returnblocl>, state, next, break, throw ...)]
;modify the break, next, and throw continuations to execute the return block first and then execture the contituion
;---newbreak (lambda (s1) (mstate (return ...) s1, break, break, throw)
;create a new throw continuation that runs the catch block followed by the return block if the exception is thrown
;---returncont ;= f(s1) --. Mstate(returnblock, s1, next, break, throw)
;---mythrow ;= f(e, s1) --> Mstate(<catchblock>, AddBinding(,var>, e, s1), fianlly cont, newbreak, newthrow..)
;Execute the try block with the new continuations
;---Mstate(,trybloc>, state, returncont, newbreak, mythrow, ....)


;--------------TRY CATCH--------------------
(define trycatch
  (lambda (lis state next break continue return throw)
      (mstate (trybody lis) state
                       (lambda (s) (mstate (finallybody lis) s next break continue return throw)) ;newnext
                       (lambda (s) (mstate (finallybody lis) s break break continue return throw)) ;newbreak
                       (lambda (s) (mstate (finallybody lis) s continue break 'continue? return throw)) ;newcontinue
                       (lambda (v) (return v state)) ;newreturn 
                       (lambda (s e) (mstate (catch lis) (add (catchvariable lis) e s) ; mythrow
                                              (lambda (s) (mstate (finallybody lis) s next break continue return throw)) ;new next
                                              (lambda (s) (mstate (finallybody lis) s break break continue return throw)) ;newbreak
                                              (lambda (s) (mstate (finallybody lis) s continue break 'continue? return throw)) ;newcontinue
                                              (lambda (v) (return v state)) ;newreturn
                                              (lambda (s1 e1) (mstate (catch lis) s1
                                                                      (lambda (s2) (throw s2 e1))
                                                                      (lambda (s2) (throw s2 e1))
                                                                      (lambda (s2) (throw s2 e1))
                                                                      (lambda (v) (throw s1 e1))
                                                                      throw)))))))

; block functionality
(define block
  (lambda (lis state next break continue return throw)
    (cond
      [(null? lis) (next (nextlayers state))]
      [else (mstate (operator lis) state (lambda (v) (block (operatorcdr lis) v next break continue return throw)) break (lambda (v) (next v)) return throw)]
      )))

;-------------------------HELPER FUNCTIONS--------------------------

;_______WIDELY USED HELPER FUNCTIONS____________
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
(define emptylist
  (lambda ()
    '()))

;-----HELPERS FOR MVALUE/MBOOLEAN/MSTATE----------------
(define operator
  (lambda (lis)
    (car lis)))

(define operatorcdr
  (lambda (lis)
    (cdr lis)))

(define firstexpression
  (lambda (lis)
    (cadr lis)))

(define secondexpression
  (lambda (lis)
    (caddr lis)))

(define firstexpressioncdr
  (lambda (lis)
    (cddr lis)))


;-----------------HELPERS FOR MODIFYING THE LAYERED STATE--------------------
(define add
  (lambda (variable value state)
    (addhelper variable value state #f)))

(define adddeclare
  (lambda (variable value state)
    (addhelper variable (box value) state #t)))

(define addhelper
  (lambda (variable value state declaring)
    (cond
      [(eq? #t declaring) (addnowcurrentlayer variable value state)] ;SINCE WE ALREADY CHECKED TO SEE IF VARIABLE IS DECLARED, WE JUST PUT IT ON THE BEGINING OF THE FIRST LAYER
      [(and (nonextlayer? state) (emptycurrentlayer? state)) (addvariabletoempty variable value state) ]
      [(emptycurrentlayer? state)(cons (currentlayer state) (addhelper variable value (nextlayers state) declaring))]
      [(rightvariable? variable state)(begin (set-box! (selectedvalue state) value) state)]
      ;[(rightvariable? variable state)(changevalue variable value state)]
      [else (addnowcurrentlayer (selectedvariable state) (selectedvalue state) (addhelper variable value (remainderofstate state) declaring))]
    )))

(define selectedvariable
  (lambda (state)
    (car (currentlayervariables state))))

(define selectedvalue
  (lambda (state)
    (car (currentlayervalues state))))
    
(define changevalue
  (lambda (variable value state)
    (cons (cons (currentlayervariables state) (cons (cons (set-box! (car (currentlayervalues state)) value) (cdr (currentlayervalues state))) (emptylist)))(nextlayers state))))

(define rightvariable?
  (lambda (variable state)
    (eq? variable (car (currentlayervariables state)))))

(define nextlayers
  (lambda (state)
    (cdr state)))

(define addvariabletoempty
  (lambda (variable value state)
    (cons (cons (cons variable (currentlayervariables state)) (cons (cons (box value) (currentlayervalues state)) (emptylist)))(cdr state))))

(define nonextlayer?
  (lambda (state)
    (null? (cdr state))))

(define emptycurrentlayer?
  (lambda (state)
    (null? (currentlayervariables state))))

; I have the state as two separate lists. One for variables, one for values. The initial state is then '(()())
(define initialstate
  (lambda ()
    '((()()))))

(define newlayer
  (lambda ()
    '(()())))

(define getnoerror
  (lambda (variable state)
    (cond
      [(and (nonextlayer? state) (emptycurrentlayer? state)) 'notdeclared]
      [(emptycurrentlayer? state) (getnoerror variable (nextlayers state))]
      [(rightvariable? variable state) (unbox (selectedvalue state))]
      [else (getnoerror variable (remainderofstate state))]
    )))

;cons c d (((a)(b))((x y)(x z))) --> (((c d) (d b))((x y)(x z))
(define addnowcurrentlayer
  (lambda (carvar carval rest)
    (cons (cons (cons carvar (currentlayervariables rest)) (cons (cons carval (currentlayervalues rest))(emptylist))) (cdr rest))))

; returns the top layer
(define currentlayer
  (lambda (state)
    (car state)))

; returns the variables in the top layer
(define currentlayervariables
  (lambda (state)
    (car (car state))))

; returns the values in the top layer
(define currentlayervalues
  (lambda (state)
    (car (cdr (car state)))))

(define remainderofstate
  (lambda (state)
    (cons (list (cdr (currentlayervariables state)) (cdr (currentlayervalues state)))(nextlayers state))))

; adds an empty layer to the state
(define addstatelayer
  (lambda (state)
    (cons (newlayer) state)))

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


;----------HELPERS FOR DECLARE/ASSIGN-----------------
(define valuetoassign
  (lambda (lis)
    (caddr lis)))

(define isnovaluetoassign
  (lambda (lis)
    (null? (cddr lis))))

(define isdeclared
  (lambda (lis state)
    (not (eq? (getnoerror (inputvariable lis) state) 'notdeclared))))

(define inputvariable
  (lambda (lis)
    (firstexpression lis)))

;---------------------------HELPERS FOR WHILE----------------

(define whilecondition
  (lambda (lis)
    (cadr lis)))

(define whileloopbody
  (lambda (lis)
    (caddr lis)))

;-------------------------HELPERS FOR TRY-CATCH--------------
(define catchvariable
  (lambda (lis)
    (caar (cdaddr lis))))
                                              
(define trybody
  (lambda (lis)
    (cadr lis)))
(define catch
  (lambda (lis)
    (cond
      ((null? (caddr lis)) (error 'uncaught))
      (else (caddr lis)))))

; returns empty list if there is no finally block
(define finallybody
  (lambda (lis)
    (cond
      ((null? (cadddr lis)) (cadddr lis))
      (else (cadr (cadddr lis))))))

;----HELPERS FOR IF STATEMENTS-------------
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

;End helpers
;--------------------

;__________TESTS_____________
(eq? (interpret "etest21.txt") 30)
(eq? (interpret "etest22.txt") 11)
(eq? (interpret "etest23.txt") 1106)
(eq? (interpret "etest24.txt") 12)
(eq? (interpret "etest25.txt") 16)
(eq? (interpret "etest26.txt") 72)
(eq? (interpret "etest27.txt") 21)
(eq? (interpret "etest28.txt") 164)
(eq? (interpret "eflowtest20.txt") 21)




'(end etests)
(eq? (interpret "test1.txt") 150)
(eq? (interpret "test2.txt") -4)
(eq? (interpret "test3.txt") 10)
(eq? (interpret "test4.txt") 16)
(eq? (interpret "test5.txt") 220)
(eq? (interpret  "test6.txt") 5)
(eq? (interpret  "test7.txt") 6)
(eq? (interpret  "test8.txt") 10)
(eq? (interpret  "test9.txt") 5)
(eq? (interpret  "test10.txt") -39)
;(interpret  "test11.txt")
;(interpret  "test12.txt")
;(interpret  "test13.txt")
;(interpret  "test14.txt")
(eq? (interpret  "test15.txt") 'true)
(eq? (interpret  "test16.txt") 100);
(eq? (interpret  "test17.txt") 'false)
(eq? (interpret  "test18.txt") 'true)
(eq? (interpret  "test19.txt") 128)
(eq? (interpret  "test20.txt") 12)
(eq? (interpret "flowtest1.txt") 20)
(eq? (interpret "flowtest2.txt") 164)
(eq? (interpret "flowtest3.txt") 32)
(eq? (interpret "flowtest4.txt") 2)
;(interpret "flowtest5.txt") ;error

(eq? (interpret "flowtest6.txt") 25)
(eq? (interpret "flowtest7.txt") 21)
(eq? (interpret "flowtest8.txt") 6)
(eq? (interpret "flowtest9.txt") -1)
(eq? (interpret "flowtest10.txt") 789)

;(eq? (interpret "flowtest11.txt") 'error)
;(interpret "flowtest12.txt") error
;(interpret "flowtest13.txt") error
(eq? (interpret "flowtest14.txt") 12)
;(parser "flowtest10.txt")
(eq? (interpret "flowtest15.txt") 125)

(eq? (interpret "flowtest16.txt") 110)
(eq? (interpret "flowtest18.txt") 101)
