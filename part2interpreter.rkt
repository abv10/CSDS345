#lang racket
;Alex Vaslow
;Parker Griffin

(require "functionParser.rkt")

;_______The ENTRY POINT TO INTERPRETING THE PROGRAM________
(define interpret2
  (lambda (filename)
    (get 'return (mstate (parser filename) (initialstate) (lambda (s) s) (lambda (s) s) (lambda (s) s) (lambda (s) s) 'throw))))

(define interpret
  (lambda (filename)
    (runmain (cons (newlayer) (addglobal (parser filename) (initialstate) (lambda (s) s) (lambda (s) s) (lambda (s) s) (lambda (s) s) 'throw)) (lambda (v) v) (lambda (v) v) 'throw)))

;---------------------MVALUE AND MBOOLEAN FUNCTIONS----------------------
(define mvalue
  (lambda (lis state next throw)
    (cond
      [(number? lis) (next lis)]
      [(boolean? lis) (next lis)]
      [(eq? lis 'true) (next #t)]
      [(eq? lis 'false) (next #f)]
      [(and (atom? lis) (eq? (get lis state) 'declared)) (next (error 'notassignederror))]
      [(atom? lis) (next (get lis state))]
      [(and (eq? (operator lis) '-)(null? (firstexpressioncdr lis))) (mvalue (firstexpression lis) state (lambda (v) (next (- v))) throw)]
      [(eq? (operator lis) '*) (mvalue (firstexpression lis) state (lambda (v1) (mvalue (secondexpression lis) state (lambda (v2) (next (* v1 v2))) throw)) throw)]
      [(eq? (operator lis) '+) (mvalue (firstexpression lis) state (lambda (v1) (mvalue (secondexpression lis) state (lambda (v2) (next (+ v1 v2))) throw)) throw)]
      [(eq? (operator lis) '-) (mvalue (firstexpression lis) state (lambda (v1) (mvalue (secondexpression lis) state (lambda (v2) (next (- v1 v2))) throw)) throw)]
      [(eq? (operator lis) '/) (mvalue (firstexpression lis) state (lambda (v1) (mvalue (secondexpression lis) state (lambda (v2) (next (quotient v1 v2))) throw)) throw)]
      [(eq? (operator lis) '%) (mvalue (firstexpression lis) state (lambda (v1) (mvalue (secondexpression lis) state (lambda (v2) (next (modulo v1 v2))) throw)) throw)]
      [(eq? (operator lis) '=) (next (begin (add (firstexpression lis) (mvalue (secondexpression lis) state (lambda (v) v) throw) state) (mvalue (secondexpression lis) state (lambda (v) v) throw)))]
      [(eq? (operator lis) '==) (mboolean lis state next throw)]
      [(eq? (operator lis) '!=) (mboolean lis state next throw)]
      [(eq? (operator lis) '<) (mboolean lis state next throw)]
      [(eq? (operator lis) '>) (mboolean lis state next throw)]
      [(eq? (operator lis) '>=) (mboolean lis state next throw)]
      [(eq? (operator lis) '<=) (mboolean lis state next throw)]
      [(eq? (operator lis) '!) (mboolean lis state next throw)]
      [(eq? (operator lis) '||) (mboolean lis state next throw)]
      [(eq? (operator lis) '&&) (mboolean lis state next throw)]
      [(eq? (operator lis) 'funcall) (next (get 'return (runfunction lis state (lambda (v) v) (lambda (v) v) throw)))] ; NEW
      [(null? (operatorcdr lis)) (mvalue (operator lis) state next throw)]
      )))

; evaluates boolean expressions.
(define mboolean
  (lambda (lis state next throw)
    (cond
      [(number? lis) (next lis)]
      [(eq? lis 'true) (next #t)]
      [(eq? lis 'false) (next #f)]
      [(atom? lis) (next (get lis state))]
      [(eq? (operator lis) '==) (mvalue (firstexpression lis) state (lambda (v1) (mvalue (secondexpression lis) state (lambda (v2) (next (eq? v1 v2))) throw)) throw)]
      [(eq? (operator lis) '!=) (mvalue (firstexpression lis) state (lambda (v1) (mvalue (secondexpression lis) state (lambda (v2) (next (not (eq? v1 v2)))) throw)) throw)]
      [(eq? (operator lis) '<) (mvalue (firstexpression lis) state (lambda (v1) (mvalue (secondexpression lis) state (lambda (v2) (next (< v1 v2))) throw)) throw)]
      [(eq? (operator lis) '>) (mvalue (firstexpression lis) state (lambda (v1) (mvalue (secondexpression lis) state (lambda (v2) (next (> v1 v2))) throw)) throw)]
      [(eq? (operator lis) '>=) (mvalue (firstexpression lis) state (lambda (v1) (mvalue (secondexpression lis) state (lambda (v2) (next (>= v1 v2))) throw)) throw)]
      [(eq? (operator lis) '<=) (mvalue (firstexpression lis) state (lambda (v1) (mvalue (secondexpression lis) state (lambda (v2) (next (<= v1 v2))) throw)) throw)]
      [(eq? (operator lis) '!) (mboolean (firstexpression lis) state (lambda (v) (next (not v))) throw)]
      [(eq? (operator lis) '||) (mboolean (firstexpression lis) state (lambda (v1) (mboolean (secondexpression lis) state (lambda (v2) (next (or v1 v2))) throw)) throw)]
      [(eq? (operator lis) '&&) (mboolean (firstexpression lis) state (lambda (v1) (mboolean (secondexpression lis) state (lambda (v2) (next (and v1 v2))) throw)) throw)]
      )))

; -----------------------------MSTATE FUNCTIONS-----------------------------
; changes the state
(define mstate
  (lambda (lis state next break continue return throw) ; next is the continuation function
    (cond
      [(null? lis) (next state)]
      [(atom? lis) (next state)]
      [(list? (operator lis)) (mstate (operator lis) state (lambda (s) (mstate (operatorcdr lis) s next break continue return throw)) break continue return throw)]
      [(eq? (operator lis) 'funcall) (next (runfunction lis state next return throw))] ; new
      [(eq? (operator lis) 'var) (declare lis state next break continue return throw)]
      [(eq? (operator lis) '=) (assign lis state next break continue return throw)]
      [(eq? (operator lis) 'return) (returnfunction lis state next break continue return throw)]
      [(and (eq? (operator lis) 'function)(not (eq? (functionname lis) 'main))) (addclosure lis state next)] ;new
      [(and (eq? (operator lis) 'function)(eq? (functionname lis) 'main)) (mstate (cadddr lis) (addclosure lis state next) next break continue return throw)] ;new
      [(eq? (operator lis) 'if) (ifstatement lis state next break continue return throw)]
      [(eq? (operator lis) 'while) (whileloop lis state next (lambda (v) (next (nextlayers v))) continue return throw)]
      [(eq? (operator lis) 'break) (break state)]
      [(eq? (operator lis) 'continue) (continue state)]
      [(eq? (operator lis) 'begin) (block lis (addstatelayer state) next break continue return throw)]
      [(and (eq? (operator lis) 'throw) (eq? throw 'throw)) (error 'uncaughtthrow)]
      [(eq? (operator lis) 'throw) (throw state (mvalue (firstexpression lis) state (lambda (v) v) throw))]
      [(eq? (operator lis) 'try) (trycatch lis state next break continue return throw)]
      [(not (null? (operatorcdr lis))) (mstate (operatorcdr lis) state next break continue return throw)] 
      [else (next state)]
    )))

(define addglobal
  (lambda (lis state next break continue return throw)
    (cond
       [(null? lis) (next state)]
       [(list? (operator lis)) (addglobal (operator lis) state (lambda (s) (addglobal (operatorcdr lis) s next break continue return throw)) break continue return throw)]
       [(eq? (operator lis) 'function) (addclosure lis state next)]
       [(eq? (operator lis) 'var) (declare lis state next break continue return throw)]
       [else (next state)]
       )))


       

       


;Add Function Closure to State
;need to add formal parameters, the body and what is in scope
(define addclosure
  (lambda (lis state next)
    (cond
      [(next (adddeclare (functionname lis) (list (formalparameters lis) (functionbody lis) 1) state))]))) ;need to add part 3 of closure

(define getscope
  (lambda (functionname state)
    (cond
     [(rightlayer? functionname (currentlayervariables state)) state]
     [else (getscope functionname (nextlayers state))])))

(define functionname
  (lambda (lis)
    (cadr lis)))

(define formalparameters
  (lambda (lis)
    (caddr lis)))

(define functionbody
  (lambda (lis)
    (cadddr lis)))

(define bodyfromclosure
  (lambda (closure)
    (cadr closure)))

(define paramsfromclosure
  (lambda (closure)
    (car closure)))
(define paramsfromcall
  (lambda (call)
    (cddr call)))

;Add Function Call
(define runmain
  (lambda (state next return throw)
    (get 'return (mstate
     (bodyfromclosure (get 'main state));body
     (cons (newlayer) (bindparams (paramsfromclosure (get 'main state)) empty (cons (newlayer) (getscope 'main state)) state next return throw))
     (lambda (s) (next s))
     (lambda (s) (error 'breakoutsideloop))
     (lambda (s) (error 'continueoutsideloop))
     (lambda (s v) (return s v))
     (lambda (s e) (throw s e))
     ))))

(define runfunction
  (lambda (lis state next return throw)
    (mstate
     (bodyfromclosure (get (functionname lis) state));body
     (bindparams (paramsfromclosure (get (functionname lis) state)) (paramsfromcall lis) (cons (newlayer) (getscope (functionname lis) state)) state next return throw)
     (lambda (s) (next s))
     (lambda (s) (error 'breakoutsideloop))
     (lambda (s) (error 'continueoutsideloop))
     (lambda (s v) (return s v))
     (lambda (s e) (throw s e))
     )))


(define bindparams
  (lambda (formal actual environment state next return throw)
    (cond
      [(and (null? formal) (null? actual)) environment]
      [(or (null? formal) (null? actual)) (error 'mismatchparams)]
      [else (bindparams (cdr formal) (cdr actual) (adddeclare (car formal) (mvalue (car actual) state (lambda (v) v) throw) environment) state next return throw)])))

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
      [(isdeclared lis (cons (currentlayer state) (emptylist))) (error 'redeclarederror)] 
      [(isnovaluetoassign lis)(next (adddeclare (inputvariable lis) 'declared state))]
      [else (next (adddeclare (inputvariable lis) (mvalue (valuetoassign lis) state (lambda (v) v) throw) state)) ]
     )))

; assigns a value to a variable
(define assign
  (lambda (lis state next break continue return throw)
    (if (not (isdeclared lis state))
        (error 'notdeclarederror)
        ;(mstate (valuetoassign lis) state (lambda (v) (add (inputvariable lis) (mvalue (valuetoassign lis) v) v)) break continue return throw)
        (next (add (inputvariable lis) (mvalue (valuetoassign lis) state (lambda (v) v) throw) state))
    )))

; uses the return continuation to stop code execution
(define returnfunction
  (lambda (lis state next break continue return throw)
    (cond
      [(eq? (mvalue (operatorcdr lis) state (lambda (v) v) throw) #t) (add 'return 'true state)]
      [(eq? (mvalue (operatorcdr lis) state (lambda (v) v) throw) #f) (add 'return 'false state)]
      [else (adddeclare 'return (mvalue (operatorcdr lis) state (lambda (v) v) throw) state)]
    )))

; executes an if statement (no side effects)
(define ifstatement
  (lambda (lis state next break continue return throw)
    (cond
      [(mvalue (ifcondition lis) state (lambda (v) v) throw) (mstate (thenstatement lis) state next break continue return throw)]
      [(null? (thenstatementcdr lis)) (next state)]
      [else (mstate (elsestatement lis) state next break continue return throw)]
    )))

; while loop (no side effects)
(define whileloop
  (lambda (lis state next break continue return throw)
    (cond
      ((null? lis) state)
      ((mvalue (whilecondition lis) state (lambda (v) v) throw) (mstate (whileloopbody lis) state (lambda (v) (mstate lis v next break continue return throw)) break continue return throw))
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

(define rightlayer?
  (lambda (variable variablelayer)
    (cond
      [(null? variablelayer) #f]
      [(eq? (car variablelayer) variable) #t]
      [else (rightlayer? variable (cdr variablelayer))])))

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
(interpret "functiontest16.txt")
'ETest21
(eq? (interpret "etest21.txt") 30)
'ETest22
(eq? (interpret "etest22.txt") 11)
'ETest23
(eq? (interpret "etest23.txt") 1106)
'ETest24
(eq? (interpret "etest24.txt") 12)
'ETest25
(eq? (interpret "etest25.txt") 16)
'ETest26
(eq? (interpret "etest26.txt") 72)
'ETest27
(eq? (interpret "etest27.txt") 21)
'ETest28
(eq? (interpret "etest28.txt") 164)
'EFlowTest20
(eq? (interpret "eflowtest20.txt") 21)




'(end etests)
'Test1
(eq? (interpret "test1.txt") 150)
'Test2
(eq? (interpret "test2.txt") -4)
'Test3
(eq? (interpret "test3.txt") 10)
'Test4
(eq? (interpret "test4.txt") 16)
'Test5
(eq? (interpret "test5.txt") 220)
'Test6
(eq? (interpret  "test6.txt") 5)
'Test7
(eq? (interpret  "test7.txt") 6)
'Test8
(eq? (interpret  "test8.txt") 10)
'Test9
(eq? (interpret  "test9.txt") 5)
'Test10
(eq? (interpret  "test10.txt") -39)
;(interpret  "test11.txt")
;(interpret  "test12.txt")
;(interpret  "test13.txt")
;(interpret  "test14.txt")
'Test15
(eq? (interpret  "test15.txt") 'true)
'Test16
(eq? (interpret  "test16.txt") 100);
'Test17
(eq? (interpret  "test17.txt") 'false)
'Test18
(eq? (interpret  "test18.txt") 'true)
'Test19
(eq? (interpret  "test19.txt") 128)
'Test20
(eq? (interpret  "test20.txt") 12)
'Flowtest1
(eq? (interpret "flowtest1.txt") 20)
'Flowtest2
(eq? (interpret "flowtest2.txt") 164)
'Flowtest3
(eq? (interpret "flowtest3.txt") 32)
'Flowtest4
(eq? (interpret "flowtest4.txt") 2)
;(interpret "flowtest5.txt") ;error
'Flowtest6
(eq? (interpret "flowtest6.txt") 25)
'Flowtest7
(eq? (interpret "flowtest7.txt") 21)
'Flowtest8
(eq? (interpret "flowtest8.txt") 6)
'Flowtest9
(eq? (interpret "flowtest9.txt") -1)
'Flowtest10
(eq? (interpret "flowtest10.txt") 789)

;(eq? (interpret "flowtest11.txt") 'error)
;(interpret "flowtest12.txt") error
;(interpret "flowtest13.txt") error
'Flowtest14
(eq? (interpret "flowtest14.txt") 12)
'Flowtest15
(eq? (interpret "flowtest15.txt") 125)
'Flowtest16
(eq? (interpret "flowtest16.txt") 110)
'Flowtest17
(eq? (interpret "flowtest17.txt") 2000400)
'Flowtest18
(eq? (interpret "flowtest18.txt") 101)
'Test1
(eq? (interpret "functiontest1.txt") 10)
'Test2
(eq?(interpret "functiontest2.txt") 14)
'Test3
(eq? (interpret "functiontest3.txt") 45)
'Test4
(eq? (interpret "functiontest4.txt") 55)
'Test5
(eq? (interpret "functiontest5.txt") 1)
'Test6
(eq? (interpret "functiontest6.txt") 115)
'Test7
(eq? (interpret "functiontest7.txt") 'true)
'Test8
(eq? (interpret "functiontest8.txt") 20)
'Test9
(eq? (interpret "functiontest9.txt") 24)
'Test10
(eq? (interpret "functiontest10.txt") 2)
'Test11
(eq? (interpret "functiontest11.txt") 35)
'Test12CorrectlyThrowsError
;(interpret "functiontest12.txt")
'Test13
(eq? (interpret "functiontest13.txt") 90)
'Test14
(eq? (interpret "functiontest14.txt") 69)
'Test15
(interpret "functiontest15.txt")
'Test16
;(interpret "functiontest16.txt")
'Test17
;This correctly throughs a notdeclarederror
;(interpret "functiontest17.txt")
'Test18
(eq? (interpret "functiontest18.txt") 125)
'Test19
(interpret "functiontest19.txt")
'Test20
(interpret "functiontest20.txt")