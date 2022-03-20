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
    (addhelper variable value state #f)))

(define adddeclare
  (lambda (variable value state)
    (addhelper variable value state #t)))

(define addhelper
  (lambda (variable value state declaring)
    (cond
      [(eq? #t declaring) (addnowcurrentlayer variable value state)] ;SINCE WE ALREADY CHECKED TO SEE IF VARIABLE IS DECLARED, WE JUST PUT IT ON THE BEGINING OF THE FIRST LAYER
      [(and (nonextlayer? state) (emptycurrentlayer? state)) (addvariabletoempty variable value state) ]
      [(emptycurrentlayer? state)(cons (currentlayer state) (addhelper variable value (nextlayers state) declaring))]
      [(rightvariable? variable state)(changevalue variable value state)]
      [else (addnowcurrentlayer (selectedvariable state) (selectedvalue state) (addhelper variable value (remainderofstate state) declaring))]
    )))




;----------
;Helpers:
(define selectedvariable
  (lambda (state)
    (car (currentlayervariables state))))

(define selectedvalue
  (lambda (state)
    (car (currentlayervalues state))))
    
(define changevalue
  (lambda (variable value state)
    (cons (cons (currentlayervariables state) (cons (cons value (cdr (currentlayervalues state))) (emptylist)))(nextlayers state))))
(define rightvariable?
  (lambda (variable state)
    (eq? variable (car (currentlayervariables state)))))
(define nextlayers
  (lambda (state)
    (cdr state)))
(define addvariabletoempty
  (lambda (variable value state)
    (cons (cons (cons variable (currentlayervariables state)) (cons (cons value (currentlayervalues state)) (emptylist)))(cdr state))))
(define nonextlayer?
  (lambda (state)
    (null? (cdr state))))

(define emptycurrentlayer?
  (lambda (state)
    (null? (currentlayervariables state))))
;End helpers
;--------------------






;***********************MODIFYING FOR LAYER STATE***********
;********This works for a layered state but could probably use some abstraction***
; gets the value of a variable
(define get
  (lambda (variable state)
    (cond
      [(and (nonextlayer? state) (emptycurrentlayer? state)) (error 'notassigned)]
      [(emptycurrentlayer? state) (get variable (nextlayers state))]
      [(rightvariable? variable state) (selectedvalue state)]
      [else (get variable (remainderofstate state))]
    )))

(define getnoerror
  (lambda (variable state)
    (cond
      [(and (nonextlayer? state) (emptycurrentlayer? state)) 'notdeclared]
      [(emptycurrentlayer? state) (getnoerror variable (nextlayers state))]
      [(rightvariable? variable state) (selectedvalue state)]
      [else (getnoerror variable (remainderofstate state))]
    )))

;cons c d (((a)(b))((x y)(x z))) --> (((c d) (d b))((x y)(x z))
(define addnowcurrentlayer
  (lambda (carvar carval rest)
    (cons (cons (cons carvar (currentlayervariables rest)) (cons (cons carval (currentlayervalues rest))(emptylist))) (cdr rest))))
(define currentlayer
  (lambda (state)
    (car state)))

(define currentlayervariables
  (lambda (state)
    (car (car state))))

(define currentlayervalues
  (lambda (state)
    (car (cdr (car state)))))

(define remainderofstate
  (lambda (state)
    (cons (list (cdr (currentlayervariables state)) (cdr (currentlayervalues state)))(nextlayers state))))



; changes the state

(define mstate
  (lambda (lis state next break continue return throw) ;;;;Add next, which is a continuation function, ;add break
    (cond
      [(null? lis) (next state)]
      [(atom? lis) (next state)]
      [(list? (operator lis)) (mstate (cdr lis) (mstate (operator lis) state next break continue return throw) next break continue return throw)] ;NEED TO CHANGE THIS
      ;[(list? (operator lis)) (mstate (operator lis) state (lambda (s) (mstate (cdr lis) s next break continue return throw)) break continue return throw)]
      [(eq? (operator lis) 'var) (declare lis state next break continue return throw)]
      [(eq? (operator lis) '=) (assign lis state next break continue return throw)]
      [(eq? (operator lis) 'return) (return lis state)]
      [(eq? (operator lis) 'if) (ifstatement lis state next break continue return throw)]
      [(eq? (operator lis) 'while) (whileloop lis state next break continue return throw)]
      [(eq? (operator lis) 'begin) (block lis state next break continue return throw)]
      [(eq? (operator lis) 'throw) (throw state (firstexpression lis))]
      [(eq? (operator lis) 'try) (trycatch lis state next break continue return throw)]
      ;[(equalityoperator? (operator lis)) (mstate (firstexpression lis) (mstate (secondexpression lis) state))] Don't think we need this since we can't assign in expression
      [(not (null? (cdr lis))) (mstate (cdr lis) state next break continue return throw)] 
      [else state]
    )))
;---Scoping----------
;let: x,y,z are only in scope in body
;let * x is in scope in expression2, expression3, and body
;---name y is in scope in expresion3 and body
;---z is in scope in body, expressions are executed in order
;letrec everything is recursivive 
(define block
  (lambda (lis state next break continue return throw)
    state))

(define addstatelayer
  (lambda (state) state))
  
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
  (lambda (lis state next break continue return throw)
    (cond
      [(isdeclared lis state) (error 'redeclarederror)] 
      [(isnovaluetoassign lis)(add (inputvariable lis) 'declared state)]
      [else  (adddeclare (inputvariable lis) (mvalue (valuetoassign lis) state) state) ]
     )))

(define assign
  (lambda (lis state next break continue return throw)
    (if (not (isdeclared lis state))
        (error 'notdeclarederror)
        (add (inputvariable lis) (mvalue (valuetoassign lis) state) state)
    )))

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
  (lambda (lis state next break continue return throw)
    (cond
      [(mvalue (ifcondition lis) state) (mstate (thenstatement lis) state next break continue return throw)]
      [(null? (thenstatementcdr lis)) state]
      [else (mstate (elsestatement lis) state next break continue return throw)]
    )))

;From Lecture: create helper function called loop, if condition is met, (loop (whileloopbody lis) state (lambda (v) (mstate lis state next)))
(define whilelooptwo ;TEMPORARILTY RENAMED
  (lambda (lis state next break continue return throw)
    (cond
      ((null? lis) state)
      ((mboolean (firstexpression lis) state) (mstate (firstexpression lis) state (lambda (v1)
                                                                             (mstate (secondexpression lis) v1 (lambda (v2)(mstate lis v2 next break continue return throw)) break continue return throw)) break continue return throw))
      (else (mstate (firstexpression lis) state next break continue return throw)))))

(define whileloop
  (lambda (lis state next break continue return throw)
    (loop (whilecondition lis) (whileloopbody lis) state next (lambda (s) (next s)) continue return throw)))

(define loop 
  (lambda (condition body state next break continue return throw)
    (cond
      ((mboolean condition state) (mstate body state (lambda (s) (next (loop condition body s next break continue return throw))) break continue return throw))
      (else (next state)))))

;---------------------------HELPERS FOR WHILE----------------

(define whilecondition
  (lambda (lis)
    (cadr lis)))

(define whileloopbody
  (lambda (lis)
    (caddr lis)))


;--------------ATTEMPT FOR TRY CATCH--------------------
;I tried to model this off the notes from February 25, but not sure how accurate it iscd cdes
(define trycatch
  (lambda (lis state next break continue return throw)
      (mstate (trybody lis) state
                       (lambda (s) (mstate (finallybody lis) s next break continue return throw)) ;newnext
                       (lambda (s) (mstate (finallybody lis) s break break continue return throw));newbreak
                       (lambda (s) (mstate (finallybody lis) s continue break 'continue? return throw));newcontinue
                       (lambda (v) (return v state));newreturn ??NOT SURE ABOUT THIS ONE
                       (lambda (s e) (mstate (catch lis) (add 'e e state) ; mythrow
                                              (lambda (s) (mstate (finallybody lis) s next break continue return throw)) ;new next
                                              (lambda (s) (mstate (finallybody lis) s break break continue return throw));newbreak
                                              (lambda (s) (mstate (finallybody lis) s continue break 'continue? return throw));newcontinue
                                              (lambda (v) (return v state));newreturn ??NOT SURE ABOUT THIS ONE
                                              (lambda (s1 e1) (mstate (finallybody lis) s1
                                                                      (lambda (s2) (throw s2 e1))
                                                                      (lambda (s2) (throw s2 e1))
                                                                      (lambda (s2) (throw s2 e1))
                                                                      (lambda (v) (throw s1 e1))
                                                                      throw)))))))



                                              
(define trybody
  (lambda (lis)
    (cadr lis)))
(define catch
  (lambda (lis)
    (caddr lis)))
(define finallybody
  (lambda (lis)
    (cadr (cadddr lis))))



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
    (get 'return (mstate (parser filename) (initialstate) (lambda (s) s) 'break 'continue (lambda (l s) (return l s)) (lambda (l s) (return l s))))))

;__________TESTS_____________
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

(interpret  "test19.txt")
(interpret  "test20.txt")
;(interpret "etest21.txt")
;(interpret "etest22.txt")
;(interpret "etest23.txt")
;(interpret "etest24.txt")
;(interpret "etest25.txt")
;(interpret "etest26.txt")
;(interpret "etest27.txt")
;(interpret "etest28.txt")
;(parser "flowtest1.txt")
(parser "flowtest16.txt")
;(interpret "flowtest1.txt")
;(interpret "flowtest2.txt")
;(interpret "flowtest3.txt")
;(interpret "flowtest4.txt")
;(interpret "flowtest5.txt")

;(interpret "flowtest6.txt")
;(interpret "flowtest7.txt")
;(interpret "flowtest8.txt")
;(interpret "flowtest9.txt")
;(interpret "flowtest10.txt")

;(interpret "flowtest11.txt")
;(interpret "flowtest12.txt")
;(interpret "flowtest13.txt")
;(interpret "flowtest14.txt")
(parser "flowtest15.txt")
;(interpret "flowtest15.txt")

(interpret "flowtest16.txt")
;(interpret "flowtest17.txt")
;(interpret "flowtest18.txt")
;(interpret "flowtest19.txt")