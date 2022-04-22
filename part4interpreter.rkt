#lang racket
;Alex Vaslow
;Parker Griffin

(require "classParser.rkt")

;_______The ENTRY POINT TO INTERPRETING THE PROGRAM________
(define interpret2
  (lambda (filename)
    (runmain (addglobal (parser filename) (initialstate) (lambda (s) s) (lambda (s) s) (lambda (s) s) (lambda (s) s) 'throw) (lambda (v) v) (lambda (v) v) 'throw)))
(define interpret
  (lambda (filename classname)
    (executemain (addallclassclosure (parser filename) (initialstate) (lambda (s) s) (lambda (s) s) (lambda (s) s) (lambda (s) s) 'throw) classname)))
    
    ;#FILL THIS IN
(define addallclassclosure
  (lambda (lis state next break continue return throw)
    (cond
      ((null? lis) (next state))
      [(list? (operator lis)) (createclosure (operator lis) state (lambda (s) (addallclassclosure (cdr lis) s next break continue return throw)) break continue return throw)]
      [else (next state)]))
  )

(define executemain
  (lambda (state name)
    (get 'return (mstate (get 'main (getmethodsfromclosure name state)) (initialstate) (lambda (s) s) (lambda (s) s) (lambda (s) s) (lambda (v) v)  'throw))))

(define createclosure
  (lambda (lis state next break continue return throw)
    (next (adddeclare
     (classname lis)
     (list
      ;superclass
      (getsuperclassclosure (superclass lis) state)
      ;methods
      (getmethods (classbody lis) (getsuperclassmethods (superclass lis) state) (lambda (s) s) break continue return throw) ;;NEED TO UPDATE THIS IF THERE'S NO SUPER CLASS (START WITH DIFFERENT STATE)
      (getinstancevariables (classbody lis) (getsuperclassfields (superclass lis) state) (lambda (s) s) break continue return throw) ;;SAME AS ABOVE
      )
     state))))

(define getsuperclassmethods
  (lambda (name state)
    (cond
      [(null? (getsuperclassclosure name state)) (initialstate)]
      [else (getmethodsfromclosure name state)])))
(define getsuperclassfields
  (lambda (name state)
    (cond
      [(null? (getsuperclassclosure name state)) (initialstate)]
      [else (getvariablesfromclosure name state)])))

(define getmethodsfromclosure
  (lambda (name state)
    (cadr (getsuperclassclosure name state))))

(define getvariablesfromclosure
  (lambda (name state)
    (caddr (getsuperclassclosure name state))))

(define getsuperclassclosure
  (lambda (name state)
    (if (eq? '() name)
        '()
        (get name state))))


(define getmethods
  (lambda (lis state next break continue return throw)
    (cond
       [(null? lis) (next state)]
       [(list? (operator lis)) (getmethods (operator lis) state (lambda (s) (getmethods (operatorcdr lis) s next break continue return throw)) break continue return throw)]
       [(eq? (operator lis) 'function) (addfunctionclosure lis state next)]
       [(eq? (operator lis) 'static-function) (addfunctionclosure lis state next)]
       [else (next state)]
       )))

(define getinstancevariables
  (lambda (lis state next break continue return throw)
    (cond
       [(null? lis) (next state)]
       [(list? (operator lis)) (getinstancevariables (operator lis) state (lambda (s) (getinstancevariables (operatorcdr lis) s next break continue return throw)) break continue return throw)]
       [(eq? (operator lis) 'var) (classdeclare lis state next break continue return throw)]
       [else (next state)]
       )))

;---------------------MVALUE AND MBOOLEAN FUNCTIONS----------------------
(define mvalue
  (lambda (lis state next throw)
    (cond
      [(number? lis) (next lis)]
      [(boolean? lis) (next lis)]
      [(eq? lis 'true) (next #t)]
      [(eq? lis #t) (next #t)]
      [(eq? lis #f) (next #f)]
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
      [(eq? (operator lis) 'funcall) (next (get 'return (runfunction lis state (lambda (v) v) (lambda (v) v) throw)))] ; NEW
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
      [(and (eq? (operator lis) 'function)(not (eq? (functionname lis) 'main))) (addfunctionclosure lis state next)] ;new
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

;---Adds all global variables and functions to the state---- Run before calling the main function
(define addglobal
  (lambda (lis state next break continue return throw)
    (cond
       [(null? lis) (next state)]
       [(list? (operator lis)) (addglobal (operator lis) state (lambda (s) (addglobal (operatorcdr lis) s next break continue return throw)) break continue return throw)]
       [(eq? (operator lis) 'function) (addfunctionclosure lis state next)]
       [(eq? (operator lis) 'var) (declare lis state next break continue return throw)]
       [else (next state)]
       )))

;Add Function Closure to State
;need to add formal parameters, the body
;Note, to determine what is in scope we just look at the functionname which we are already stored in closure
;So, closure only has two elements
(define addfunctionclosure
  (lambda (lis state next)
    (cond
      [(next (adddeclare (functionname lis) (list (formalparameters lis) (functionbody lis)) state))]))) ;need to add part 3 of closure

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
     (bindparams (paramsfromclosure (get 'main state)) empty (addstatelayer (getscope 'main state)) state next return throw)
     (lambda (s) (next state))
     (lambda (s) (error 'breakoutsideloop))
     (lambda (s) (error 'continueoutsideloop))
     (lambda (s) (next state))
     (lambda (s e) (throw state e))
     ))))

(define runfunction
  (lambda (lis state next return throw)
    (mstate
     (bodyfromclosure (get (functionname lis) state));body
     (bindparams (paramsfromclosure (get (functionname lis) state)) (paramsfromcall lis) (addstatelayer (getscope (functionname lis) state)) state next return throw)
     (lambda (s) (next state))
     (lambda (s) (error 'breakoutsideloop))
     (lambda (s) (error 'continueoutsideloop))
     (lambda (s) (next state))
     (lambda (s e) (throw state e))
     )))


(define bindparams
  (lambda (formal actual environment state next return throw)
    (cond
      [(and (null? formal) (null? actual)) environment]
      [(or (null? formal) (null? actual)) (error 'mismatchparams)]
      [else (bindparams (remaining formal) (remaining actual) (adddeclare (selected formal) (mvalue (selected actual) state (lambda (v) v) throw) environment) state next return throw)])))

(define remaining
  (lambda (lis)
    (cdr lis)))
(define selected
  (lambda (lis)
    (car lis)))

; gets the value of a variable
;uses boxes
(define get
  (lambda (variable state)
    (cond
      [(and (nonextlayer? state) (emptycurrentlayer? state)) (error 'notassigned)]
      [(emptycurrentlayer? state) (get variable (nextlayers state))]
      [(rightvariable? variable state) (unbox (selectedvalue state))]
      [else (get variable (remainderofstate state))]
    )))

(define declare
  (lambda (lis state next break continue return throw)
    (declarehelper lis state next break continue return throw #f)))

(define classdeclare
  (lambda (lis state next break continue return throw)
    (declarehelper lis state next break continue return throw #t)))
     
; declares a variable. Format: (declare '(var x 5) state)
(define declarehelper
  (lambda (lis state next break continue return throw classdeclare)
    (cond
      [(and (not classdeclare) (isdeclared lis (current state))) (error 'redeclarederror)] 
      [(isnovaluetoassign lis)(next (adddeclare (inputvariable lis) 'declared state))]
      [else (next (adddeclare (inputvariable lis) (mvalue (valuetoassign lis) state (lambda (v) v) throw) state)) ]
     )))
;currentlayer
(define current
  (lambda (state)
    (cons (currentlayer state) (emptylist))))

; assigns a value to a variable
(define assign
  (lambda (lis state next break continue return throw)
    (if (not (isdeclared lis state))
        (error 'notdeclarederror)
        (next (add (inputvariable lis) (mvalue (valuetoassign lis) state (lambda (v) v) throw) state))
    )))

; uses the return continuation to stop code execution
(define returnfunction
  (lambda (lis state next break continue return throw)
    (cond
      [(eq? (mvalue (operatorcdr lis) state (lambda (v) v) throw) #t) (adddeclare 'return 'true state)]
      [(eq? (mvalue (operatorcdr lis) state (lambda (v) v) throw) #f) (adddeclare 'return 'false state)]
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

;----HELPERS FOR CLASSES-------------------
(define classname
  (lambda (lis)
    (cadr lis)))

(define superclass
  (lambda (lis)
    (cond
      ((null? (caddr lis)) '())
      (else (cadr (caddr lis))))))

(define classbody
  (lambda (lis)
    (cadddr lis)))
;End helpers
;--------------------

;__________TESTS_____________
;(interpret "classtest1.txt" "A")

(interpret "simpleclasstest1.txt" 'A)


