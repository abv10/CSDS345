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
    (executemain (addallclassclosure (parser filename) (initialstate) (lambda (s) s) (lambda (s) s) (lambda (s) s) (lambda (s) s) 'throw) (string->symbol classname))))

; 
(define executedot
  (lambda (lis state next throw)
    (cond
      [(list? (firstexpression lis))
       (next (getvaluefromindex
              (getinstancefieldvalues (createinstance (firstexpression lis) state next throw))
              (getvariableindex (secondexpression lis) (getinstancefieldnames (cadr (firstexpression lis)) state) (- (length (getinstancefieldnames (cadr (firstexpression lis)) state)) 1))))]
      [else
       (next (getvaluefromindex
              (getinstancefieldvalues (get (firstexpression lis) state)) ; values of instance fields (ordered oldest --> newest
              (getvariableindex (secondexpression lis) (getinstancefieldnames (classofinstance (get (firstexpression lis) state)) state) (- (length (getinstancefieldnames (classofinstance (get (firstexpression lis) state)) state)) 1)) ; index of variable
              ))]
    )))


(define assigndot
  (lambda (lis state next break continue return throw classname)
    (next (begin (replaceatindex
     (getvariableindex (secondexpression (firstexpression lis)) (getinstancefieldnames (classofinstance (get (cadr (firstexpression lis)) state)) state) (- (length (getinstancefieldnames (classofinstance (get (cadr (firstexpression lis)) state)) state)) 1)) ; index of variable
     (mvalue (secondexpression lis) state (lambda (v) v) throw classname)
     (getinstancefieldvalues (get (cadr (firstexpression lis)) state))) state) ; values of instance fields (ordered oldest --> newest
     )))

(define replaceatindex
  (lambda (index newvalue values)
    (cond
      [(null? values) (error 'index)]
      [(zero? index) (begin (set-box! (car values) newvalue) values)]
      [else (replaceatindex (- index 1) newvalue (cdr values))])))
           
    
; gets the index of a variable. used to match variables between class and instance closures
; (getvariableindex 'x '(a b c d x e f) 0) => 2
(define getvariableindex
  (lambda (name variables index)
    (cond
      [(null? variables) 'notdeclared]
      [(eq? name (car variables)) index]
      [else (getvariableindex name (cdr variables) (- index 1))]
   )))

; gets value from index of variable. Values will all be in boxes for the input
(define getvaluefromindex
  (lambda (values index)
    (cond
      [(null? values) (error 'incorrectindex)]
      [(zero? index) (unbox (car values))]
      [else (getvaluefromindex (cdr values) (- index 1))]
      )))

    ;#FILL THIS IN
(define addallclassclosure
  (lambda (lis state next break continue return throw)
    (cond
      ((null? lis) (next state))
      [(list? (operator lis)) (createclosure (operator lis) state (lambda (s) (addallclassclosure (cdr lis) s next break continue return throw)) break continue return throw classname)]
      [else (next state)]))
  )

(define executemain
  (lambda (state name)
    (mstate (get 'main (getmethodsfromclosure name state)) (addstatelayer state) (lambda (s) s) (lambda (s) s) (lambda (s) s) (lambda (v) v) 'throw name)))

(define createclosure
  (lambda (lis state next break continue return throw classname)
    (next (adddeclare
     (classname lis)
     (list
      ;superclass
      (getsuperclassclosure (superclass lis) state)
      ;methods
      (getmethods (classbody lis) (getsuperclassmethods (superclass lis) state) (lambda (s) s) break continue return throw (classname lis)) ;;NEED TO UPDATE THIS IF THERE'S NO SUPER CLASS (START WITH DIFFERENT STATE)
      (getinstancevariables (classbody lis) (getsuperclassfields (superclass lis) state) (lambda (s) s) break continue return throw classname)
      (classname lis)
      )
     state))))

;GET THE CLASS NAME AND FIELD VARIABLES VALUES
(define createinstance
  (lambda (lis state next throw)
    (next (list (firstexpression lis) (reverse (rebox (cadar (getvariablesfromclosure (firstexpression lis) state))))))))

; takes a list of boxed values and puts them in a different box
(define rebox
  (lambda (lis)
    (cond
      [(null? lis) lis]
      [else (cons (box (unbox (car lis))) (rebox (cdr lis)))]
   )))
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

(define getsuperfromclosure
  (lambda (name state)
    (car (getsuperclassclosure name state))))

(define getvariablesfromclosure
  (lambda (name state)
    (caddr (getsuperclassclosure name state))))

(define getsuperclassclosure
  (lambda (name state)
    (if (eq? '() name)
        '()
        (get name state))))

; returns instance field names of class
(define getinstancefieldnames
  (lambda (name state)
    (caar (getvariablesfromclosure name state))))

(define getmethods
  (lambda (lis state next break continue return throw classname)
    (cond
       [(null? lis) (next state)]
       [(list? (operator lis)) (getmethods (operator lis) state (lambda (s) (getmethods (operatorcdr lis) s next break continue return throw classname)) break continue return throw classname)]
       [(eq? (operator lis) 'function) (addfunctionclosure lis state next classname)]
       [(eq? (operator lis) 'static-function) (addfunctionclosure lis state next classname)]
       [else (next state)]
       )))

(define getinstancevariables
  (lambda (lis state next break continue return throw classname)
    (cond
       [(null? lis) (next state)]
       [(list? (operator lis)) (getinstancevariables (operator lis) state (lambda (s) (getinstancevariables (operatorcdr lis) s next break continue return throw classname)) break continue return throw classname)]
       [(eq? (operator lis) 'var) (classdeclare lis state next break continue return throw classname)]
       [else (next state)]
       )))

;---------------------MVALUE AND MBOOLEAN FUNCTIONS----------------------
(define mvalue
  (lambda (lis state next throw classname)
    (cond
      [(number? lis) (next lis)]
      [(boolean? lis) (next lis)]
      [(eq? lis 'true) (next #t)]
      [(eq? lis #t) (next #t)]
      [(eq? lis #f) (next #f)]
      [(eq? lis 'false) (next #f)]
      [(atom? lis) (next (getlocaltheninstance lis state (lambda () state)))]
      [(and (atom? lis) (eq? (get lis state) 'declared)) (next (error 'notassignederror))]
      [(atom? lis) (next (get lis state))]
      [(eq? (operator lis) 'new) (createinstance lis state next throw)]
      [(eq? (operator lis) 'dot) (executedot lis state next throw)]
      [(and (eq? (operator lis) '-)(null? (firstexpressioncdr lis))) (mvalue (firstexpression lis) state (lambda (v) (next (- v))) throw classname)]
      [(eq? (operator lis) '*) (mvalue (firstexpression lis) state (lambda (v1) (mvalue (secondexpression lis) state (lambda (v2) (next (* v1 v2))) throw classname)) throw classname)]
      [(eq? (operator lis) '+) (mvalue (firstexpression lis) state (lambda (v1) (mvalue (secondexpression lis) state (lambda (v2) (next (+ v1 v2))) throw classname)) throw classname)]
      [(eq? (operator lis) '-) (mvalue (firstexpression lis) state (lambda (v1) (mvalue (secondexpression lis) state (lambda (v2) (next (- v1 v2))) throw classname)) throw classname)]
      [(eq? (operator lis) '/) (mvalue (firstexpression lis) state (lambda (v1) (mvalue (secondexpression lis) state (lambda (v2) (next (quotient v1 v2))) throw classname)) throw classname)]
      [(eq? (operator lis) '%) (mvalue (firstexpression lis) state (lambda (v1) (mvalue (secondexpression lis) state (lambda (v2) (next (modulo v1 v2))) throw classname)) throw classname)]
      [(eq? (operator lis) '=) (next (begin (add (firstexpression lis) (mvalue (secondexpression lis) state (lambda (v) v) throw classname) state) (mvalue (secondexpression lis) state (lambda (v) v) throw classname)))]
      [(eq? (operator lis) '==) (mboolean lis state next throw classname)]
      [(eq? (operator lis) '!=) (mboolean lis state next throw classname)]
      [(eq? (operator lis) '<) (mboolean lis state next throw classname)]
      [(eq? (operator lis) '>) (mboolean lis state next throw classname)]
      [(eq? (operator lis) '>=) (mboolean lis state next throw classname)]
      [(eq? (operator lis) '<=) (mboolean lis state next throw classname)]
      [(eq? (operator lis) '!) (mboolean lis state next throw classname)]
      [(eq? (operator lis) '||) (mboolean lis state next throw classname)]
      [(eq? (operator lis) '&&) (mboolean lis state next throw classname)]
      [(eq? (operator lis) 'funcall) (next (runfunctionexpression lis state (lambda (v) v) (lambda (v) v) throw classname))] ; NEW
      [(null? (operatorcdr lis)) (mvalue (operator lis) state next throw classname)]
      )))

; evaluates boolean expressions.
(define mboolean
  (lambda (lis state next throw classname)
    (cond
      [(number? lis) (next lis)]
      [(eq? lis 'true) (next #t)]
      [(eq? lis 'false) (next #f)]
      [(atom? lis) (next (get lis state))]
      [(eq? (operator lis) 'funcall) (next (runfunctionexpression lis state (lambda (v) v) (lambda (v) v) throw classname))] ; NEW
      [(eq? (operator lis) '==) (mvalue (firstexpression lis) state (lambda (v1) (mvalue (secondexpression lis) state (lambda (v2) (next (eq? v1 v2))) throw classname)) throw classname)]
      [(eq? (operator lis) '!=) (mvalue (firstexpression lis) state (lambda (v1) (mvalue (secondexpression lis) state (lambda (v2) (next (not (eq? v1 v2)))) throw classname)) throw classname)]
      [(eq? (operator lis) '<) (mvalue (firstexpression lis) state (lambda (v1) (mvalue (secondexpression lis) state (lambda (v2) (next (< v1 v2))) throw classname)) throw classname)]
      [(eq? (operator lis) '>) (mvalue (firstexpression lis) state (lambda (v1) (mvalue (secondexpression lis) state (lambda (v2) (next (> v1 v2))) throw classname)) throw classname)]
      [(eq? (operator lis) '>=) (mvalue (firstexpression lis) state (lambda (v1) (mvalue (secondexpression lis) state (lambda (v2) (next (>= v1 v2))) throw classname)) throw classname)]
      [(eq? (operator lis) '<=) (mvalue (firstexpression lis) state (lambda (v1) (mvalue (secondexpression lis) state (lambda (v2) (next (<= v1 v2))) throw classname)) throw classname)]
      [(eq? (operator lis) '!) (mboolean (firstexpression lis) state (lambda (v) (next (not v))) throw classname)]
      [(eq? (operator lis) '||) (mboolean (firstexpression lis) state (lambda (v1) (mboolean (secondexpression lis) state (lambda (v2) (next (or v1 v2))) throw classname)) throw classname)]
      [(eq? (operator lis) '&&) (mboolean (firstexpression lis) state (lambda (v1) (mboolean (secondexpression lis) state (lambda (v2) (next (and v1 v2))) throw classname)) throw classname)]
      )))

; -----------------------------MSTATE FUNCTIONS-----------------------------
; changes the state
(define mstate
  (lambda (lis state next break continue return throw classname) ; next is the continuation function
    (cond
      [(null? lis) (next state)]
      [(atom? lis) (next state)]
      [(list? (operator lis)) (mstate (operator lis) state (lambda (s) (mstate (operatorcdr lis) s next break continue return throw classname)) break continue return throw classname)]
      [(eq? (operator lis) 'funcall) (runfunction lis state next return throw classname)] ; new
      [(eq? (operator lis) 'var) (declare lis state next break continue return throw classname)]
      [(and (eq? (operator lis) '=) (not (isdeclared (list '= 'this 'test) state))) (assign lis state next break continue return throw classname)] ; checks if we are inside an instance
      [(eq? (operator lis) '=) (assignlocaltheninstance lis state next break continue return throw classname)]
      [(eq? (operator lis) 'return) (returnfunction lis state next break continue return throw classname)]
      [(and (eq? (operator lis) 'function)(not (eq? (functionname lis) 'main))) (addfunctionclosure lis state next)] ;new
      [(eq? (operator lis) 'if) (ifstatement lis state next break continue return throw classname)]
      [(eq? (operator lis) 'while) (whileloop lis state next (lambda (v) (next (nextlayers v))) continue return throw classname)]
      [(eq? (operator lis) 'break) (break state)]
      [(eq? (operator lis) 'continue) (continue state)]
      [(eq? (operator lis) 'begin) (block lis (addstatelayer state) next break continue return throw classname)]
      [(and (eq? (operator lis) 'throw) (eq? throw 'throw)) (error 'uncaughtthrow)]
      [(eq? (operator lis) 'throw) (throw state (mvalue (firstexpression lis) state (lambda (v) v) throw classname))]
      [(eq? (operator lis) 'try) (trycatch lis state next break continue return throw classname)]
      [(not (null? (operatorcdr lis))) (mstate (operatorcdr lis) state next break continue return throw classname)] 
      [else (next state)]
    )))

;---Adds all global variables and functions to the state---- Run before calling the main function
(define addglobal
  (lambda (lis state next break continue return throw classname)
    (cond
       [(null? lis) (next state)]
       [(list? (operator lis)) (addglobal (operator lis) state (lambda (s) (addglobal (operatorcdr lis) s next break continue return throw classname)) break continue return throw classname)]
       [(eq? (operator lis) 'function) (addfunctionclosure lis state next)]
       [(eq? (operator lis) 'var) (declare lis state next break continue return throw classname)]
       [else (next state)]
       )))

;Add Function Closure to State
;need to add formal parameters, the body
;Note, to determine what is in scope we just look at the functionname which we are already stored in closure
;So, closure only has two elements
(define addfunctionclosure
  (lambda (lis state next classname)
    (cond
      [(next (adddeclare (functionname lis) (list (formalparameters lis) (functionbody lis) classname) state))]))) ;need to add part 3 of closure

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
    (mstate
     (bodyfromclosure (get 'main state));body
     (bindparams (paramsfromclosure (get 'main state)) empty (addstatelayer (getscope 'main state)) state next return throw)
     (lambda (s) next s)
     (lambda (s) (error 'breakoutsideloop))
     (lambda (s) (error 'continueoutsideloop))
     (lambda (s) (next state))
     (lambda (s e) (throw state e))
     )))

(define runfunctionexpression
  (lambda (lis state next return throw classname)
    (cond
      [(list? (functionname lis))
       (mstate
        (bodyfromclosure (getdotfunction (cadr lis) state))
        (bindparams (paramsfromclosure (getdotfunction (cadr lis) state))(paramsfromcall lis) (addstatelayer (getmethodscope lis state)) state next return throw classname)
        (lambda (s) (error 'nonextcontinuation))
        (lambda (s) (error 'breakoutsideloop))
        (lambda (s) (error 'continueoutsideloop))
        (lambda (s) s)
        (lambda (s e) (throw state e))
        classname
        )]
      [else
       (mstate
        (bodyfromclosure (get (functionname lis) state));body
        (bindparams (paramsfromclosure (get (functionname lis) state)) (paramsfromcall lis) (addstatelayer (getscope (functionname lis) state)) state next return throw classname)
        (lambda (s) (error 'nonextcontinuation))
        (lambda (s) (error 'breakoutsideloop))
        (lambda (s) (error 'continueoutsideloop))
        (lambda (s) s)
        (lambda (s e) (throw state e))
        classname
        )]
      )))
      
(define runfunction
  (lambda (lis state next return throw classname)
    (cond
      [(list? (functionname lis))
       (mstate
        (bodyfromclosure (getdotfunction (cadr lis) state))
        (bindparams (paramsfromclosure (getdotfunction (cadr lis) state))(paramsfromcall lis) (addstatelayer (getmethodscope lis state)) state next return throw classname)
        (lambda (s) (next state))
        (lambda (s) (error 'breakoutsideloop))
        (lambda (s) (error 'continueoutsideloop))
        (lambda (s) (next state))
        (lambda (s e) (throw s e))
        classname
        )]
      [else
       (mstate
        (bodyfromclosure (get (functionname lis) state));body
        (bindparams (paramsfromclosure (get (functionname lis) state)) (paramsfromcall lis) (addstatelayer (getscope (functionname lis) state)) state next return throw classname)
        (lambda (s) (next state))
        (lambda (s) (error 'breakoutsideloop))
        (lambda (s) (error 'continueoutsideloop))
        (lambda (s) (next state))
        (lambda (s e) (throw state e))
        classname
     )]
      )))

(define getmethodscope
  (lambda (lis state)
    (cond
     [(eq? (instancename (relevant lis)) 'this) (nextlayers state)]
     [(eq? (instancename (relevant lis)) 'super)
      (adddeclare 'this (createinstance (list 'new (nameofthisclass lis state)) state (lambda (v) v) 'throw) (addmethodstoglobalthis lis state))]
     [(list? (instancename (relevant lis))) (adddeclare 'this (createinstance (cadadr lis) state (lambda (v) v) 'throw) (addmethodstoglobalnew lis state))] ;everything but the local variables
     [else (adddeclare 'this (get (instancename (relevant lis)) state) (addmethodstoglobal lis state))])))

(define relevant
  (lambda (lis)
    (cadr lis)))

(define nameofthisclass
  (lambda (lis state)
    (cadddr (car (get (classofinstance (get 'this state)) state)))))

(define addmethodstoglobalnew
  (lambda (lis state)
  (cons (car (cadr (get (cadadr (cadr lis)) state))) (getgloballayer state))))

(define addmethodstoglobalthis
  (lambda (lis state)
    (cons (car (cadr (car (get (classofinstance (get 'this state)) state)))) (getgloballayer state))))
(define addmethodstoglobal
  (lambda (lis state)
    (cons (car (cadr (get (classofinstance (get (instancename (cadr lis)) state)) state))) (getgloballayer state))))
  
(define getgloballayer
  (lambda (state)
    (cond
      [(null? (cdr state)) state]
      [else (getgloballayer (cdr state))])))

       
(define dotinstanceclosure
  (lambda (lis state)
    (get (instancename (cadr lis)) state)))
    
;Get the instance name --> get the class of the instance --> get the method from that class
(define getdotfunction
  (lambda (lis state)
    (cond
      [(eq? 'this (instancename lis)) (get (methodname lis) state)]
      [(eq? 'super (instancename lis)) (get (methodname lis) (getthismethods lis state))]
      [(list? (instancename lis)) (get (methodname lis) (getnewmethods lis state) )]
      [else (get (methodname lis) (getmethodsfromclass lis state))])))

(define getthismethods
  (lambda (lis state)
    (cadr (car (get (car (get 'this state)) state)))))
(define getmethodsfromclass
  (lambda (lis state)
    (cadr (get (classofinstance (get (instancename lis) state)) state))))
(define getnewmethods
  (lambda (lis state)
    (cadr (get (cadr (instancename lis)) state))))
    

(define classofinstance
  (lambda (lis)
    (car lis)))

(define getinstancefieldvalues
  (lambda (lis)
    (cadr lis)))

(define instancename
  (lambda (lis)
    (cadr lis)))

(define methodname
  (lambda (lis)
    (caddr lis)))
     
(define bindparams
  (lambda (formal actual environment state next return throw classname)
    (cond
      [(and (null? formal) (null? actual)) environment]
      [(or (null? formal) (null? actual)) (error 'mismatchparams)]
      [else (bindparams (remaining formal) (remaining actual) (adddeclare (selected formal) (mvalue (selected actual) state (lambda (v) v) throw classname) environment) state next return throw classname)])))

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
      [(and (nonextlayer? state) (emptycurrentlayer? state)) (error variable)]
      [(emptycurrentlayer? state) (get variable (nextlayers state))]
      [(rightvariable? variable state) (unbox (selectedvalue state))]
      [else (get variable (remainderofstate state))]
    )))

; searches for value of a variable. If it doesn't find it, searches in 'this
(define getlocaltheninstance
  (lambda (variable state restorestate)
    (cond
      [(and (nonextlayer? state) (emptycurrentlayer? state)) (executedot (list 'dot 'this variable) (restorestate) (lambda (v) v) (lambda (v) (error 'invalidthrow)))]
      [(emptycurrentlayer? state) (getlocaltheninstance variable (nextlayers state) restorestate)]
      [(rightvariable? variable state) (unbox (selectedvalue state))]
      [else (getlocaltheninstance variable (remainderofstate state) restorestate)]
    )))

(define declare
  (lambda (lis state next break continue return throw classname)
    (declarehelper lis state next break continue return throw #f classname)))

(define classdeclare
  (lambda (lis state next break continue return throw classname)
    (declarehelper lis state next break continue return throw #t classname)))
     
; declares a variable. Format: (declare '(var x 5) state)
(define declarehelper
  (lambda (lis state next break continue return throw classdeclare classname)
    (cond
      [(and (not classdeclare) (isdeclared lis (current state))) (error 'redeclarederror)] 
      [(isnovaluetoassign lis)(next (adddeclare (inputvariable lis) 'declared state))]
      [else (next (adddeclare (inputvariable lis) (mvalue (valuetoassign lis) state (lambda (v) v) throw classname) state)) ]
     )))
;currentlayer
(define current
  (lambda (state)
    (cons (currentlayer state) (emptylist))))

; assigns a value to a variable
(define assign
  (lambda (lis state next break continue return throw classname)
    (cond
      [(list? (firstexpression lis)) (assigndot lis state next break continue return throw classname)]
      [(not (isdeclared lis state)) (error 'notdeclarederror)]
      [else (next (add (inputvariable lis) (mvalue (valuetoassign lis) state (lambda (v) v) throw classname) state))]
    )))

(define assignlocaltheninstance
  (lambda (lis state next break continue return throw classname)
    (cond
      [(list? (firstexpression lis)) (assigndot lis state next break continue return throw classname)]
      [(eq? (isdeclaredlocalorinstance lis state) 'notdeclared) (error 'notdeclarederror)]
      [(eq? (isdeclaredlocalorinstance lis state) 'local) (next (add (inputvariable lis) (mvalue (valuetoassign lis) state (lambda (v) v) throw classname) state))]
      [else (assigndot (list '= (list 'dot 'this (inputvariable lis)) (valuetoassign lis)) state next break continue return throw classname)]
    )))

; uses the return continuation to stop code execution
(define returnfunction
  (lambda (lis state next break continue return throw classname)
    (return (mvalue (operatorcdr lis) state (lambda (v) v) throw classname))))
;    (cond
 ;     [(number? result) (return result)]
  ;    [(eq? result #t) (return 'true)]
   ;   [(eq? result 'true) (return 'true)]
    ;  [(eq? result #f) (return 'false)]
     ; [(eq? result 'false) (return 'false)]
      ;[else (return result)]
    ;))))

; executes an if statement (no side effects)
(define ifstatement
  (lambda (lis state next break continue return throw classname)
    (cond
      [(mvalue (ifcondition lis) state (lambda (v) v) throw classname) (mstate (thenstatement lis) state next break continue return throw classname)]
      [(null? (thenstatementcdr lis)) (next state)]
      [else (mstate (elsestatement lis) state next break continue return throw classname)]
    )))

; while loop (no side effects)
(define whileloop
  (lambda (lis state next break continue return throw classname)
    (cond
      ((null? lis) state)
      ((mvalue (whilecondition lis) state (lambda (v) v) throw classname) (mstate (whileloopbody lis) state (lambda (v) (mstate lis v next break continue return throw classname)) break continue return throw classname))
      (else (mstate (whilecondition lis) state next break continue return throw)))))


;--------------TRY CATCH--------------------
(define trycatch
  (lambda (lis state next break continue return throw classname)
      (mstate (trybody lis) state
                       (lambda (s) (mstate (finallybody lis) s next break continue return throw classname)) ;newnext
                       (lambda (s) (mstate (finallybody lis) s break break continue return throw classname)) ;newbreak
                       (lambda (s) (mstate (finallybody lis) s continue break 'continue? return throw classname)) ;newcontinue
                       (lambda (v) (return v state)) ;newreturn 
                       (lambda (s e) (mstate (catch lis) (add (catchvariable lis) e s) ; mythrow
                                              (lambda (s) (mstate (finallybody lis) s next break continue return throw classname)) ;new next
                                              (lambda (s) (mstate (finallybody lis) s break break continue return throw classname)) ;newbreak
                                              (lambda (s) (mstate (finallybody lis) s continue break 'continue? return throw classname)) ;newcontinue
                                              (lambda (v) (return v state)) ;newreturn
                                              (lambda (s1 e1) (mstate (catch lis) s1
                                                                      (lambda (s2) (throw s2 e1))
                                                                      (lambda (s2) (throw s2 e1))
                                                                      (lambda (s2) (throw s2 e1))
                                                                      (lambda (v) (throw s1 e1))
                                                                      throw
                                                                      classname)))))))

; block functionality
(define block
  (lambda (lis state next break continue return throw classname)
    (cond
      [(null? lis) (next (nextlayers state))]
      [else (mstate (operator lis) state (lambda (v) (block (operatorcdr lis) v next break continue return throw classname)) break (lambda (v) (next v)) return throw classname)]
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
  ;(lambda (variable state restorestate)
  (lambda (variable state)
    (cond
      ;[(eq? restorestate 'error) 'notdeclared]
      ;[(and (nonextlayer? state) (emptycurrentlayer? state)) (executedot (list 'dot 'this variable) (restorestate) (lambda (v) v) (lambda (v) (error 'invalidthrow)))]
      [(and (nonextlayer? state) (emptycurrentlayer? state)) 'notdeclared]
      ;[(emptycurrentlayer? state) (getnoerror variable (nextlayers state) restorestate)]
      [(emptycurrentlayer? state) (getnoerror variable (nextlayers state))]
      [(rightvariable? variable state) (unbox (selectedvalue state))]
      [else (getnoerror variable (remainderofstate state))]
      ;[else (getnoerror variable (remainderofstate state) restorestate)]
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

(define isdeclaredlocalorinstance
  (lambda (lis state)
    (cond
      [(isdeclared lis state) 'local]
      [(not (eq? (executedot (list 'dot 'this (inputvariable lis)) state (lambda (v) v) (lambda (v) (error 'invalidthrow))) 'notdeclared)) 'instancefield]
      [else 'notdeclared])))
      

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
;(parser "classtest1.txt")

;(interpret "simpleclasstest3.txt" "A")
(eq? (interpret "classtest1.txt" "A") 15)
(eq? (interpret "classtest2.txt" "A") 12)
(eq? (interpret "classtest3.txt" "A") 125)
(eq? (interpret "classtest4.txt" "A") 36)
(eq? (interpret "classtest5.txt" "A") 54)
(eq? (interpret "classtest6.txt" "A") 110)
(eq? (interpret "classtest7.txt" "C") 26)
;(interpret "classtest10.txt" "List")