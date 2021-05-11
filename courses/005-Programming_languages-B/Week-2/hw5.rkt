;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

; This homework has to do with mupl (a Made Up Programming Language). mupl programs
; are written directly in Racket by using the constructors defined by the structs defined at the beginning of
; hw5.rkt. This is the definition of mupl’s syntax:
; • If s is a Racket string, then (var s) is a mupl expression (a variable use).
; • If n is a Racket integer, then (int n) is a mupl expression (a constant).
; • If e1 and e2 are mupl expressions, then (add e1 e2) is a mupl expression (an addition).
; • If s1 and s2 are Racket strings and e is a mupl expression, then (fun s1 s2 e) is a mupl expression (a
; function). In e, s1 is bound to the function itself (for recursion) and s2 is bound to the (one) argument.
; Also, (fun #f s2 e) is allowed for anonymous nonrecursive functions.
; • If e1, e2, and e3, and e4 are mupl expressions, then (ifgreater e1 e2 e3 e4) is a mupl expression.
; It is a conditional where the result is e3 if e1 is strictly greater than e2 else the result is e4. Only one
; of e3 and e4 is evaluated.
; • If e1 and e2 are mupl expressions, then (call e1 e2) is a mupl expression (a function call).
; • If s is a Racket string and e1 and e2 are mupl expressions, then (mlet s e1 e2) is a mupl expression
; (a let expression where the value resulting e1 is bound to s in the evaluation of e2).
; • If e1 and e2 are mupl expressions, then (apair e1 e2) is a mupl expression (a pair-creator).
; • If e1 is a mupl expression, then (fst e1) is a mupl expression (getting the first part of a pair).
; • If e1 is a mupl expression, then (snd e1) is a mupl expression (getting the second part of a pair).
; • (aunit) is a mupl expression (holding no data, much like () in ML or null in Racket). Notice
; (aunit) is a mupl expression, but aunit is not.
; • If e1 is a mupl expression, then (isaunit e1) is a mupl expression (testing for (aunit)).
; • (closure env f) is a mupl value where f is mupl function (an expression made from fun) and env
; is an environment mapping variables to values. Closures do not appear in source programs; they result
; from evaluating functions.
; A mupl value is a mupl integer constant, a mupl closure, a mupl aunit, or a mupl pair of mupl values.
; Similar to Racket, we can build list values out of nested pair values that end with a mupl aunit. Such a
; mupl value is called a mupl list.
; You should assume mupl programs are syntactically correct (e.g., do not worry about wrong things like (int
; "hi") or (int (int 37)). But do not assume mupl programs are free of type errors like (add (aunit)
; (int 7)) or (fst (int 7)).

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1
; (a) Write a Racket function racketlist->mupllist that takes a Racket list (presumably of mupl
; values but that will not affect your solution) and produces an analogous mupl list with the same
; elements in the same order.
(define (racketlist->mupllist lst)
    (if (null? lst)
        (aunit)
        (apair (car lst) (racketlist->mupllist (cdr lst)))))

; (b) Write a Racket function mupllist->racketlist that takes a mupl list (presumably of mupl
; values but that will not affect your solution) and produces an analogous Racket list (of mupl
; values) with the same elements in the same order.
(define (mupllist->racketlist lst)
    (if (aunit? lst)
        null
        (cons (apair-e1 lst) (mupllist->racketlist (apair-e2 lst)))))

;; Problem 2
; Implementing the mupl Language: Write a mupl interpreter, i.e., a Racket function eval-exp
; that takes a mupl expression e and either returns the mupl value that e evaluates to under the empty
; environment or calls Racket’s error if evaluation encounters a run-time mupl type error or unbound
; mupl variable.
; A mupl expression is evaluated under an environment (for evaluating variables, as usual). In your
; interpreter, use a Racket list of Racket pairs to represent this environment (which is initially empty)
; so that you can use without modification the provided envlookup function. Here is a description of
; the semantics of mupl expressions:
; • All values (including closures) evaluate to themselves. For example, (eval-exp (int 17)) would
; return (int 17), not 17.
; • A variable evaluates to the value associated with it in the environment.
; • An addition evaluates its subexpressions and assuming they both produce integers, produces the
; integer that is their sum. (Note this case is done for you to get you pointed in the right direction.)
; • Functions are lexically scoped: A function evaluates to a closure holding the function and the
; current environment.
; • An ifgreater evaluates its first two subexpressions to values v1 and v2 respectively. If both
; values are integers, it evaluates its third subexpression if v1 is a strictly greater integer than v2
; else it evaluates its fourth subexpression.
; • An mlet expression evaluates its first expression to a value v. Then it evaluates the second
; expression to a value, in an environment extended to map the name in the mlet expression to v.
; • A call evaluates its first and second subexpressions to values. If the first is not a closure, it is an
; error. Else, it evaluates the closure’s function’s body in the closure’s environment extended to map
; the function’s name to the closure (unless the name field is #f) and the function’s argument-name
; (i.e., the parameter name) to the result of the second subexpression.
; • A pair expression evaluates its two subexpressions and produces a (new) pair holding the results.
; • A fst expression evaluates its subexpression. If the result for the subexpression is a pair, then the
; result for the fst expression is the e1 field in the pair.
; • A snd expression evaluates its subexpression. If the result for the subexpression is a pair, then
; the result for the snd expression is the e2 field in the pair.
; • An isaunit expression evaluates its subexpression. If the result is an aunit expression, then the
; result for the isaunit expression is the mupl value (int 1), else the result is the mupl value
; (int 0).
; Hint: The call case is the most complicated. In the sample solution, no case is more than 12 lines
; and several are 1 line.

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e)
         (envlookup env (var-string e))]
        [(add? e)
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; CHANGE add more cases here
        [(or (int? e)
             (closure? e)
             (aunit? e)) 
         e]
        [(fun? e) (closure env e)]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (cond [(not (and (int? v1) (int? v2)))
                  (error "MUPL ifgreater applied to non-number")]
                 [(> (int-num v1) (int-num v2))
                  (eval-under-env (ifgreater-e3 e) env)]
                 [#t (eval-under-env (ifgreater-e4 e) env)]))]
        [(mlet? e)
         (let ([v (eval-under-env (mlet-e e) env)])
           (eval-under-env (mlet-body e) 
                           (cons (cons (mlet-var e) v) 
                                 env)))]
        [(call? e)
         (let ([cl (eval-under-env (call-funexp e) env)]
               [arg (eval-under-env (call-actual e) env)])
           (if (closure? cl)
               (letrec ([fun (closure-fun cl)]
                        [fun-env (cons (cons (fun-formal fun) arg)
                                       (if (fun-nameopt fun)
                                           (cons (cons (fun-nameopt fun) cl) (closure-env cl))
                                           (closure-env cl)))])
                 (eval-under-env (fun-body fun) fun-env))
               (error (format "MUPL function call applied to non-function: ~v" cl))))]
        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]
        [(fst? e)
         (let ([pr (eval-under-env (fst-e e) env)])
           (if (apair? pr)
               (apair-e1 pr)
               (error (format "MUPL fst applied to non-pair: ~v" pr))))]
        [(snd? e)
         (let ([pr (eval-under-env (snd-e e) env)])
           (if (apair? pr)
               (apair-e2 pr)
               (error "MUPL snd applied to non-pair")))]
        [(isaunit? e)
         (let ([un (eval-under-env (isaunit-e e) env)])
           (if (aunit? un)
               (int 1)
               (int 0)))]
        [#t (error (format "bad MUPL expression: ~v" e))]))
; my-test-cases
;(displayln (eval-under-env (var "a") (list (cons "a" (int 5))))) ; (int 5)
;(displayln (eval-under-env (int 17) null )) ; (int 17)
;(displayln (eval-under-env (fun "add1" "x" (add (var "x") (int 1))) null )) ; (closure null (fun "add1" "x" (add (var "x") (int 1))))
;(displayln (eval-under-env (fun #f "x" (add (var "x") (int 1))) null )) ; (closure null (fun #f "x" (add (var "x") (int 1))))
;(displayln (eval-under-env (ifgreater (var "x") (var "y") (add (var "x") (int 1)) (int 33)) (list (cons "x" (int 5)) (cons "y" (int 3))) )) ; (int 6)
;(displayln (eval-under-env (ifgreater (var "x") (var "y") (add (var "x") (int 1)) (int 33)) (list (cons "x" (int 1)) (cons "y" (int 3))) )) ; (int 33)
;(displayln (eval-under-env (mlet "a" (add (int 1) (var "x")) (add (var "a") (var "x"))) (list (cons "x" (int 2))) )) ; (int 5)
;(displayln (eval-under-env (mlet "add1" (fun #f "x" (add (var "x") (int 1)))
;                                        (call (var "add1") (int 5)))
;                           null)) ; (int 6)
;(displayln (eval-under-env (apair (int 17) (var "a")) (list (cons "a" (int 5))))) ; (apair (int 17) (int 5))
;(displayln (eval-under-env (fst (apair (int 17) (var "a"))) (list (cons "a" (int 5))))) ; (int 17)
;(displayln (eval-under-env (snd (apair (int 17) (var "a"))) (list (cons "a" (int 5))))) ; (int 5)
; recursion
;(displayln (eval-under-env (mlet "up-to-15" (fun "aux" "x" (ifgreater (var "x") (int 15) (int 0) (add (int 1) (call (var "aux") (add (int 1) (var "x"))))))
;                                        (call (var "up-to-15") (int 11))) null)) ; (int 5)
; carrying
;(displayln (eval-under-env (mlet "car-add" (fun #f "x" (fun #f "y" (add (var "x") (var "y"))))
;    (call (call (var "car-add") (int 5)) (int 3))) null)) ; (int 8)
; multiplication (just for fun)
;(define mupl-multiplication
;    (fun #f "pr"
;         (mlet "aux" (fun "aux-inner" "n" 
;                          (ifgreater (fst (var "pr"))
;                                     (var "n") 
;                                     (add (snd (var "pr"))
;                                          (call (var "aux-inner") (add (var "n") (int 1))))
;                                     (int 0)))
;            (call (var "aux") (int 0)))))
;(displayln (eval-under-env (mlet "mul" mupl-multiplication
;                                (call (var "mul") (apair (int 10) (int 11)))) null)) ; (int 110)
;(define mupl-square
;    (fun #f "n"
;        (mlet "mul" mupl-multiplication
;              (call (var "mul") (apair (var "n") (var "n"))))))
;(displayln (eval-under-env (mlet "square" mupl-square
;                                (call (var "square") (int 25))) null)) ; (int 625)

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3
; 3. Expanding the Language: mupl is a small language, but we can write Racket functions that act like
; mupl macros so that users of these functions feel like mupl is larger. The Racket functions produce
; mupl expressions that could then be put inside larger mupl expressions or passed to eval-exp. In
; implementing these Racket functions, do not use closure (which is used only internally in eval-exp).
; Also do not use eval-exp (we are creating a program, not running it).

; (a) Write a Racket function ifaunit that takes three mupl expressions e1, e2, and e3. It returns a
; mupl expression that when run evaluates e1 and if the result is mupl’s aunit then it evaluates e2
; and that is the overall result, else it evaluates e3 and that is the overall result. Sample solution: 1 line.
(define (ifaunit e1 e2 e3) 
    (ifgreater (isaunit e1) (int 0) e2 e3))

; (b) Write a Racket function mlet* that takes a Racket list of Racket pairs ’((s1 . e1) . . . (si . ei) . . . (sn . en))
; and a final mupl expression en+1. In each pair, assume si is a Racket string and ei
; is a mupl expression. mlet* returns a mupl expression whose value is en+1 evaluated in an
; environment where each si is a variable bound to the result of evaluating the corresponding ei
; for 1 ≤ i ≤ n. The bindings are done sequentially, so that each ei is evaluated in an environment
; where s1 through si−1 have been previously bound to the values e1 through ei−1.
(define (mlet* lstlst e2)
    (if (null? lstlst)
        e2
        (mlet (car (car lstlst)) 
              (cdr (car lstlst))
              (mlet* (cdr lstlst) e2))))

; (c) Write a Racket function ifeq that takes four mupl expressions e1, e2, e3, and e4 and returns
; a mupl expression that acts like ifgreater except e3 is evaluated if and only if e1 and e2 are
; equal integers. Assume none of the arguments to ifeq use the mupl variables _x or _y. Use this
; assumption so that when an expression returned from ifeq is evaluated, e1 and e2 are evaluated
; exactly once each.
(define (ifeq e1 e2 e3 e4)
    (mlet* (list (cons "_x" e1) (cons "_y" e2))
           (ifgreater (var "_x") (var "_y")
                      e4
                      (ifgreater (var "_y") (var "_x") e4 e3))))

;; Problem 4
; 4. Using the Language: We can write mupl expressions directly in Racket using the constructors for
; the structs and (for convenience) the functions we wrote in the previous problem.
; (a) Bind to the Racket variable mupl-map a mupl function that acts like map (as we used extensively
; in ML). Your function should be curried: it should take a mupl function and return a mupl
; function that takes a mupl list and applies the function to every element of the list returning a
; new mupl list. Recall a mupl list is aunit or a pair where the second component is a mupl list.
(define mupl-map
    (fun #f "f"
        (fun "inner-map" "lst"
            (ifaunit (var "lst")
                     (aunit)
                     (apair (call (var "f") (fst (var "lst")))
                            (call (var "inner-map") (snd (var "lst"))))))))

; (b) Bind to the Racket variable mupl-mapAddN a mupl function that takes an mupl integer i and
; returns a mupl function that takes a mupl list of mupl integers and returns a new mupl list of
; mupl integers that adds i to every element of the list. Use mupl-map (a use of mlet is given to
; you to make this easier).
(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "int"
            (call (var "map") (fun #f "x" (add (var "x") (var "int")))))))

;; Challenge Problem
; Write a second version of eval-exp (bound to eval-exp-c) that builds closures
; with smaller environments: When building a closure, it uses an environment that is like the current
; environment but holds only variables that are free variables in the function part of the closure. (A free
; variable is a variable that appears in the function without being under some shadowing binding for the
; same variable.)
; Avoid computing a function’s free variables more than once. Do this by writing a function compute-free-vars
; that takes an expression and returns a different expression that uses fun-challenge everywhere in
; place of fun. The new struct fun-challenge (provided to you; do not change it) has a field freevars
; to store exactly the set of free variables for the function. Store this set as a Racket set of Racket strings.
; (Sets are predefined in Racket’s standard library; consult the documentation for useful functions such
; as set, set-add, set-member?, set-remove, set-union, and any other functions you wish.)
; You must have a top-level function compute-free-vars that works as just described — storing the
; free variables of each function in the freevars field — so the grader can test it directly. Then write a
; new “main part” of the interpreter that expects the sort of mupl expression that compute-free-vars
; returns. The case for function definitions is the interesting one.

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function


; helper for compute free variables in fun expression
; uses mutual recursion to handle case where fun body contains another fun expression
(define (compute-fun-free-vars f)
    (letrec ([fun-free-vars 
              (lambda (f)
                (let ([nameopt (fun-nameopt f)]
                      [formal (fun-formal f)]
                      [body (fun-body f)])
                    (set-remove
                        (set-remove (expression-vars body)
                                    nameopt)
                        formal)))]
             [expression-vars
              (lambda (e)
                (cond [(or (int? e)
                           (closure? e)
                           (aunit? e)) 
                       (set)]
                      [(var? e) (set (var-string e))]
                      [(add? e) 
                       (set-union (expression-vars (add-e1 e))
                                  (expression-vars (add-e2 e)))]
                      [(fun? e)
                       (fun-free-vars e)]
                      [(ifgreater? e)
                       (set-union (expression-vars (ifgreater-e1 e))
                                  (expression-vars (ifgreater-e2 e))
                                  (expression-vars (ifgreater-e3 e))
                                  (expression-vars (ifgreater-e4 e)))]
                      [(mlet? e)
                       (set-union (expression-vars (mlet-e e))
                                  (expression-vars (mlet-body e)))]
                      [(call? e)
                       (set-union (expression-vars (call-funexp e))
                                  (expression-vars (call-actual e)))]
                      [(apair? e)
                       (set-union (expression-vars (apair-e1 e))
                                  (expression-vars (apair-e2 e)))]
                      [(fst? e)
                       (set (expression-vars (fst-e e)))]
                      [(snd? e)
                       (set (expression-vars (snd-e e)))]
                      [(isaunit? e)
                       (set (expression-vars (isaunit-e e)))]))])
        (fun-free-vars f)))
;(displayln (compute-fun-free-vars (fun #f "x" (add (var "x") (var "y"))))) ; (set "y")
;(displayln (compute-fun-free-vars (fun "aux" "x" (ifgreater (var "x") (var "b") (int 0) (add (int 1) (call (var "aux") (add (var "a") (var "x")))))))) ; (set "b" "a")
;(displayln (compute-fun-free-vars (fun #f "x" (add (var "a") (call (fun #f "y" (add (var "x") (var "b"))) (var "c")))))) ; (set "a" "b" "c")

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e)
  (cond [(or (var? e)
             (int? e)
             (closure? e)
             (aunit? e)) 
         e]
        [(add? e) 
         (add (compute-free-vars (add-e1 e))
              (compute-free-vars (add-e2 e)))]
        [(fun? e)
         (fun-challenge (fun-nameopt e)
                        (fun-formal e)
                        (fun-body e)
                        (compute-fun-free-vars e))]
        [(ifgreater? e)
         (ifgreater (compute-free-vars (ifgreater-e1 e))
                    (compute-free-vars (ifgreater-e2 e))
                    (compute-free-vars (ifgreater-e3 e))
                    (compute-free-vars (ifgreater-e4 e)))]
        [(mlet? e)
         (mlet (mlet-var e)
               (compute-free-vars (mlet-e e))
               (compute-free-vars (mlet-body e)))]
        [(call? e)
         (call (compute-free-vars (call-funexp e))
               (compute-free-vars (call-actual e)))]
        [(apair? e)
         (apair (compute-free-vars (apair-e1 e))
                (compute-free-vars (apair-e2 e)))]
        [(fst? e)
         (fst (compute-free-vars (fst-e e)))]
        [(snd? e)
         (snd (compute-free-vars (snd-e e)))]
        [(isaunit? e)
         (snd (compute-free-vars (isaunit-e e)))]))
;(displayln (compute-free-vars (var "a"))) ; (var "a")
;(displayln (compute-free-vars (int 17))) ; (int 17)
;(displayln (compute-free-vars (add (int 5) (int 6)))) ; (add (int 5) (int 6))
;(displayln (compute-free-vars (fun "add1" "x" (add (var "x") (int 1))))) ; (fun-challenge "add1" "x" (add (var "x") (int 1)) (set))
;(displayln (compute-free-vars (fun #f "x" (add (var "x") (int 1))))) ; (fun-challenge #f "x" (add (var "x") (int 1)) (set))
;(displayln (compute-free-vars (fun #f "x" (add (var "x") (var "y"))))) ; (fun-challenge #f "x" (add (var "x") (var "y")) (set "y"))
;(displayln (compute-free-vars (fun #f "x" (add (var "y") (var "y"))))) ; (fun-challenge #f "x" (add (var "y") (var "y")) (set "y"))
;(displayln (compute-free-vars (fun "aux" "x" (ifgreater (var "x") (int 15) (int 0) (add (int 1) (call (var "aux") (add (int 1) (var "x"))))))))
;;(fun-challenge "aux" "x" (ifgreater (var "x") (int 15) (int 0) (add (int 1) (call (var "aux") (add (int 1) (var "x"))))) (set))
;(displayln (compute-free-vars (fun "aux" "x" (ifgreater (var "x") (var "b") (int 0) (add (int 1) (call (var "aux") (add (var "a") (var "x"))))))))
;;(fun-challenge "aux" "x" (ifgreater (var "x") (var "b") (int 0) (add (int 1) (call (var "aux") (add (var "a") (var "x"))))) (set "a" "b"))


; helper for compute smaller enviroment for closure
(define (compute-minimal-env env vars)
    (

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env)
  (cond [(var? e)
         (envlookup env (var-string e))]
        [(add? e)
         (let ([v1 (eval-under-env-с (add-e1 e) env)]
               [v2 (eval-under-env-с (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(or (int? e)
             (closure? e)
             (aunit? e)) 
         e]
        [(fun-challenge? e) (closure (compute-minimal-env env
                                                          (fun-challenge-freevars e))
                                     e)]
        [(ifgreater? e)
         (let ([v1 (eval-under-env-с (ifgreater-e1 e) env)]
               [v2 (eval-under-env-с (ifgreater-e2 e) env)])
           (cond [(not (and (int? v1) (int? v2)))
                  (error "MUPL ifgreater applied to non-number")]
                 [(> (int-num v1) (int-num v2))
                  (eval-under-env-с (ifgreater-e3 e) env)]
                 [#t (eval-under-env-с (ifgreater-e4 e) env)]))]
        [(mlet? e)
         (let ([v (eval-under-env-с (mlet-e e) env)])
           (eval-under-env-с (mlet-body e) 
                           (cons (cons (mlet-var e) v) 
                                 env)))]
        [(call? e)
         (let ([cl (eval-under-env-с (call-funexp e) env)]
               [arg (eval-under-env-с (call-actual e) env)])
           (if (closure? cl)
               (letrec ([fun (closure-fun cl)]
                        [fun-env (cons (cons (fun-challenge-formal fun) arg)
                                       (if (fun-challenge-nameopt fun)
                                           (cons (cons (fun-challenge-nameopt fun) cl) (closure-env cl))
                                           (closure-env cl)))])
                 (eval-under-env-с (fun-challenge-body fun) fun-env))
               (error (format "MUPL function call applied to non-function: ~v" cl))))]
        [(apair? e)
         (let ([v1 (eval-under-env-с (apair-e1 e) env)]
               [v2 (eval-under-env-с (apair-e2 e) env)])
           (apair v1 v2))]
        [(fst? e)
         (let ([pr (eval-under-env-с (fst-e e) env)])
           (if (apair? pr)
               (apair-e1 pr)
               (error (format "MUPL fst applied to non-pair: ~v" pr))))]
        [(snd? e)
         (let ([pr (eval-under-env-с (snd-e e) env)])
           (if (apair? pr)
               (apair-e2 pr)
               (error "MUPL snd applied to non-pair")))]
        [(isaunit? e)
         (let ([un (eval-under-env-с (isaunit-e e) env)])
           (if (aunit? un)
               (int 1)
               (int 0)))]
        [#t (error (format "bad MUPL expression: ~v" e))]))
;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
