#lang pl

#| Please complete the missing rules below
;it was easy
<SOL> :: = { <NumList> }
        |  { scalar-mult <num> <SOL> }
        |  { intersect <SOL> <SOL>}
        |  { union <SOL> <SOL> }
        |  <id>
        |  { with {<id> <SOL>  <id> <SOL>} <SOL> } ;; this should be a syntactic sugar
        |  { fun { <id> <id> } <SOL> } ;; a function must have exactly two formal parameters
        |  { call-static <SOL> <SOL> <SOL> } ;; extends closure environment
        |  { call-dynamic <SOL> <SOL> <SOL> } ;; extends current environment

        |  True
        |  False
        | { if <SOL> then <SOL> else <SOL> }
        | { equal? <SOL> <SOL> }

<NumList> :: =  λ | <num> <NumList> ;; where λ stands for the empty word, i.e., { } is the empty set

;; where <num> is any expression identified by Racket as a Number
;; and <id> is any expression such that Racket identifies '<id> as a symbol
 
|#


;; -----------------------------------------------------
;; The abstract syntax tree SOL
;it was easy
(define-type SET = (Listof Number))
(define-type SOL
  ;; Please complete the missing parts -- you are NOT allowed to use additional variants (constructors)
  [Set  SET]
  [Smult Number SOL]
  [Inter SOL SOL]
  [Union SOL SOL]
  [Id    Symbol]
  ;;    [With  Symbol Symbol SOL SOL SOL] -- not to be used, syntactic sugar for ...
  [Fun   Symbol Symbol SOL]
  [CallS SOL SOL SOL]
  [CallD SOL SOL SOL]
  [Bool Boolean]
  [If SOL SOL SOL]
  [Equal SOL SOL])


;; ----------------------------------------------------
;; Operations on SETs
;; Please complete the missing parts, and add comments (comments should specify 
;; the role of each procedure, but also describe your work process). Keep your code readable. 

(: ismember? : Number SET  -> Boolean)
(define (ismember? n l)
  (cond [(null? l) #f]
        [(= n (first l)) #t]
        [else (ismember? n (rest l))]))

(test (not (ismember? 1 '(3 4 5))))
(test (not (ismember? 1 '( 3 2 3 5 6))))
(test (ismember? 1 '(3 4 5 1 3 4)))
(test (ismember? 1 '(1)))

(: remove-duplicates : SET  -> SET)
(define (remove-duplicates l)
  (cond [(or (null? l) (null? (rest l))) l]
        [(ismember? (first l) (rest l)) (remove-duplicates (rest l))]
        [else (cons (first l) (remove-duplicates (rest l)))]))
  
(: create-sorted-set : SET -> SET)
(define (create-sorted-set l)
  (remove-duplicates (sort l <)))
  
(: set-union : SET SET -> SET)
(define (set-union A B)
  (create-sorted-set (append A B)))

(: set-intersection : SET SET -> SET)
(define (set-intersection A B)
  (: mem-filter : Number -> Boolean)
  (define (mem-filter n)
    (ismember? n A))
  (filter mem-filter (create-sorted-set B)))

  


;; ---------------------------------------------------------
;; Parser
;; Please complete the missing parts, and add comments (comments should specify 
;; choices you make, and also describe your work process). Keep your code readable.
;took me about 5 minuts ,by looking at the exampels in lecture.
;main dificult in  static and dynamic and convert with to fun constractor
(: parse-sexpr : Sexpr -> SOL)
;; to convert s-expressions into SOLs
(define (parse-sexpr sexpr)
  (match sexpr
    [(list (number: ns) ...) (Set (create-sorted-set ns))] ;; sort and remove-duplicates
    ['True (Bool true)] 
    ['False (Bool false)] 
    [(symbol: name) (Id name)]
 [(cons 'with more)
       (match sexpr
         [(list 'with (list (symbol: name1) named1 (symbol: name2) named2) body)
          (if (not (equal? name1 name2))
               ;; cannot use the same param name twice
              (CallS (Fun name1 name2 (parse-sexpr body)) (parse-sexpr named1) (parse-sexpr named2) )
              (error 'parse-sexpr " `fun' has a duplicate param name in ~s" sexpr))] ;;; There is no With constructor. Replace it with existing constructors...
         [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])]
    [(cons 'fun more)
       (match sexpr
         [(list 'fun (list (symbol: name1) (symbol: name2)) body)
          (if (equal? name1 name2)
              (error 'parse-sexpr " `fun' has a duplicate param name in ~s" sexpr) ;; cannot use the same param name twice
              (Fun name1 name2 (parse-sexpr body)))]
         [else (error 'parse-sexpr "bad `fun' syntax in ~s" sexpr)])]
    [(list 'scalar-mult (number: sc) rhs) (Smult sc (parse-sexpr rhs))]
    [(list 'intersect lhs rhs) (Inter (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list 'union lhs rhs) (Union (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list 'call-static fun arg1 arg2) (CallS (parse-sexpr fun) (parse-sexpr arg1) (parse-sexpr arg2))]
    [(list 'call-dynamic fun arg1 arg2) (CallD (parse-sexpr fun) (parse-sexpr arg1) (parse-sexpr arg2))]
    [(list 'if cond 'then true-cond 'else false-cond) (If (parse-sexpr cond) (parse-sexpr true-cond) (parse-sexpr false-cond))] 
    [(list 'equal? r l) ( Equal (parse-sexpr r) (parse-sexpr l))] 
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

(: parse : String -> SOL)
;; parses a string containing a SOL expression to a SOL AST
(define (parse str)
  (parse-sexpr (string->sexpr str)))


;;; Tests for parse
 
(test (parse "{1 2 3  4 1 4  4 2 3 4 1 2 3}") => (Set '(1 2 3 4)))
(test (parse "{union {1 2 3} {4 2 3}}") => (Union (Set '(1 2 3)) (Set '(2 3 4))))
(test (parse "{fun {x x} x}") =error> "parse-sexpr: `fun' has a duplicate param name in (fun (x x) x)")
(test (parse "{with {x {} x {}} x}") =error> "parse-sexpr: `fun' has a duplicate param name in {with {x {} x {}} x}")
(test (parse "{with {x {} x {}} x x}") =error> "parse-sexpr: bad `with' syntax in {with {x {} x {}} x x}")

(test (parse "{func {x x} x}") =error> "parse-sexpr: bad syntax in in (func (x x) x)")

(test (parse "{intersect {1 2 3} {4 2 3}}") => (Inter (Set '(1 2 3)) (Set '(2 3 4))))
(test (parse "{with {S {intersect {1 2 3} {4 2 3}} c {}}
                 {call-static {fun {x y} {union x S}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}") 
      =>
      (CallS (Fun 'S
                  'c
                  (CallS (Fun 'x 'y (Union (Id 'x) (Id 'S))) 
                         (Smult 3 (Id 'S)) 
                         (Set '(4 5 6 7 8 9))))
             (Inter (Set '(1 2 3)) (Set '(2 3 4)))
             (Set '())))


(test (parse "{with {S {intersect {1 2 3} {4 2 3}} S1 {union {1 2 3} {4 2 3}}}
                          {fun {x} S}}")
      =error> "parse-sexpr: bad `fun' syntax in (fun (x) S)") ;; functions require two formal parameters
(test (parse "True") => (Bool true))
(test (parse "False") => (Bool false))
(test (parse "{if {equal? {1 2 3} {1 2}} then {1 2 3} else {1 2}}") =>
      (If (Equal (Set '(1 2 3)) (Set '(1 2))) (Set '(1 2 3)) (Set '(1 2))))

(test (parse "{with {S {intersect {1 2 3} {4 2 3}} c {}}
                 {call-dynamic {fun {x y} {union x S}}
                               {if {equal? S {scalar-mult 3 S}}
                                   then S
                                   else {4 5 7 6 9 8 8 8}}
                               {}}}")
=> (CallS (Fun 'S 'c
               (CallD (Fun 'x 'y (Union (Id 'x) (Id 'S)))
                      (If (Equal (Id 'S) (Smult 3 (Id 'S)))
                          (Id 'S)
                          (Set '(4 5 6 7 8 9)))
                      (Set '())))
          (Inter (Set '(1 2 3)) (Set '(2 3 4)))
          (Set '())))


;;-----------------------------------------------------
;; Evaluation 
#|
------------------------------------------------------
Evaluation rules:
;took me 1 hour to undestand the different between dynamic and static scoping
;i went over the matirial in calss
    ;; Please complete the missing parts in the formal specifications below
    eval({ N1 N2 ... Nl })      = sort( create-set({ N1 N2 ... Nl })) ;; where create-set removes all duplications from
                                                                         the sequence (list) and sort is a sorting procedure

    eval({scalar-mult K E})     = { K*N1 K*N2 ... K*Nl }              ;; where eval(E)={ N1 N2 ... Nl }
    eval({intersect E1 E2})     = sort( create-set(set-intersection (eval(E1) , eval(E2)))     
    eval({union E1 E2})         = sort( create-set(set-union (eval(E1) , eval(E2)))
    eval({fun {x1 x2} E},env)   = <{fun {x1 x2} E}, env>
    eval({call-static E-op E1 E2},env)
                             = eval(Ef,extend(E1,eval(E1,extend(E2,eval(E2,env)),env2))
                                                      if eval(E-op,env) = {fun {x} Ef,env2}
                                = error!              otherwise
    eval({call-dynamic E-op E1 E2},env)
                                = eval(Ef,extend(E1,eval(E1,extend(E2,eval(E2,env)),env))
                                                      if eval(E-op,env) = {fun {x} Ef,env2}

                                = error!              otherwise

    eval(True,env)              = true
    eval(False,env)             = false
    eval({if E1 then E2 else E3},env)
                                = eval(E3, env)       if eval(E1,env) = false
                                = eval(E3, env)     otherwise

    eval({equal? E1 E2},env)    = true                if eval(E1,env) is equal in content to eval(E2,env)
                                = false               otherwise





|#

;; Types for environments, values, and a lookup function

(define-type ENV
  [EmptyEnv]
  [Extend Symbol VAL ENV])
  ;[Extend 'cons (VAL VAL -> ((VAL VAL  -> VAL))) ENV])
;;by looking at the types SOL should return
(define-type VAL
  [SetV SET]
  [FunV Symbol Symbol SOL ENV]
  [BoolV Boolean]
) 

(: lookup : Symbol ENV -> VAL)
(define (lookup name env)
  (cases env
    [(EmptyEnv) (error 'lookup "no binding for ~s" name)]
    [(Extend id val rest-env)
     (if (eq? id name) val (lookup name rest-env))]))


;; Auxiliary procedures for eval 
;; Please complete the missing parts, and add comments (comments should specify 
;; the role of each procedure, but also describe your work process). Keep your code readable. 

(: SetV->set : VAL -> SET)
(define (SetV->set v)
  (cases v
    [(SetV S) S]
    [else (error 'SetV->set "expects a set, got: ~s" v)]))


(: smult-set : Number VAL -> VAL)
(define (smult-set n s)
  (: mult-op : Number -> Number)
  (define (mult-op k)
    (* k n))
  (SetV  (map mult-op  (SetV->set s))))


;;took me 1 hour to siove ,it was dificult to understand what is the purpose of this func
(: set-op :  (SET SET -> SET) VAL VAL -> VAL)
;; gets a binary SET operator, and uses it within a SetV
;; wrapper
(define (set-op op val1 val2)
  (SetV (op (SetV->set val1) (SetV->set val2))))

;;---------  the eval procedure ------------------------------
;; Please complete the missing parts, and add comments (comments should specify 
;; the choices you make, and also describe your work process). Keep your code readable. 
(: eval : SOL ENV -> VAL)
;took me 3 hours to solve.
;main dificults in soving the synamic static scoping and the if else problem.i solved it by undestand the formal pattern above
;; evaluates SOL expressions by reducing them to set values
(define (eval expr env)
  (cases expr
    [(Set S) (SetV S)]
    [(Smult n set) (smult-set n (eval set env) )]
    [(Inter l r) (set-op set-intersection (eval l env) (eval r env))]
    [(Union l r) (set-op set-union (eval l env) (eval r env))]
    [(Id name) (lookup name env)]
    [(Fun bound-id1 bound-id2 bound-body)
     (FunV bound-id1 bound-id2 bound-body env)]
    [(CallS fun-expr arg-expr1 arg-expr2)
     (let ([fval (eval fun-expr env)])
       (cases fval
         [(FunV bound-id1 bound-id2 bound-body f-env)
          (eval bound-body
                (Extend bound-id2  (eval arg-expr2 env) (Extend bound-id1  (eval arg-expr1 env) f-env)))]
        ; [(consv consfun) (consfun (eval arg-expr1 env) (eval arg-expr2 env))]
        ; [firstv (((SET SET  -> SET) -> SET) -> SET)]
         [else (error 'eval "`call-static' expects a function, got: ~s"
                      fval)]))]
    [(CallD fun-expr arg-expr1 arg-expr2)
     (let ([fval (eval fun-expr env)])
       (cases fval
         [(FunV bound-id1 bound-id2 bound-body f-env)
          (eval bound-body
                (Extend bound-id2  (eval arg-expr2 env) (Extend bound-id1  (eval arg-expr1 env) env)))]
         [else (error 'eval "`call-dynamic' expects a function, got: ~s"
                      fval)]))];
    [(Bool b) (if (equal? b false)
                  (BoolV false)
                  (BoolV true))]
    [(If cond true-cond false-cond)
     (let ([cval (eval cond env)])
       (cases cval
         [(BoolV b) (if (equal? b #t) (eval true-cond env) (eval false-cond env) )]
         [else (error 'eval "not a bool")]))] 
    [(Equal l r) (if (equal? (eval l env) (eval r env)) (BoolV true) (BoolV false))]))



;took me 3 days to solve it
;i tryed to implement i separete procedure but it went wronge,so itook inpair from last lecture and used dynamice scoping so  the 2 arguments will be defined latter
(: createGlobalEnv : -> ENV)
(define (createGlobalEnv)
  (Extend 'second
          (FunV 'Prosedure 'emp (CallS         (Id 'Prosedure)         (Fun 'x 'y (Id 'y))       (Set '(1 2 3)))    (EmptyEnv))      
          (Extend
            'first
             (FunV 'Prosedure 'emp (CallS       (Id 'Prosedure)        (Fun 'x 'y (Id 'x))       (Set '(1 2 3)))    (EmptyEnv))
                  (Extend 
                   'cons     
                    (FunV 'x 'y (Fun      'selector     'emp    (CallD     (Id 'selector)    (Id 'x)    (Id 'y))) (EmptyEnv))
                          (EmptyEnv)))))

(: run : String -> (U SET VAL Boolean))
;;solve it by looking at the types SOL should return,no difficults
;; evaluate a SOL program contained in a string
(define (run str)
  (let ([result (eval (parse str) (createGlobalEnv))])
    (cases result
      [(SetV S) S]
      [(BoolV b) b]
       [else (error 'eval "`canot return function"
                      )])))


(test (run "{1 2 3  4 1 4  4 2 3 4 1 2 3}") => '(1 2 3 4))
(test (run "{union {1 2 3} {4 2 3}}") => '(1 2 3 4))
(test (run "{intersect {1 2 3} {4 2 3}}") => '( 2 3))
(test (run "{with {S {intersect {1 2 3} {4 2 3}}
                   S1 {}}
                 {call-static {fun {x y} {union x S}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}")
      => '(2 3 6 9))


(test (run "{with {S {intersect {1 2 3} {4 2 3}}
                   S1 {}}
               {call-static {fun {x y} {union x y}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}")
      => '(4 5 6 7 8 9))
;(test (run "{with {p {call-static cons {1 2 3} {4 2 3}}
   ;                 S1 {}} {call-static first p {}}}")=> '{1 2 3})


(test (run "{with {p {call-static cons {1 2 3} {4 2 3}}
                    S1 {}} 
              {with {S {intersect {call-static first p {}}
                                  {call-static second p {}}}
                     S1 {}}
                 {call-static {fun {x y} {union x S}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}}")
      =>  '(2 3 6 9))
(test (run "{fun {x y} x}") =error> "eval: `canot return function")

(test (run "{with {p {call-dynamic cons {1 2 3} {4 2 3}}
                   S1 {}}
              {with {S {intersect {call-dynamic first p {}}
                                  {call-dynamic second p {}}}
                     S1 {}}
                 {call-dynamic {fun {x y} {union x S}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}}")
      =>  '(2 3 6 9))
(test (run "{union S {4 2 3}}")
      =error> "no binding for S")
(test (run "{call-static {1} {2 2} {}}")
      =error> "eval: `call-static' expects a function, got: #(struct:SetV (1))")
(test (run "{call-dynamic {1} {2 2} {}}")
      =error> "eval: `call-dynamic' expects a function, got: #(struct:SetV (1))")
(test (run "True") => #t)
(test (run "False") => #f)
(test (run "{if {equal? {1 2 3} {1 2}} then {1 2 3} else {1 2}}") => '(1 2))
(test (run "{equal? {union {1 2 3} {4 2 3}} {1 2 3 4}}") => #t)
(test (run "{union {equal? {4} {4}} {4 2 3}}") =error> "SetV->set: expects a set, got: #(struct:BoolV #t)")
(test (run "{if {equal? {1 2} {1 2}} then {1 2 3} else {1 2}}") => '(1 2 3))
(test (run "{if {fun {x y} x} then {1 2 3} else {1 2}}")  =error> "eval: not a bool")
#|
1. What are the types that we have now (after you are done) in the SOL 
language?-
  [SetV 
  [FunV 
  [BoolV
2. Explain where in the solution of section 2 (when parsing with expressions)
you called a function dynamically/statically – what was the importance of 
your choices?
i used static call because when using with ,user want to use the current named verint whith it current value and dont wnat it to change dynamicly.
3. Explain where in the solution of section 6 you used call-dynamic and 
where you used call-static – what was the importance of your choices?
when creating the pocedure i used dynamic call because selector (argument of the function) can by change as the user desair but once the procedure created i used static call because the use need immidietly the first/second elemnt in the procedure .
4. Would there be any difference if we used call-dynamic in some places in 
the following test? Explain your answer.
no because there is no tow variance whis the same name(exept S1 useless) .
|#