#lang pl

#| Please complete the missing rules below  
<SOL> :: = { <NumList> }
        |  { scalar-mult <num> <SOL> }
        |  { intersect <-- fill in --> <SOL> <SOL>}
        |  { union <-- fill in --> <SOL> <SOL>} 
        |  <id>
        |  { with {<id> <SOL> } <SOL> } ;; this should be a syntactic sugar
       
<NumList> :: =  λ | {<num>} ;; where λ stands for the empty word, i.e., { } is the empty set
<num> ::= <DIGIT> | 
          <DIGIT> <num> | 
          {string-length <Str>} 
<DIGIT> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 
;; where <num> is any expression identified by Racket as a Number
;; and <id> is any expression such that Racket identifies '<id> as a symbol
 
|#


;; -----------------------------------------------------
;; The abstract syntax tree SOL
;; -----------------------------------------------------
;; The abstract syntax tree SOL
(define-type SET = (Listof Number))
(define-type SOL
  ;; Please complete the missing parts -- you are NOT allowed to use additional variants (constructors)
  ;; it took me 5 minutea
  ;;i took inspair from the lector on WAE
    [Set  SET]
    [Smult Number SOL ]
    [Inter SOL SOL]
    [Union SOL SOL]
    [IdS    Symbol]
    [WithS  Symbol SOL SOL])

;; ----------------------------------------------------
;; Operations on SETs
;; Please complete the missing parts, and add comments (comments should specify 
;; the role of each procedure, but also describe your work process). Keep your code readable. 

(: ismember? : Number SET  -> Boolean)
  ;; it took me 10 minutea
;;very easy
  ;;i took inspair from the lectore on recursive and lists
  ;;input-number and list ,output-is the list contains the number-
  ;;methode-iterete on all list and asck om every element if it equals to n
(define (ismember? n l)
  ( match l
     ['() #f]
     [(cons a b) (if (eq? n a) #t (ismember? n b))]  
      ))

(test (ismember? 1 '(3 4 5)) => #f)
(test (ismember? 1 '()) => #f)
(test (ismember? 1 '(1)) => #t)

(: reverse2 : SET  -> SET)
;;helper fuctionto reverse the set
;;i took it from tirful 3
(define (reverse2 lst)
  (if (null? lst) '()
    (append (reverse2 (cdr lst)) (list (car lst)))))

(: remove-duplicates2 : SET  -> SET)
  ;;helper function. it took me 5 minutea
  ;;i took inspair from the lectore on recursive and lists
  ;;input-  list ,output- list not  contains the dup
  ;;methode-iterete on all list and by  filter lambda expression remove all the presence of this number in the rest list 
(define (remove-duplicates2 lst)
  (cond
    [(eq? '() lst) '()]
    [else (cons (first lst) (remove-duplicates2 (filter (lambda (x) (not (equal? (first lst) x))) lst)))]))

(: remove-duplicates : SET  -> SET)
  ;; it took me 10 minutea
  ;;i took inspair from the lectore on recursive and lists
  ;;input-  list ,output- list not  contains the dup
  ;;methode-using the helper fun do the remve one the reverse list and the revers again
  ;;main defacult:understand the lambda method

(define (remove-duplicates lst)
  (reverse2 (remove-duplicates2 (reverse2 lst))))

(test (remove-duplicates '(3 4 5 1 3 4)) => '(5 1 3 4))
(test (remove-duplicates '(1)) => '(1))
(test (remove-duplicates '()) => '())


(: create-sorted-set : SET -> SET)
;; to create a list of numbers with the same
;; numbers
;; as alon sorted in descending order
  ;; it took me 20 minutea
  ;;i took inspair from the lectore on recursive and lists and internet
  ;;input-  list ,output- list sorted
  ;;methode-using the helper fun do the remve one the reverse list and the revers again
  ;;main defacult: creat the insert function

(define (create-sorted-set alon)
  (sort (remove-duplicates alon))
)

(: sort : SET -> SET)
  ;; it took me 20 minutes
  ;;i took inspair from the lectore on recursive and lists
  ;;input-  list ,output- list sorted
  ;;methode-using the insert iteret on all elements and mobve the smallest to the head of the list
(define (sort alon)
  (cond
    [(eq? '() alon) '()]
    [else (insert (first alon)
                        (create-sorted-set (rest alon)))]))

(: insert : Number (Listof Number) -> (Listof Number))
;; it took me 20 minutes
;; to create a list of numbers from n and alon that is
;; sorted in incressing order; alon is sorted
;;iteret on all elements and mobve the smallest to the head of the list
(define (insert n alon)
  (cond
    [(eq? '() alon) (cons n '())]
    [else (cond
            [(< n (first alon)) (cons n alon)]
            [else (cons (first alon)
    (insert n (rest alon)))])]))



(test (create-sorted-set '(3 4 5)) => '(3 4 5))
(test (create-sorted-set '( 3 2 3 5 6)) => '(2 3 5 6))
(test (create-sorted-set '()) => '())

(: set-union : SET SET -> SET)
 ;; it took me 30 minutes
  ;;i took inspair from the lectore on recursive and lists
  ;;input-  2 sets ,output- sorted list union
  ;;methode-using the helper iteret on all elements in one list and  chack if smaller then the corresponding element in the other list.
  ;;then remove-duplicates
  ;;main defacult: rhe recursive methode

(define (set-union a b)
  (remove-duplicates (set-union2 a b)))

(: set-union2 : SET SET -> SET)
 ;; it took me 20 minutes
  ;;i took inspair from the lectore on recursive and lists and internet
  ;;input-  2 sets ,output- sorted list union
  ;;methode-iteret on all elements in one list and  chack if smaller then the corresponding element in the other list.
  ;;then remove-duplicates
(define (set-union2 a b)
  (cond
    [(null? a) b]
    [(null? b) a]
    [(< (first a) (first b)) (cons (first a) (set-union2 (rest a) b))]
    [#t (cons (first b) (set-union2 a (rest b)))]))

(test (set-union '(3 4 5) '(3 4 5)) => '(3 4 5))
(test (set-union '(3 4 5) '()) => '(3 4 5))
(test (set-union '(3 4 5) '(1)) => '(1 3 4 5))

(: set-intersection : SET SET -> SET)
 ;; it took me 20 minutes
  ;;i took inspair from the lectore on recursive and lists and internet
  ;;input-  2 sets ,output-  lists intersection
  ;;methode-iteret on all elements in one list and using the helper function chack if it equals  the corresponding element in the other list.
   ;;main defacult: use the inner fnction

(define (set-intersection a b)
  
  (: mem-filter : Number -> Boolean)
  (define (mem-filter n)
    (ismember? n a))
  
   (if (null? a)
      '()
      (if (mem-filter (car b))
          (cons (car b) (set-intersection (cdr b) a))
          (set-intersection (cdr b) a))))

(test (set-intersection '(3 4 5) '(3 4 5)) => '(3 4 5))
(test (set-intersection '(3 4 5) '(3)) => '(3))
(test (set-intersection '(3 4 5) '(1)) => '())

(: set-smult : Number (Listof Number) -> SET)
 ;; it took me 5 minutes
  ;;very easy 
  ;;input-  scalar and sets ,output-  list set-smult by scalar
  ;;methode-iteret on all elements in one list and mult' it
 
(define (set-smult n l)
   ( match l
     ['() '()]
     [(cons a b) (cons (* a n) (set-smult n b))  ]  
      ))

(test (set-smult 3 '(3 4 5)) => '(9 12 15))
(test (set-smult 2 '()) => '())
(test (set-smult 0 '(3 4 5)) => '(0 0 0))

;; ---------------------------------------------------------
;; Parser
;; Please complete the missing parts, and add comments (comments should specify 
;; choices you make, and also describe your work process). Keep your code readable. 
(: parse-sexprS : Sexpr -> SOL)
;; to convert s-expressions into SOLs
 ;; it took me 30 minutes
  ;;i took inspair from the lectore on parsing
  ;;input-  Sexpr ,output-  SOL
  ;;main defacult: drop correct rerrrors

(define (parse-sexprS sexpr)
  (match sexpr
    [(list (number: ns) ...) (Set  ns) ] 
    [(symbol: name) (IdS name)]
    [(cons 'with more)
     (match sexpr
     		[(list 'with (list (symbol: oldName) newName) body)
     			 (WithS oldName (parse-sexprS newName ) (parse-sexprS body ))]
          [else (error 'parse-sexprS "bad `with' syntax in ~s" sexpr)])]
    [(list 'scalar-mult (number: sc) rhs) (Smult sc (parse-sexprS rhs))]
    [(list 'intersect lhs rhs) (Inter (parse-sexprS lhs) (parse-sexprS rhs)) ]
    [(list 'union lhs rhs)(Union (parse-sexprS lhs) (parse-sexprS rhs))]
    [else (error 'parse-sexprS "bad syntax in ~s" sexpr)]))


  
(: parseS : String -> SOL)
;; parses a string containing a SOL expression to a SOL AST
(define (parseS str)
  (parse-sexprS (string->sexpr str)))

(test (parseS "{1 3  4 1 4  4 2 3 4 1 2 3}") => (Set '(1 3 4 1 4 4 2 3 4 1 2 3)))
(test (parseS "{union {1 2 3} {4 2 3}}") => (Union (Set '(1 2 3)) (Set '(4 2 3))))
(test (parseS "{intersect {1 2 3} {4 2 3}}") => (Inter (Set '(1 2 3)) (Set '(4 2 3))))
(test (parseS "{with {S {intersect {1 2 3} {4 2 3}}}
                    {union S {12}}}")
      => (WithS 'S (Inter (Set '(1 2 3)) (Set '(4 2 3))) (Union (IdS 'S) (Set '(12))) ))
(test (parseS "{with S {intersect {1 2 3} {4 2 3}}
                 {union S S}}")
      =error> "parse-sexprS: bad `with' syntax in")
(test (parseS "{wit}")
      =error> "parse-sexprS: bad syntax in ")

;;-----------------------------------------------------
;; Substation 
#|
------------------------------------------------------
 Formal specs for `subst':
   (`Set' is a <NumList>, E, E1, E2 are <SOL>s, `x' is some <id>,
   `y' is a *different* <id>)
      Set[v/x]              = Set
      {smult n E}[v/x]      = {smult n E[v/x]}
      {inter E1 E2}[v/x]    = {inter E1[v/x] E2[v/x]}
      {union E1 E2}[v/x]    = {union E1[v/x] E2[v/x]}
      y[v/x]                = y
      x[v/x]                = v
      {with {y E1} E2}[v/x] = {with {y E1[v/x]} E2[v/x]}
      {with {x E1} E2}[v/x] = {with {x E1[v/x]} E2}
|#

(: substS : SOL Symbol SOL -> SOL)
 ;; it took me 20 minutes
  ;;i took inspair from the lectore on Substation
  ;;input-  SOL Symbol SOL ,output-  SOL
 ;;substSution of not free instanses
  ;;main defacult: withS

(define (substS expr from to)
  (cases expr
    [(Set n) expr]
    [(Smult n s) (Smult n (substS s from to))]
    [(Inter l r) (Inter (substS l from to) (substS r from to) )];;
    [(Union l r) (Union(substS l from to) (substS r from to))]
    [(IdS name) (if (eq? name from) to expr)]
    [(WithS bound-id named-expr bound-body)
     (WithS bound-id
             (substS named-expr from to)
             (if (eq? bound-id from)
               bound-body ;;
               (substS bound-body from to)))]))

;;-----------------------------------------------------
;; Evaluation 
#|
------------------------------------------------------
Evaluation rules:
    ;; Please complete the missing parts in the formal specifications below

    eval({ N1 N2 ... Nl })  =  (sort (create-set (N1 N2 ... Nl)))
                               where create-set removes all duplications from
                              the sequence (list) and sort is a sorting procedure
    eval({scalar-mult K E}) =  (K*N1 K*N2 ... K*Nl) if (N1 N2 ... Nl) = eval(E,) is a sorted set AND
                                = error! otherwise (if S is not a sorted set)
    eval({intersect E1 E2}) = (sort (create-set (set-intersection (eval(E1,) , eval(E2,))))
                                    if both E1 and E2 evaluate to sorted sets
                                = error! otherwise
    eval({union E1 E2}) = (sort (create-set (eval(E1,) , eval(E2,))))
                                  if both E1 and E2 evaluate to sorted sets
                             = error! otherwise
    eval({with {x E1} E2}) = eval(E2,extend(x,eval(E1,),)) 
|#



;;---------  the eval procedure ------------------------------
;; Please complete the missing parts, and add comments (comments should specify 
;; the choices you make, and also describe your work process). Keep your code readable. 
(: eval : SOL -> SET)
;; evaluates SOL expressions by reducing them to set values
 ;; it took me 20 minutes
  ;;i took inspair from the lectore on Evaluation
  ;;input-  SOL  ,output-  SOL
  ;;main defacult: withS
(define (eval expr )
  (cases expr
    [(Set S)         (create-sorted-set (remove-duplicates S))]  ;; sort and remove-duplicates
    [(Smult n set)   (set-smult n (eval set))]            
    [(Inter l r)     (create-sorted-set (set-intersection (eval l ) (eval r)))]   
    [(Union l r)     (create-sorted-set (set-union (eval l ) (eval r)))]          
    [(WithS b-name named b-body) (eval (substS b-body b-name (Set (eval named))))]    ;;evaluat only not free identifier  symbol is impasible!  
    [(IdS name)      (error 'eval "free identifier: ~s" name)]))             ;;evaluat a not recognazed symbol is impasible!      

(: run : String -> SET)
;; evaluate a SOL program contained in a string
(define (run str)
  (eval (parseS str)))
    
(test (run "{1 2 3  4 1 4  4 2 3 4 1 2 3}") => '(1 2 3 4))
(test (run "{union {1 2 3} {4 2 3}}") => '(1 2 3 4))
(test (run "{intersect {1 2 3} {4 2 3}}") => '(2 3))
(test (run "{with {S {intersect {1 2 3} {4 2 3}}}
                 {with {x {4 5 7 6 9 8 8 8}}
                    {union x S}}}")
      => '(2 3 4 5 6 7 8 9))
(test (run "{with {S {union {1 2 3} {4 2 3}}}
                 {with {x {4 5 7 6 9 8 8 8}}
                    {intersect x S}}}")
      => '(4))
(test (run "{with {x {intersect {1 2 3} {4 2 3}}}
                 {with {x {4 5 7 6 9 8 8 8}}
                    {union x x}}}")
      => '(4 5 6 7 8 9))
(test (run "{with {S {intersect {1 2 3} {4 2 3}}}
              {union {scalar-mult 3 B}
                 {4 5 7 9 8 8 8}}}")
      =error> "eval: free identifier:")
(test (run "{with {S {union {1 2 3} {4 2 3}}}
              {intersect {scalar-mult 3 B}
                 {4 5 7 9 8 8 8}}}")
      =error> "eval: free identifier:")

;;-----------------------------------------------------

#| BNF for the WAE language:
 <WAE> ::= <num>
 | { + <WAE> <WAE> }
 | { - <WAE> <WAE> }
 | { * <WAE> <WAE> }
 | { / <WAE> <WAE> }
 | { with { <id> <WAE> } <WAE> }
 | <id>
 |#
 ;; WAE abstract syntax trees
 (define-type WAE
 [Num Number]
 [Add WAE WAE]
 [Sub WAE WAE]
 [Mul WAE WAE]
 [Div WAE WAE]
 [Id Symbol]
 [With Symbol WAE WAE])

 (: parse-sexprw : Sexpr -> WAE)
 ;; to convert s-expressions into WAEs
 (define (parse-sexprw sexpr)
 (match sexpr
 [(number: n) (Num n)]
 [(symbol: name) (Id name)]
 [(cons 'with more)
 (match sexpr
 [(list 'with (list (symbol: name) named) body)
 (With name (parse-sexprw named) (parse-sexprw body))]
 [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])]
 [(list '+ lhs rhs) (Add (parse-sexprw lhs) (parse-sexprw rhs))]
 [(list '- lhs rhs) (Sub (parse-sexprw lhs) (parse-sexprw rhs))]
 [(list '* lhs rhs) (Mul (parse-sexprw lhs) (parse-sexprw rhs))]
 [(list '/ lhs rhs) (Div (parse-sexprw lhs) (parse-sexprw rhs))]
 [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))
 (: parsew : String -> WAE)
 ;; parses a string containing a WAE expression to a WAE AST
 (define (parsew str)
 (parse-sexprw (string->sexpr str)))
 #| Formal specs for `subst':
 (`N' is a <num>, `E1', `E2' are <WAE>s, `x' is some <id>, `y' is a
 *different* <id>)
 N[v/x] = N
 {+ E1 E2}[v/x] = {+ E1[v/x] E2[v/x]}
 {- E1 E2}[v/x] = {- E1[v/x] E2[v/x]}
 {* E1 E2}[v/x] = {* E1[v/x] E2[v/x]}
 {/ E1 E2}[v/x] = {/ E1[v/x] E2[v/x]}
 y[v/x] = y
 x[v/x] = v
 {with {y E1} E2}[v/x] = {with {y E1[v/x]} E2[v/x]}
 {with {x E1} E2}[v/x] = {with {x E1[v/x]} E2}
 |#
 (: substw : WAE Symbol WAE -> WAE)
 ;; substitutes the second argument with the third argument in the
 ;; first argument, as per the rules of substitution; the resulting
 ;; expression contains no free instances of the second argument
 (define (substw expr from to)
 (cases expr
 [(Num n) expr]
 [(Add l r) (Add (substw l from to) (substw r from to))]
 [(Sub l r) (Sub (substw l from to) (substw r from to))]
 [(Mul l r) (Mul (substw l from to) (substw r from to))]
 [(Div l r) (Div (substw l from to) (substw r from to))]
 [(Id name) (if (eq? name from) to expr)]
 [(With bound-id named-expr bound-body)
 (With bound-id
 (substw named-expr from to)
 (if (eq? bound-id from)
 bound-body
 (substw bound-body from to)))]))
 #| Formal specs for `eval':
 eval(N) = N
 eval({+ E1 E2}) = eval(E1) + eval(E2)
 eval({- E1 E2}) = eval(E1) - eval(E2)
 eval({* E1 E2}) = eval(E1) * eval(E2)
 eval({/ E1 E2}) = eval(E1) / eval(E2)
 eval(id) = error!
 eval({with {x E1} E2}) = eval(E2[eval(E1)/x])
 |#
 (: evalw : WAE -> Number)
 ;; evaluates WAE expressions by reducing them to numbers
 (define (evalw expr)
 (cases expr
 [(Num n) n]
 [(Add l r) (+ (evalw l) (evalw r))]
 [(Sub l r) (- (evalw l) (evalw r))]
 [(Mul l r) (* (evalw l) (evalw r))]
 [(Div l r) (/ (evalw l) (evalw r))]
 [(With bound-id named-expr bound-body)
 (evalw (substw bound-body
 bound-id
(Num (evalw named-expr))))]
 [(Id name) (error 'eval "free identifier: ~s" name)]))

 (: runw : String -> Number)
 ;; evaluate a WAE program contained in a string
 (define (runw str)
 (evalw (parsew str)))

(: remove-duplicates22 : (Listof Symbol)  -> (Listof Symbol))

(define (remove-duplicates22 lst)
  (cond
    [(eq? '() lst) '()]
    [else (cons (first lst) (remove-duplicates22 (filter (lambda (x) (not (equal? (first lst) x))) lst)))]))

(: freeInstanceList : WAE  -> (Listof Symbol))
(define(freeInstanceList expr)
  (remove-duplicates22 (countfree expr))
)




(: countfree : WAE -> (Listof Symbol)) 
(define(countfree expr)
  (cases expr
    [(Num n) '()]
    [(Add l r) (append (countfree l ) (countfree r ))]
    [(Sub l r) (append (countfree l ) (countfree r ))]
    [(Mul l r) (append (countfree l ) (countfree r ))]
    [(Div l r) (append (countfree l ) (countfree r ))]
    [(Id name)
          (list name) ]
    [(With b-id named b-body)
                (append (countfree named)
                                 (countfree (substw
                                               b-body
                                               b-id
                                               (Num 0)) ))]))

(test (freeInstanceList (parsew "w")) => '(w))
(test (freeInstanceList (parsew "{with {xxx 2} {with {yyy 3} {+ 
{- xx y} z}}}")) => '(xx y z))
(test (freeInstanceList (With 'x (Num 2) (Add (Id 'x) (Num 
3)))) => '())
(test (freeInstanceList (With 'x (Num 2) (Mul (Id 'x) (Num 
3)))) => '()) 
(test (freeInstanceList (parsew "{+ z {+ x z}}")) => '(z x))
(test (freeInstanceList (parsew "{/ z {/ x z}}")) => '(z x))
(test (freeInstanceList (parsew "{+ z {+ x z}}")) => '(z x))


