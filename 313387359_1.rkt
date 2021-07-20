#lang pl

(require rackunit)


(: append5 : Char Char Char Char Char -> String)
;consumes 5 characters and returns the
;concatenation of them.
;use the string function in lib.
;i realzed it by looking at doc
;i spend 45 minute on it

(define (append5 A p1 a b c )
  (string A p1 a b c))

(test(append5 #\A #\0 #\A #\2 #\A  ) => "A0A2A")

(test(append5 #\A #\p #\A #\p #\A  ) => "ApApA")
;(test(append5 #\A #\p #\A #\p 4  ) =error> " ")
;(test(append5 #\A #\p ) =error> " ")
;(test(append5 "" ) =error> " ")

;----------------------------------------------



(: permute3 : Char Char Char -> (Listof String ))
;consumes 3 characters and returns a list of 
;strings the concatenation of them in any possible ordering.
;by match func i produced all the posible comination of the 3.
;i realized it by the lesson on match
;i spend 50 minute on it

(define (permute3 a b c)
  (cond [(or (null? a) (null? b) (null? c)) (error "no char ~s")]
        [else (list (string a b c) (string a c b) (string b a c) (string b c a) (string c a b ) (string c b a))]))

(test (permute3 #\a #\b #\c) => '("abc" "acb" "bac" "bca" "cab" "cba"))


;--------------------------------------------------



(: length3? : (Listof Any) -> Natural)
;check list is length3?
(define (length3? lst)
  (match lst
    ['()   0]
    [(list h t a)   1]
    [(list h t)   0]
    [(list h )   0]
    [(list h t y i...)   0]
    ))

(define x 0)

(: count-3lists : (Listof Any) -> Natural)
;consumes a list of lists (where the type of the elements in the inner 
;lists may be any type) and returns the number of inner lists (within the 
;wrapping list) that contain exactly 3 elements.
;,i  run recursivly on the list and ask helper func wether the list length is 3.i implement this helper function by using match.
;for this function i did alot of debuging work
;i spend 50 minute on it

(define (count-3lists L)
  (match L 
    ['()   0]
    [(cons h t)
     (if(list? h)
        (+ (length3? h) (count-3lists t))
        (count-3lists t))]
    [e 0]
    ))
  
(test(count-3lists '(5 (7) )) => 0)
(test(count-3lists '( )) => 0)
(test(count-3lists '((7 6 5) (6 5) ("as" 2 1) () (3 2 1) )) => 3)
(test(count-3lists '((7 6 5) ( ()6 5) () )) => 2)
(test (count-3lists '((1 3 4) (() (1 2 3)) ("tt" "Three" 7) (2 4 6 8) (1 2 3))) => 3)

;----------------------------------------------





  
 (: count-3lists-tail  : (Listof Any) -> Natural)
(define (count-3lists-tail   L)
;consumes a list of lists (where the type of the elements in the inner 
;lists may be any type) and returns the number of inner lists (within the 
;wrapping list) that contain exactly 3 elements.
;,i  run recursivly on the list and ask helper func wether the list length is 3 but now i ised inner func so that when calling recursivly the inner function,it not wait for the next result of the recursive finction.this calls tail recursive.
;.i implement this helper function by using match.
;for this function i used the code in the tirgul of netanel
  ;i spend 50 minute on it


 (: helper : Natural (Listof Any) -> Natural)
(define (helper acc L)
  (match L 
 ['()   acc]
 [(cons h t)
       (if  (list? h)
          (helper (+ (length3? h) acc) (rest L))
          (helper acc (rest L)))]
 [e 0]
  
  ))
  (helper 0 L)
  )  
  

(test(count-3lists-tail  '(5 (7) )) => 0)
(test(count-3lists-tail  '( )) => 0)
(test(count-3lists-tail  '((7 6 5) (6 5) ("as" 2 1) () (3 2 1) )) => 3)
(test(count-3lists-tail  '((7 6 5) ( ()6 5) () )) => 2)
(test (count-3lists-tail  '((1 3 4) (() (1 2 3)) ("tt" "Three" 7) (2 4 6 8) (1 2 3))) => 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(: count-3listsRec : (Listof Any) -> Natural)
 ;consumes a list of lists (where the type of the elements in the inner 
;lists may be any type) and returns the number of inner lists (within the 
;wrapping list) that contain exactly 3 elements. however counts the number of lists of length 3 
;recursively
;,i  run recursivly on the list ,flat it,and ask helper func wether the list length is 3.i implement this helper function by using match.
;for this function i did alot of debuging work
;i spend 50 minute on it

(define (count-3listsRec L)
  (match L 
    ['()   0]
    [(cons h t)
     (if(list? h)
        (+ (length3? (flat h)) (count-3listsRec t))
        (count-3listsRec t))]
    [e 0]
    ))

(: flat : (Listof Any) ->(Listof Any))
(define (flat lst)
  (match lst
    ['()   null]
    [(cons h t)
     (cond
       [(and (list? h) (list? t)) (append (flat h ) (flat t ))]
       [(list? h) (list (flat h ) t )]
       [(list? t) (append (list h) (flat t ) )]
       [else (list h t )])]

    ))

;(test(flat '((7 6 5) (6 5) ())) => '(7 6 5 6 5 ))
(test (count-3listsRec '((1 3 4) (() (1 2 3)) ("tt" "Three" 7) (2 4 6 8) (1 2 3))) => 4)
(test(count-3listsRec '(5 (7) )) => 0)
(test(count-3listsRec '( )) => 0)
(test(count-3listsRec '(() () () )) => 0)
(test(count-3listsRec '((7 6 5) (6 5) ("as" 2 1) () (3 2 1) )) => 3)
(test(count-3listsRec '((7 6 5) ( () 6 5) () )) => 1)
(test (count-3listsRec '((1 3 4) (() (1 2 3)) ("tt" "Three" 7) (2 4 6 8) (1 2 3))) => 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-type KeyStack     ; constructor
 [EmptyKS]    
 [push Symbol String KeyStack])




(: Push : Symbol String KeyStack -> KeyStack )
;take as input a symbol (key), a 
;string (value), and an existing keyed-stack and return an extended key-
;stack in the natural way.
;i used cases func to biuld the return keystack
;i realized it by the given tirgul
;i spend 50 minute on it

(define  (Push val key stk)
  (cases stk
    [(EmptyKS) (push 'b "s" (EmptyKS))]
    [(push e s g) (cond
                     [(and (symbol? val) (string? key)) (push val key stk)]) ]) 
  )

(: pop-stack : KeyStack -> Any )
;take as input a symbol (key) and a keyed-stack and return the first 
;(LIFO, last in first out) value that is keyed accordingly.
; i used cases to chack if the first part of the stack consist this symbol .if yes-return the rest.else look dor it in the rest list
;i spend 50 minute on it

(define  (pop-stack stk)
  (cases stk
    [(EmptyKS) #f]
    [(push e s g)  (cases g
                      [(EmptyKS) (EmptyKS)]
                      [(push a b c) g]
     )] 
  ))


(: search-stack : Symbol KeyStack -> Any )
;take as input a keyed-stack and return the keyed-stack without its first (keyed) 
;value .
;i used cases as teached in class to hendle caces of empty  stack and non empty stack
;i spend 55 minute on it

(define  (search-stack sim stk)
  (cases stk
    [(EmptyKS) #f]
    [(push e s g)  (if(eq? sim e) s
                       (cases g
                      [(EmptyKS) #f]
                      [(push a b c) (search-stack sim c)]
     ))] 
  ))

(test (EmptyKS) => (EmptyKS))
(test (Push 'b "B" (EmptyKS)) =>  (Push 'b "B" (EmptyKS)))
(test (Push 'b "B" (Push 'a "A" (EmptyKS))) =>  (Push 'b "B" (Push 'a "A" (EmptyKS))))
(test (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS)))) => (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS)))))
(test (search-stack 'a (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => "AAA")
(test (search-stack 'c (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => #f)

(test (search-stack 'c (EmptyKS)) => #f)
(test (pop-stack (Push 'a "AAA" (Push 'b "B" (Push 'a "A" 
(EmptyKS))))) => (Push 'b "B" (Push 'a "A" (EmptyKS))))

(test (pop-stack (EmptyKS)) => #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(: is-odd? : Natural -> Boolean)
;purpose-check wether a given num is odd.
;how-by given input-ask if zero ,means odd so false.if not,it start a ping-pong  game whith a helper func so if the input  is odd,by decreasing it by one and sending it to the iseven? func it will eventualy return true,otherwise return false.
;i realize  it by debug the code with even and odd number.
;i spend 10 minute on it
(define (is-odd? x)
 (if (zero? x)
 false
 (is-even? (- x 1))))
(: is-even? : Natural -> Boolean)
;purpose-check wether a given num is even.
;how-by given input-ask if zero ,means even so true.if not,it start a ping-pong  game with a helper func so if the input  is even,by decreasing it by one and sending it to the isodd? func it will eventualy return true,otherwise return false.
;i realize  it by debug the code with even and odd number.
;i spend 10 minute on it

(define (is-even? x)
 (if (zero? x)
 true
 (is-odd? (- x 1))))
;; tests --- is-odd?/is-even?
(test (not (is-odd? 12)))
(test (is-even? 12))
(test (not (is-odd? 0)))
(test (is-even? 0))
(test (is-odd? 1))
(test (not (is-even? 1)))
(: every? : (All (A) (A -> Boolean) (Listof A) -> Boolean))
;purpose-check wether each of the elements in the given list is setisfy the given func(func returns true on it).
;how-check -if null each element in the empty list  trivialy setisfy the func.otherwise it check that also the first element and the rest of the list (recursivly ) setisfy the func
;i realize  it by debug the code with lists of even and odd number.
;i spend 10 minute on it

(define (every? pred lst)
 (or (null? lst)
 (and (pred (first lst))
 (every? pred (rest lst)))))
;; An example for the usefulness of this polymorphic function
(: all-even? : (Listof Natural) -> Boolean)
;purpose-check wether each of the elements in the given list is even.
;how-send th given list with the iseven? func to the every?  function
;i realized it simply by looking at the code
;i spend 10 minute on it

(define (all-even? lst)
 (every? is-even? lst))
;; tests
(test (all-even? null))
(test (all-even? (list 0)))(test (all-even? (list 2 4 6 8)))
(test (not (all-even? (list 1 3 5 7))))
(test (not (all-even? (list 1))))
(test (not (all-even? (list 2 4 1 6))))
(: every2? : (All (A B) (A -> Boolean) (B -> Boolean) (Listof A) (Listof B) -> 
Boolean))
;purpose-check wether each of the elements in the given  first list is setisfy the given first func(func returns true on it).
;also,check wether each of the elements in the given  second  list is setisfy the given second func(func returns true on it).
;how-check -if first list is null each element in the empty list  trivialy setisfy the func.both lists assumed to be of same length so if the first list is null so the second too.
;otherwise it check that also the first element and the rest of the first list (recursivly ) setisfy the first func, and the first element and the rest of the second  list (recursivly ) setisfy the second func recursivly
;i realized it simply by looking at the code
;i spend 2 minute on it

(define (every2? pred1 pred2 lst1 lst2)
 (or (null? lst1) ;; both lists assumed to be of same length
 (and (pred1 (first lst1))
 (pred2 (first lst2))
 (every2? pred1 pred2 (rest lst1) (rest lst2)))))