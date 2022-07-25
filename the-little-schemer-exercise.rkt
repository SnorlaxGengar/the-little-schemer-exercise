#lang sicp
; https://docs.racket-lang.org/sicp-manual/Installation.html
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))
(lat? '(Jack Sprat could eat no chicken fat))
(lat? '((Jack) Sprat could eat no chicken fat))
(lat? '())

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      ((eq? a (car lat)) #t)
      (else (member? a (cdr lat))))))
(member? 'tea '(coffee tea or milk))
(member? 'poached '(fried eggs and scrambled eggs))

(define rember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? a (car lat)) (cdr lat))
      (else (cons (car lat) (rember a (cdr lat)))))))
(rember 'mint '(lamb chops and mint jelly))
(rember 'mint '(lamb chops and mint flavored mint jelly))
(rember 'toast '(bacon lettuce and tomato))
(rember 'cup '(coffee cup tea cup and hick cup))

(define firsts
  (lambda (l)
  (cond
    ((null? l) '())
    (else (cons (car (car l)) (firsts (cdr l)))))))
(firsts '((apple peach pumpkin)
          (plum pear cherry)
          (grape raisin pea)
          (bean carrot eggplant)))
(firsts '((a b) (c d) (e f)))
(firsts '())

(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old)
       ; (cons old (cons new (cdr lat))))
       (cons old lat))
      (else (cons (car lat) (insertR new old (cdr lat)))))))
(insertR 'topping 'fudge '(ice cream with fudge for dessert))
(insertR 'jalapeno 'and '(tacos tamales and salsa))
(insertR 'e 'd '(a b c d f g d h))

(define insertL
  (lambda (new old lat)
    (cond
      ((eq? (car lat) old)
       (cons new (cons old (cdr lat))))
      (else (cons (car lat) (insertL new old (cdr lat)))))))
(insertL 'topping 'fudge '(ice cream with fudge for dessert))

(define subst
  (lambda (new old lat)
    (cond
      ((eq? (car lat) old)
       (cons new (cdr lat)))
      (else (cons (car lat) (subst new old (cdr lat)))))))
(subst 'topping 'fudge '(ice cream with fudge for dessert))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((or (eq? (car lat) o1) (eq? (car lat) o2))
       (cons new (cdr lat)))
      (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))
(subst2 'vanilla 'chocolate 'banana '(banana ice cream with chocolate topping))

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? a (car lat)) (multirember a (cdr lat)))
      (else (cons (car lat) (multirember a (cdr lat)))))))
(multirember 'cup '(coffee cup tea cup and hick cup))

(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat))
       (cons old (cons new (multiinsertR new old (cdr lat)))))
      (else (cons (car lat) (multiinsertR new old (cdr lat)))))))
(multiinsertR 'topping 'fudge '(fudge ice cream with fudge for dessert on topping))

(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat))
       (cons new (cons old (multiinsertL new old (cdr lat)))))
      (else (cons (car lat) (multiinsertL new old (cdr lat)))))))
(multiinsertL 'topping 'fudge '(fudge ice cream with fudge for dessert on topping))

(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat))
       (cons new (multisubst new old (cdr lat))))
      (else (cons (car lat) (multisubst new old (cdr lat)))))))
(multisubst 'javascript 'c++ '(c++ programmer write program with c++))

(define add1
  (lambda (n)
    (+ n 1)))
(add1 67)

(define sub1
  (lambda (n)
    (- n 1)))
(sub1 5)
(sub1 0)

(zero? 0)

(define plus
  (lambda (x y)
    (cond
     ((zero? y) x)
     ; (else (plus (add1 x) (sub1 y))))))
     (else (add1 (plus x (sub1 y)))))))
(plus 46 12)

(define minus
  (lambda (x y)
    (cond
      ((zero? y) x)
      (else (sub1 (minus x (sub1 y)))))))
(minus 14 3)
(minus 17 9)
(minus 18 25)

(define tup?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((number? (car l)) (tup? (cdr l)))
      (else #f))))
(tup? '(2 11 3 79 47 6))
(tup? '(8 55 5 555))
(tup? '(1 2 8 apple 4 3))
(tup? '(3 (7 4) 13 9))
(tup? '())

(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (plus (car tup) (addtup (cdr tup)))))))
(addtup '(3 5 2 8))
(addtup '(15 6 7 12 3))

(define multi
  (lambda (x y)
    (cond
      ((zero? y) 0)
      (else (plus x (multi x (sub1 y)))))))
(multi 5 3)
(multi 13 4)

(define tup+
  (lambda (tup1 tup2)
    (cond
      ((and (null? tup1) (null? tup2)) '())
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else (cons (plus (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))
(tup+ '(3 6 9 11 4) '(8 5 2 0 7))
(tup+ '(2 3) '(4 6))
(tup+ '(3 7) '(8 5 2 0 7))
(tup+ '() '(8 5 2 0 7))

(define greater
  (lambda (x y)
    (cond
      ((zero? x) #f)
      ((zero? y) #t)
      (else (greater (sub1 x) (sub1 y))))))
(greater 0 0)
(greater 0 133)
(greater 12 0)
(greater 12 133)
(greater 120 11)

(define less?
  (lambda (x y)
    (cond
      ((zero? y) #f)
      ((zero? x) #t)
      (else (less? (sub1 x) (sub1 y))))))
(less? 0 0)
(less? 0 1)
(less? 1 0)
(less? 1 2)
(less? 2 1)

(define equ?
  (lambda (x y)
    (cond
      ((zero? x) (zero? y))
      ((zero? y) (zero? x))
      (else (equ? (sub1 x) (sub1 y))))))
(equ? 0 0)
(equ? 1 1)
(equ? 0 1)
(equ? 1 2)

(define power
  (lambda (x y)
    (cond
      ((zero? y) 1)
      (else (multi x (power x (sub1 y)))))))
(power 1 1)
(power 2 3)
(power 5 3)

(define division
  (lambda (x y)
    (cond
      ((less? x y) 0)
      (else (add1 (division (minus x y) y))))))
(division 16 5)
(division 21 5)

(define len
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (len (cdr lat)))))))
(len '(hotdogs with mustard sauerkraut and pickles))
(len '(ham and cheese on rye))

(define pick
  (lambda (n lat)
    (cond
      ((zero? n) '())
      (else (cons (car lat) (pick (sub1 n) (cdr lat)))))))
(pick 4 '(lasagna spaghetti ravioli macaroni meatball))
(pick 0 '(a))

(define rempick
  (lambda (n lat)
    (cond
      ((equ? n 1) (cdr lat))
      (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))
(rempick 3 '(hotdogs with hot mustard))

(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (no-nums (cdr lat)))
      (else (cons (car lat) (no-nums (cdr lat)))))))
(no-nums '(5 pears 6 prunes 9 dates))

(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
      (else (all-nums (cdr lat))))))
(all-nums '(5 pears 6 prunes 9 dates))

(define eqan?
  (lambda (a1 a2)
    (cond
      ((null? a1) (null? a2))
      ((null? a2) (null? a1))
      ((and (number? (car a1)) (number? (car a2)))
       (and (equ? (car a1) (car a2)) (eqan? (cdr a1) (cdr a2))))
      (else
       (and (eq? (car a1) (car a2)) (eqan? (cdr a1) (cdr a2)))
       ))))
(eqan? '(1 a b 4) '(1 a b 4))
(eqan? '(1 a b 4) '(2 a b 4))
(eqan? '(1 a b 4) '(1 b b 4))
(eqan? '(1 a) '(1 a b 4))
(eqan? '(1 a b 4) '(1 a b))

(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      ((eq? a (car lat)) (add1 (occur a (cdr lat))))
      (else (occur a (cdr lat))))))
(occur 'a '(a b c a b c))
(occur '1 '(1 b 1 a 1 c))

(define one?
  (lambda (n)
    (eq? 1 n)))
(one? 1)
(one? 111)
(one? 'abc)

(define rempick1
  (lambda (n lat)
    (cond
      ((one? n) (cdr lat))
      (else (cons (car lat) (rempick1 (sub1 n) (cdr lat)))))))
(rempick1 3 '(lemon meringue salty pie))