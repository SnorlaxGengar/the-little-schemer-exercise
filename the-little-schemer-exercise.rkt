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