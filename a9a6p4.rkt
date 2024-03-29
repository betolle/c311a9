#lang racket
;Add a ^ to each of the formal parameters of extend-env (not the inner function, mind you).
;Ensure that the inner functions in both extend-env and empty-env use the same formal parameters
;as the second argument to apply-env (here, typically y and k^).


(define empty-env
  (lambda ()
    (lambda (y)
      (error 'value-of "unbound identifier"))))
 
(define empty-k
  (lambda ()
    (lambda (v)
      v)))

(define apply-env
  (lambda (env y k)
    (apply-k k (extend-env env y)))) ;eenv

(define apply-closure
  (lambda (rato rando k)
    (rato rando k)
    )) 

(define apply-k
  (lambda (k e)
    (k e)))

(define extend-env
  (lambda (env^ v^)
    (lambda (y k)
      (if (zero? y) (apply-k k v^) (apply-env env^ (sub1 y) k)))))


(define value-of-cps
  (lambda (expr env k)
    (match expr
      [`(const ,expr) (apply-k k expr)]
      [`(var ,y) (apply-env env y k)] 
      [`(mult ,x1 ,x2) (value-of-cps x1 env (lambda (x1^) (value-of-cps x2 env (lambda (x2^) (apply-k k (* x1^ x2^))))))]
      [`(sub1 ,x)  (value-of-cps x env (lambda (sub1x) (apply-k k (sub1 sub1x))))]
      [`(zero ,x)  (value-of-cps x env (lambda (zerox) (apply-k k (zero? zerox))))]
      [`(if ,test ,conseq ,alt) (value-of-cps test env (lambda (v) (if v (value-of-cps conseq env k) (value-of-cps alt env k))))]
      [`(let ,e ,body) (value-of-cps e env (lambda  (a) (value-of-cps body (lambda (y) (if (zero? y) a (extend-env env (sub1 y)))) k)))];eenv
      [`(letcc ,body) (value-of-cps body (lambda (y) (if (zero? y) k (extend-env  env (sub1 y)))) k)];eenv
      [`(throw ,k-exp ,v-exp) (value-of-cps k-exp env (lambda (k-exp-v) (value-of-cps v-exp env (lambda (v-exp-v) (k-exp-v v-exp-v)))))]
      [`(lambda ,body) (lambda (a) (value-of-cps body (lambda (y) (if (zero? y) a (extend-env  env (sub1 y)))) k))] ;eenv
      [`(app ,rator ,rand) (value-of-cps rator env (lambda (rato) (value-of-cps rand env  (lambda (rando) (apply-closure rato rando k)))))])))
































(eqv? (value-of-cps '(const 7) (empty-env)(empty-k))
      7)

(eqv? (value-of-cps '(mult (const 7) (const 3)) (empty-env)(empty-k))
      21)
(eqv? (value-of-cps '(mult (const 7) (mult (const 3) (const 2))) (empty-env)(empty-k))
      42)

(eqv? (value-of-cps '(sub1 (const 7)) (empty-env)(empty-k))
      6)

(eqv? (value-of-cps '(zero (const 7)) (empty-env)(empty-k))
      #f)
(eqv? (value-of-cps '(zero (const 0)) (empty-env)(empty-k))
      #t)

(eqv? (value-of-cps '(if (zero (const 0)) (sub1 (const 2)) (const 9)) (empty-env) (empty-k))
      1)

(eqv? 
(value-of-cps '(if (zero (const 1)) (sub1 (const 2)) (sub1 (const 9))) (empty-env) (empty-k))
8)

(eqv?
(value-of-cps '(let (var 0)(const 5) ) (empty-env)(empty-k))
5)

(eqv? 
(value-of-cps '(letcc (mult (const 2) (const 5))) (empty-env)(empty-k))
10)

(eqv?
(value-of-cps '(letcc (const 2)) (empty-env)(empty-k))
2)

