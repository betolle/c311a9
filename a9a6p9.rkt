#lang racket
;Define constructors for your continuations, and replace your explicitly higher-order function continuations with calls to these constructors.
;For nested continuations, you should consider working from the inside out.



(define make-inner-mult
  (lambda (x1^ k)
    (lambda (x2^) (apply-k k (* x1^ x2^)))))

(define make-outer-mult
  (lambda  (x2 env k)
    (lambda (x1^) (value-of-cps x2 env (make-inner-mult x1^ k)))))

(define make-inner-sub1
  (lambda (k)
    (lambda (sub1x) (apply-k k (sub1 sub1x)))))

(define make-inner-zero 
  (lambda (k)
    (lambda (zerox) (apply-k k (zero? zerox)))))
  
(define make-inner-if
  (lambda (conseq alt env k)
    (lambda (v) (if v (value-of-cps conseq env k)
                      (value-of-cps alt env k)))))

(define make-inner-let
  (lambda (body env k)
    (lambda (e-val) (value-of-cps body (extend-env env e-val) k))))

(define make-inner-throw
  (lambda(k-exp-v)
    (lambda (v-exp-v)
    (k-exp-v v-exp-v))))

(define make-outer-throw
  (lambda (v-exp env)
    (lambda (k-exp-v) (value-of-cps v-exp env (make-inner-throw k-exp-v)))))

(define make-inner-app
  (lambda (clos k)
    (lambda (a) (apply-closure clos a k))))

(define make-outer
  (lambda (rand env k)
    (lambda (clos) (value-of-cps rand (extend-env env (make-inner-app clos k)) k)))) 
    
(define empty-env
  (lambda ()
    `(empty-env)))
 
(define empty-k
  (lambda ()
    (lambda (v)
      v)))

(define apply-env
  (lambda (env y k^)
    (match env
      (`(empty-env) (error 'value-of "unbound identifier"))
      (`(extend-env ,env^ ,v^) (if (zero? y) (apply-k k^ v^) (apply-env env^ (sub1 y) k^)))))) 

(define extend-env
  (lambda (env^ v^)
    `(extend-env ,env^ ,v^)))

(define make-closure
  (lambda (body env)
    `(make-closure ,body ,env)))

(define apply-closure
  (lambda (clos a k)
    (match clos
      (`(make-closure ,body ,env) (value-of-cps body (extend-env env a) k)))))

(define apply-k
  (lambda (k v)
    (k v)))


(define value-of-cps
  (lambda (expr env k)
    (match expr
      [`(const ,expr) (apply-k k expr)]
      [`(var ,y) (apply-env env y k)] 
      [`(mult ,x1 ,x2) (value-of-cps x1 env (make-outer-mult x2 env k))]
      [`(sub1 ,x)  (value-of-cps x env (make-inner-sub1 k))]
      [`(zero ,x)  (value-of-cps x env (make-inner-zero k))] 
      [`(if ,test ,conseq ,alt) (value-of-cps test env (make-inner-if  conseq alt env k))]
      [`(let ,e ,body) (value-of-cps e env (make-inner-let body env k))]
      ;fixed errors (ignore if you do not care
      ;just found error !X!. extending env w recur call doesnt make sense.
      ;also just realized there is not a let case in lex. Which is probably why test wasn't actually testing 
      ;[`(let ,e ,body) (value-of-cps e (!X! extend-env env (value-of-cps body (make-inner-let env k)(lambda (y) (extend-env env y)) k)) k)]
      ;another  error found here   -->        V     (cant still have lambdas  (guessing to pass continuation to env bc idk what other value is avaliable
      ;[`(letcc ,body) (value-of-cps body (lambda (y) (extend-env env y)) k)]               ^ worked so guessing the guess was correct
      [`(letcc ,body) (value-of-cps body (extend-env env k) k)] 
      [`(throw ,k-exp ,v-exp) (value-of-cps k-exp env (make-outer-throw v-exp env))] 
      [`(lambda ,body) (apply-k k (make-closure body env))]
      [`(app ,rator ,rand) (value-of-cps rator env (make-outer rand env k))])))
































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
(value-of-cps '(let (const 5) (var 0)) (empty-env)(empty-k))
5)

(eqv? 
(value-of-cps '(letcc (mult (const 2) (const 5))) (empty-env)(empty-k))
10)

(eqv?
(value-of-cps '(letcc (const 2)) (empty-env)(empty-k))
2)

(eqv?
(value-of-cps '(app (if (zero (const 1)) (lambda (var 0) (const 0)) (lambda (var 1))) (const 3)) (empty-env)(empty-k))
3)

(eqv?
(value-of-cps '(letcc (throw (var 0) (const 4))) (empty-env)(empty-k))
4)






