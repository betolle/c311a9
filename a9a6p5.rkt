#lang racket
;Replace your higher-order function representation of environments with a tagged-list data structure
;representation. Consider first ensuring that the last two formal parameters to apply-env (y and k) are
;the same as the formal parameters to the inner functions in extend-env and apply-env. Remember, if you
;add (else (env-cps y k)) as the last line of your match expression, you can test each transformation one at a time.

;accidently did multiple  steps in this step. Because this is due soon I will not be going back to correct this mistake
;steps 5 6 and 7


(define empty-env
  (lambda ()
    `(empty-env)
    #;(lambda (y k^)
      (error 'value-of "unbound identifier"))))
 
(define empty-k
  (lambda ()
    (lambda (v)
      v)))

(define apply-env
  (lambda (env y k^)
    (match env
      (`(empty-env) (error 'value-of "unbound identifier"))
      (`(extend-env ,env^ ,v^) (if (zero? y) (apply-k k^ v^) (apply-env env^ (sub1 y) k^)))
      #;(else (apply-k k^ (extend-env env y)))
    #;(apply-k k^ (extend-env env y))
      ))) 

(define extend-env
  (lambda (env^ v^)
    `(extend-env ,env^ ,v^)
    #;(lambda (y k^)
      (if (zero? y) (apply-k k^ v^) (apply-env env^ (sub1 y) k^)))))

(define make-closure
  (lambda (body env)
    `(make-closure ,body ,env)
    #;(lambda (y k^)
      (value-of-cps body (extend-env env y) k^))))

#;(define apply-closure
  (lambda (rato rando k)
    (rato rando k)
    ))
;rator should lead to makeclosure, rando should be atleast
;1 lambda so refering to as body, k retains form

(define apply-closure
  (lambda (clos body k)
    (match clos
      (`(make-closure ,body ,env) (lambda (y k^) (value-of-cps body (extend-env env y) k^))))))

(define apply-k
  (lambda (k e)
    (k e)))


(define value-of-cps
  (lambda (expr env k)
    (match expr
      [`(const ,expr) (apply-k k expr)]
      [`(var ,y) (apply-env env y k)] 
      [`(mult ,x1 ,x2) (value-of-cps x1 env (lambda (x1^) (value-of-cps x2 env (lambda (x2^) (apply-k k (* x1^ x2^))))))]
      [`(sub1 ,x)  (value-of-cps x env (lambda (sub1x) (apply-k k (sub1 sub1x))))]
      [`(zero ,x)  (value-of-cps x env (lambda (zerox) (apply-k k (zero? zerox))))]
      [`(if ,test ,conseq ,alt) (value-of-cps test env (lambda (v) (if v (value-of-cps conseq env k) (value-of-cps alt env k))))]
      [`(let ,e ,body) (value-of-cps e (extend-env env (value-of-cps body (lambda (y) (extend-env env y)) k)) k)]
      [`(letcc ,body) (value-of-cps body (lambda (y) (extend-env env y)) k)]
      [`(throw ,k-exp ,v-exp) (value-of-cps k-exp env (lambda (k-exp-v) (value-of-cps v-exp env (lambda (v-exp-v) (k-exp-v v-exp-v)))))]
      [`(lambda ,body) (apply-k k (make-closure body env))]
      [`(app ,rator ,rand) (value-of-cps rator env (lambda (clos) (value-of-cps rand (extend-env env (lambda (body) (apply-closure clos body k))) k)))])))
































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
(value-of-cps '(let (var 0)(const 5)) (empty-env)(empty-k))
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