#lang racket
(require "parenthec.rkt")
(require racket/trace)
;Transform your continuation constructors to a define-union, change the match in apply-k
;to instead use union-case, and ensure all constructor invocations are preceded with kt_,
;or something other than kt if you use a different name for your union. Make sure to remove
;the backquotes and commas in the patterns in what was your match expression.

(define-union kt
  (make-inner-mult x1^  k^)
  (make-outer-mult x2^ env^ k^)
  (make-inner-sub1 k^)
  (make-inner-zero k^)
  (make-inner-if conseq^ alt^ env^ k^)
  (make-inner-let body^ env^ k^)
  (make-inner-throw v-exp^ env^)
  (make-inner-app clos^ k^)
  (make-outer-app rand^ env^ k^)
  (empty-k))


(define apply-k
  (lambda (k v)
    (union-case k kt
      [(make-inner-mult x1^  k^) (apply-k k^ (* x1^ v))]
      [(make-outer-mult x2^ env^ k^) (value-of-cps x2^ env^ (kt_make-inner-mult v k^))]
      [(make-inner-sub1 k^) (apply-k k^ (sub1 v))]
      [(make-inner-zero k^) (apply-k k^ (zero? v))]
      [(make-inner-if conseq^ alt^ env^ k^) (if v (value-of-cps conseq^ env^ k^) (value-of-cps alt^ env^ k^))]
      [(make-inner-let body^ env^ k^) (value-of-cps body^ (envr_extend-env env^ v) k^)]
      [(make-inner-throw v-exp^ env^) (value-of-cps v-exp^ env^ v)]
      [(make-inner-app clos^ k^) (apply-closure clos^ v k^)]
      [(make-outer-app rand^ env^ k^) (value-of-cps rand^ env^ (kt_make-inner-app v k^))] 
      [(empty-k) v])))

#;(define apply-k
  (lambda (k v)
    (match k
      [`(make-inner-mult ,x1^  ,k^) (apply-k k^ (* x1^ v))]
      [`(make-outer-mult ,x2^ ,env^ ,k^) (value-of-cps x2^ env^ (make-inner-mult v k^))]
      [`(make-inner-sub1 ,k^) (apply-k k^ (sub1 v))]
      [`(make-inner-zero ,k^) (apply-k k^ (zero? v))]
      [`(make-inner-if ,conseq^ ,alt^ ,env^ ,k^) (if v (value-of-cps conseq^ env^ k^) (value-of-cps alt^ env^ k^))]
      [`(make-inner-let ,body^ ,env^ ,k^) (value-of-cps body^ (envr_extend-env env^ v) k^)]
      [`(make-inner-throw ,v-exp^ ,env^) (value-of-cps v-exp^ env^ v)]
      [`(make-inner-app ,clos^ ,k^) (apply-closure clos^ v k^)]
      [`(make-outer-app ,rand^ ,env^ ,k^) (value-of-cps rand^ env^ (make-inner-app v k^))] 
      [`(empty-k) v])))

#;(define empty-k
  (lambda ()
    `(empty-k)))

#;(define make-inner-mult
  (lambda (x1^ k^)
    `(make-inner-mult ,x1^  ,k^)))

#;(define make-outer-mult
  (lambda  (x2^ env^ k^)
    `(make-outer-mult ,x2^ ,env^ ,k^)))

#;(define make-inner-sub1
  (lambda (k^)
    `(make-inner-sub1 ,k^)))

#;(define make-inner-zero 
  (lambda (k^)
    `(make-inner-zero ,k^)))
  
#;(define make-inner-if
  (lambda (conseq^ alt^ env^ k^)
    `(make-inner-if ,conseq^ ,alt^ ,env^ ,k^)))

#;(define make-inner-let
  (lambda (body^ env^ k^)
    `(make-inner-let ,body^ ,env^ ,k^)))

#;(define make-inner-throw
  (lambda (v-exp^ env^)
    `(make-inner-throw ,v-exp^ ,env^)))

#;(define make-inner-app
  (lambda (clos^ k^)
    `(make-inner-app ,clos^ ,k^)))

#;(define make-outer-app
  (lambda (rand^ env^ k^)
    `(make-outer-app ,rand^ ,env^ ,k^))) 




(define apply-env
  (lambda (env y k^)
    (union-case env envr
      ((empty-env) (error 'value-of "unbound identifier"))
      ((extend-env env^ v^) (if (zero? y) (apply-k k^ v^) (apply-env env^ (sub1 y) k^)))))) 

(define-union envr
  (extend-env env^ v^)
  (empty-env))

(define-union closure
  (make-closure body env))

(define apply-closure
  (lambda (clos a k)
    (union-case clos closure
      ((make-closure body env) (value-of-cps body (envr_extend-env env a) k)))))

(define-union expr
  (const cexp)
  (var n)
  (if test conseq alt)
  (mult nexp1 nexp2)
  (sub1 nexp)
  (zero nexp)
  (letcc body)
  (throw kexp vexp)
  (let exp body)              
  (lambda body)
  (app rator rand))


    



(define value-of-cps
  (lambda (e env k)
    (union-case e expr 
                [(const expr) (apply-k k expr)]
                [(var y) (apply-env env y k)] 
                [(mult x1 x2) (value-of-cps x1 env (kt_make-outer-mult x2 env k))]
                [(sub1 x)  (value-of-cps x env (kt_make-inner-sub1 k))]
                [(zero x)  (value-of-cps x env (kt_make-inner-zero k))] 
                [(if test conseq alt) (value-of-cps test env (kt_make-inner-if  conseq alt env k))]
                [(let e body) (value-of-cps e env (kt_make-inner-let body env k))]
                [(letcc body) (value-of-cps body (envr_extend-env env k) k)] 
                [(throw k-exp v-exp) (value-of-cps k-exp env (kt_make-inner-throw v-exp env))] 
                [(lambda body) (apply-k k (closure_make-closure body env))]
                [(app rator rand) (value-of-cps rator env (kt_make-outer-app rand env k))])))


(define main 
  (lambda ()
    (value-of-cps 
     (expr_let 
      (expr_lambda
       (expr_lambda 
        (expr_if
         (expr_zero (expr_var 0))
         (expr_const 1)
         (expr_mult (expr_var 0) (expr_app (expr_app (expr_var 1) (expr_var 1)) (expr_sub1 (expr_var 0)))))))
      (expr_mult
       (expr_letcc
        (expr_app
         (expr_app (expr_var 1) (expr_var 1))
         (expr_throw (expr_var 0) (expr_app (expr_app (expr_var 1) (expr_var 1)) (expr_const 4)))))
       (expr_const 5)))
     (envr_empty-env)
     (kt_empty-k))))


(main)

