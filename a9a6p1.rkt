#lang racket
;CPS this interpreter. Call it value-of-cps...


(require racket/trace)

(define lex
  (lambda (exp acc)
    (match exp
      [`(zero? ,nexp) `(zero ,(lex nexp acc))]
      [`(* ,nexp1 ,nexp2) `(mult ,(lex nexp1 acc) ,(lex nexp2 acc))]
      [`(letcc ,k ,body) `(letcc ,(lex body (cons k acc)))]  
      [`(throw ,k ,exp) `(throw ,(lex k acc) ,(lex exp acc))]
      [`,n #:when (number? n) `(const ,n)]
      [`,y #:when (symbol? y)
           (let ([index (index-of acc y)]) 
             (cond
               [(eqv? index #f)  y]              
               [else (cons `var (list index))]))]
      [`(lambda (,arg) ,body)  `(lambda ,(lex body (cons arg acc)))]
      [`(,rator ,rand) `(app ,(lex rator acc) ,(lex rand acc))])))


(lex '(zero? 0) '())
(lex '(zero? 1) '())

(lex '(* 9 2) '())
(lex '(* 9 0) '())

(lex '(letcc x (sub1 x)) '())
(lex '(letcc (x) (* 2 5)) '())
(lex '(letcc x (x 2)) '())

(lex 'a '())

(lex '5 '())

(lex `(lambda (x) 5) '())
(lex '(lambda (x) x) '())
(lex '(lambda (y) (lambda (x) y)) '())


(define value-of
  (lambda (expr env)
    (match expr
      [`(const ,expr) expr]
      [`(mult ,x1 ,x2) (* (value-of x1 env) (value-of x2 env))]
      [`(sub1 ,x) (sub1 (value-of x env))]
      [`(zero ,x) (zero? (value-of x env))]
      [`(if ,test ,conseq ,alt) (if (value-of test env)
                                    (value-of conseq env)
                                    (value-of alt env))]
      [`(letcc ,body) (let/cc k
                         (value-of body (lambda (y) (if (zero? y) k (env (sub1 y))))))]
      [`(throw ,k-exp ,v-exp) ((value-of k-exp env) (value-of v-exp env))]
      [`(let ,e ,body) (let ((a (value-of e env)))
                         (value-of body (lambda (y) (if (zero? y) a (env (sub1 y))))))]
      [`(var ,y) (env y)]
      [`(lambda ,body) (lambda (a) (value-of body (lambda (y) (if (zero? y) a (env (sub1 y))))))]
      [`(app ,rator ,rand) ((value-of rator env) (value-of rand env))])))
 
(define empty-env
  (lambda ()
    (lambda (y)
      (error 'value-of "unbound identifier"))))
 
(define empty-k
  (lambda ()
    (lambda (v)
      v)))


(value-of '(const 7) (empty-env))

(value-of '(mult (const 7) (const 3)) (empty-env))

(value-of '(sub1 (const 7)) (empty-env))

(value-of '(zero (const 7)) (empty-env))

(value-of '(if (zero (const 0)) (sub1 (const 2)) (const 9)) (empty-env))

(value-of (lex '(letcc x (x (* 2 5))) '()) (empty-env))

(value-of '(letcc (app (var 0) (const 2))) (empty-env))


(value-of '(mult (const 9) (letcc (throw (const 5) (throw (var 0) (const 10)))))  (empty-env))
(value-of '(let (const 6) (const 4)) (empty-env))
(value-of '(letcc (const 5)) (empty-env)) 
(value-of '(app (lambda (var 0)) (const 5)) (empty-env))
((value-of (lex '(lambda (x) 5) (empty-env))  (empty-env)) '())
(value-of '(mult (const 9) (const 2)) (empty-env))
(value-of (lex '(* 9 2) (empty-env))  (empty-env))

(value-of '(letcc (mult (const 2) (const 5))) (empty-env))
(value-of '(letcc (const 2)) (empty-env))
(writeln "cps below")
       

(define value-of-cps
  (lambda (expr env k)
    (match expr
      [`(const ,expr) (k expr)]
      [`(var ,y) (k(env y))]
      [`(mult ,x1 ,x2) (value-of-cps x1 env (lambda (x1^) (value-of-cps x2 env (lambda (x2^) (k (* x1^ x2^))))))]
      [`(sub1 ,x)  (value-of-cps x env (lambda (sub1x) (k (sub1 sub1x))))]
      [`(zero ,x)  (value-of-cps x env (lambda (zerox) (k (zero? zerox))))]
      [`(if ,test ,conseq ,alt) (value-of-cps test env (lambda (v) (if v (value-of-cps conseq env k) (value-of-cps alt env k))))]
      [`(let ,e ,body) (value-of-cps e env (lambda  (a) (value-of-cps body (lambda (y) (if (zero? y) a (env (sub1 y)))) k)))]
      [`(letcc ,body) (value-of-cps body (lambda (y) (if (zero? y) k (env (sub1 y)))) k)]
      [`(throw ,k-exp ,v-exp) (value-of-cps k-exp env (lambda (k-exp-v) (value-of-cps v-exp env (lambda (v-exp-v) (k-exp-v v-exp-v)))))]
      [`(lambda ,body) (lambda (a) (value-of-cps body (lambda (y) (if (zero? y) a (env (sub1 y)))) k))]
      [`(app ,rator ,rand) (value-of-cps rator env (lambda (rato) (value-of-cps rand env  (lambda (rando) (rato rando k)))))])))

;(trace value-of-cps)




(eqv? (value-of-cps '(const 7) (empty-env)(empty-k))
      (value-of '(const 7) (empty-env)))

(eqv? (value-of-cps '(mult (const 7) (const 3)) (empty-env)(empty-k))
      (value-of '(mult (const 7) (const 3)) (empty-env)))
(eqv? (value-of-cps '(mult (const 7) (mult (const 3) (const 2))) (empty-env)(empty-k))
      (value-of '(mult (const 7) (mult (const 3) (const 2))) (empty-env)))

(eqv? (value-of-cps '(sub1 (const 7)) (empty-env)(empty-k))
      (value-of '(sub1 (const 7)) (empty-env)))

(eqv? (value-of-cps '(zero (const 7)) (empty-env)(empty-k))
      (value-of '(zero (const 7)) (empty-env)))
(eqv? (value-of-cps '(zero (const 0)) (empty-env)(empty-k))
      (value-of '(zero (const 0)) (empty-env)))

(eqv? (value-of-cps '(if (zero (const 0)) (sub1 (const 2)) (const 9)) (empty-env) (empty-k))
      (value-of '(if (zero (const 0)) (sub1 (const 2)) (const 9)) (empty-env)))

(eqv? 
(value-of-cps '(if (zero (const 1)) (sub1 (const 2)) (sub1 (const 9))) (empty-env) (empty-k))
(value-of '(if (zero (const 1)) (sub1 (const 2)) (sub1 (const 9))) (empty-env)))

(eqv?
(value-of-cps '(let (const 5) (var 0)) (empty-env)(empty-k))
(value-of '(let (const 5) (var 0)) (empty-env)))

(eqv? 
(value-of-cps '(letcc (mult (const 2) (const 5))) (empty-env)(empty-k))
(value-of '(letcc (mult (const 2) (const 5))) (empty-env)))

(eqv?
(value-of-cps '(letcc (const 2)) (empty-env)(empty-k))
(value-of '(letcc (const 2)) (empty-env)))

