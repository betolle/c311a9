#lang racket
(require "parenthec.rkt")
(require racket/trace)

;Convert all label invocations into assignments to the program counter

;Add calls to mount-trampoline and dismount-trampoline.
;Note this will require modifyingempty-k in your kt union, and the empty-k
;clause in the union-case inside apply-k. 

;On the last line of main, print the register containing the final value of the program, e.g.
;(printf “Fact 5: ~s\n” *v*) 

(define-registers *a-k* *k^* *k* *a* *v* *e* *y* *env* *clos*)

(define-program-counter *pc*)

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
  (empty-k jumpout))

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

(define-union envr
  (extend-env env^ v^)
  (empty-env))

(define-union closure
  (make-closure body env))


(define-label apply-k
    (union-case *a-k* kt
                [(make-inner-mult x1^  k^) (begin (set! *a-k* k^)
                                                  (set! *v* (* x1^ *v*))
                                                  (set! *pc* apply-k))]
                [(make-outer-mult x2^ env^ k^) (begin (set! *k* (kt_make-inner-mult *v* k^))
                                                      (set! *e* x2^)
                                                      (set! *env* env^)
                                                 (set! *pc* value-of-cps))]
                [(make-inner-sub1 k^) (begin (set! *a-k* k^)
                                             (set! *v* (sub1 *v*))
                                        (set! *pc* apply-k))]
                [(make-inner-zero k^) (begin (set! *a-k* k^)
                                             (set! *v* (zero? *v*))
                                        (set! *pc* apply-k ))]
                [(make-inner-if conseq^ alt^ env^ k^) (if *v*
                                                          (begin (set! *k* k^)
                                                                     (set! *e* conseq^)
                                                                     (set! *env* env^)
                                                                (set! *pc* value-of-cps))
                                                          (begin (set! *k* k^)
                                                                 (set! *e* alt^)
                                                                 (set! *env* env^)
                                                            (set! *pc* value-of-cps)))]
                [(make-inner-let body^ env^ k^) (begin (set! *k* k^)
                                                       (set! *e* body^)
                                                       (set! *env* (envr_extend-env env^ *v*))
                                                  (set! *pc* value-of-cps))]
      
                [(make-inner-throw v-exp^ env^) (begin (set! *k* *v*)
                                                        (set! *e* v-exp^)
                                                        (set! *env* env^)
                                                   (set! *pc* value-of-cps))]
                [(make-inner-app clos^ k^) (begin (set! *k* k^)
                                                  (set! *clos* clos^)
                                                  (set! *a* *v*)
                                             (set! *pc* apply-closure))]
                [(make-outer-app rand^ env^ k^) (begin (set! *k* (kt_make-inner-app *v* k^))
                                                       (set! *e* rand^)
                                                       (set! *env* env^)
                                                  (set! *pc* value-of-cps))] 
                [(empty-k jumpout) (dismount-trampoline jumpout)]))

(define-label apply-env
    (union-case *env* envr
                ((empty-env) (error 'value-of "unbound identifier"))
                ((extend-env env^ v^) (if (zero? *y*)
                                          (begin (set! *a-k* *k^*)
                                                 (set! *v* v^)
                                            (set! *pc* apply-k))
                                          (begin (set! *k^* *k^*)
                                                 (set! *env* env^)
                                                 (set! *y* (sub1 *y*))
                                            (set! *pc* apply-env)))))) 

(define-label apply-closure
    (union-case *clos* closure
                ((make-closure body env) (begin (set! *k* *k*)
                                                (set! *e* body)
                                                (set! *env* (envr_extend-env env *a*))
                                           (set! *pc* value-of-cps)))))


(define-label value-of-cps
    (union-case *e* expr 
                [(const expr) (begin (set! *a-k* *k*)
                                     (set! *v* expr)
                                (set! *pc* apply-k))]
                [(var y) (begin (set! *k^* *k*)
                                (set! *env* *env*)
                                (set! *y* y)
                           (set! *pc* apply-env))] 
                [(mult x1 x2) (begin (set! *k* (kt_make-outer-mult x2 *env* *k*))
                                     (set! *e* x1)
                                     (set! *env* *env*)
                                (set! *pc* value-of-cps))]
                [(sub1 x) (begin (set! *k* (kt_make-inner-sub1 *k*))
                                 (set! *e* x)
                                 (set! *env* *env*)
                            (set! *pc* value-of-cps))]
                [(zero x) (begin (set! *k* (kt_make-inner-zero *k*))
                                 (set! *e* x)
                                 (set! *env* *env*)
                            (set! *pc* value-of-cps))] 
                [(if test conseq alt) (begin (set! *k* (kt_make-inner-if  conseq alt *env* *k*))
                                             (set! *e* test)
                                             (set! *env* *env*)
                                        (set! *pc* value-of-cps))]
                [(let e body) (begin (set! *k* (kt_make-inner-let body *env* *k*))
                                     (set! *e* e)
                                     (set! *env* *env*)
                                (set! *pc* value-of-cps))]
                [(letcc body) (begin (set! *k* *k*)
                                     (set! *e* body)
                                     (set! *env* (envr_extend-env *env* *k*))
                                (set! *pc* value-of-cps))] 
                [(throw k-exp v-exp) (begin (set! *k* (kt_make-inner-throw v-exp *env*))
                                            (set! *e* k-exp)
                                            (set! *env* *env*)
                                       (set! *pc* value-of-cps))] 
                [(lambda body) (begin (set! *a-k* *k*)
                                      (set! *v* (closure_make-closure body *env*))
                                 (set! *pc* apply-k))]
                [(app rator rand) (begin (set! *k* (kt_make-outer-app rand *env* *k*))
                                         (set! *e* rator)
                                         (set! *env* *env*)
                                    (set! *pc* value-of-cps))]))

(define-label main 
    (begin (set! *k* (kt_empty-k *v*))
           (set! *env* (envr_empty-env))
           (set! *e* (expr_let 
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
       (expr_const 5))))
           (set! *pc* value-of-cps)
           (mount-trampoline kt_empty-k *k* *pc*)
           (printf "Fact 5: ~s\n" *v*)))
           
(main)