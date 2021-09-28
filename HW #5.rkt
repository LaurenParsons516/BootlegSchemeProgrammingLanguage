;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |HW #5|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define resolve
  (lambda (varName env)
    (cond
      ((null? env) #f)
      ((eq? varName (caar env)) (cadar env))
      (else (resolve varName (cdr env))))))


(define do-mathy-stuff-toaster
  (lambda (op num1 num2)
    (cond
      ((eq? op '+) (+ num1 num2))
      ((eq? op '-) (- num1 num2))
      ((eq? op '/) (/ num1 num2))
      ((eq? op '//) (quotient num1 num2))
      ((eq? op '%) (modulo num1 num2))
      ((eq? op '*) (* num1 num2))
      (else #f))))


(define no-parser
  (lambda (no-code)
    (cond
      ((number? no-code) (list 'num-lit-exp no-code))
      ((symbol? no-code) (list 'var-exp no-code))
      ((eq? (car no-code) 'do-mathy-stuff)
       (list 'math-exp (cadr no-code) (no-parser (caddr no-code)) (no-parser (cadddr no-code))))
      ((eq? (car no-code) 'function)
       (list 'func-exp
             (list 'params (cadr no-code))
             (list 'body
                   (no-parser (caddr no-code)))))
      ((eq? (car no-code) 'call)
       (if (null? (cddr no-code))
           (list 'call-exp (no-parser (cadr no-code)))
           (list 'call-exp (no-parser (cadr no-code)) (no-parser (cddr no-code)))))
      (else
       (if (null? (cdr no-code))
                  (list (no-parser (car no-code)))
                  (cons (no-parser (car no-code)) (no-parser (cdr no-code))))))))

(define define-params
  (lambda (params vals env)
    (if (null? params)
        '()
        (cons (list (car params) (run-parsed-code (car vals) env)) (define-params (cdr params) (cdr vals) env)))))


(define run-parsed-code
  (lambda (parsed-no-code env)
    (cond
      ((eq? (car parsed-no-code) 'num-lit-exp)
       (cadr parsed-no-code))
      ((eq? (car parsed-no-code) 'var-exp)
       (resolve (cadr parsed-no-code) env))
      ((eq? (car parsed-no-code) 'math-exp)
       (do-mathy-stuff-toaster
        (cadr parsed-no-code)
        (run-parsed-code (caddr parsed-no-code) env)
        (run-parsed-code (cadddr parsed-no-code) env)))
      ((eq? (car parsed-no-code) 'func-exp)
       (run-parsed-code (cadr (caddr parsed-no-code)) env))
      ((eq? (car parsed-no-code) 'call-exp)
       (if (null? (cadr (cadr (cadr parsed-no-code))))
           (run-parsed-code (cadr parsed-no-code) env)
           (run-parsed-code (cadr parsed-no-code) (append (define-params (cadr (cadr (cadr parsed-no-code))) (caddr parsed-no-code) env) env)))))))


(define env '((age 21) (a 7) (b 5) (c 23)))

(run-parsed-code (no-parser '(call (function () age))) env)
(run-parsed-code (no-parser '(call (function (e) e) (do-mathy-stuff % a b))) env)
(run-parsed-code (no-parser '(call (function (e f g h i) f) (do-mathy-stuff % a b) 100 200 300 400 500)) env)