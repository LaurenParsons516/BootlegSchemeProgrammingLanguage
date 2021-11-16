;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Lab 11_16_21|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;Given the grammar:

;block-exp := number

;                  := (number symbol block-exp)

;                  := (symbol block-exp block-exp)
 
;                  := symbol



;1. Write a block-exp that contains exactly 8 numbers
(define block-exp '(do
                       (remember a 1)
                     (remember b 2)
                     (remember c 3)
                     (remember d 4)
                     (remember e 5)
                     (remember f 6)
                     (remember g 7)
                     (remember h 8)))

;2. Write a scheme function called count-block-exp that takes a block-exp as a parameter and boils down to the sum of all of the numbers in the block-exp
(define count-block-exp
  (lambda (block-exp)
    (if (null? block-exp)
        0
        (+ (run-code (parse-code (car block-exp))) (count-block-exp (cdr block-exp))))) 

;3. Write a scheme function called collect-symbols that takes a block-exp as a parameter and returns a list containing all of the symbols found in the block-exp

;submit a link to the code as well as the self assessment

(define collect-symbols
  (lambda (block-exp)
    (if (null? block-exp)
        '()
        (let ([result (run-code (parse-code (car block-exp)))])
          (if (symbol? result)
            (cons result (collect-symbols (cdr block-exp)))
            (collect-symbols (cdr block-exp))))))
