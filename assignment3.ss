#lang racket
(define-struct gnode (sym list) #:transparent)
(define-struct ident (str) #:transparent)
(define-struct num (val) #:transparent)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Predicate_parser
(define(pred-p p)
  (λ(str) (if(string=? str "") "fail"
             (if(p (string-ref str 0)) (cons (string-ref str 0)  (substring str 1))
                "fail"))))
;;;;;;;;;;;;;;;;;;;;;;;;;
;Single-digit_parser
(define single-digit-p 
  (λ(str)
    ((pred-p (lambda(c) (char-numeric? c)))str)))
;;;;;;;;;;;;;;;;;;;;;;;;;
;Single-alphabet_parser
(define single-alphabet-p 
  (λ(str)
    ((pred-p (lambda(c) (char-alphabetic? c)))str)))
;;;;;;;;;;;;;;;;;;;;;;;;;;
;Sequential_parser
(define (seq p1 p2 f)
  (λ(str)
    (let* ((res ( p1 str)))
      (if(pair? res) 
         (let ((res2 ( p2 (cdr res))))
           (if (pair? res2) (f (p1 str) (p2 (cdr res)))
               "fail"))
         "fail"))))
(define (combine-cc c1 c2)
  (cons (string (car c1) (car c2)) (cdr c2)))
;;;;;;;;;;;;;;;;;;;;;;;;
;Alternate_parser
(define (alt p1 p2)
  (λ(str)
    (let* ((res ( p1 str)))
      (if(pair? res) res
         (let ((res2 (p2  str)))
           (if(pair? res2) res2
              "fail"))))))
;;;;;;;;;;;;;;;;;;;;;;;;;
;Epsilon_parser
(define epsilon-p (λ(str)
                    (cons "" str)))
;;;;;;;;;;;;;;;;;;;;;;;;;
;Zero-or-more_parser
(define(zero-or-more p f)
  (λ(str)
    (define(helper st c)
      (if(pair? (p st))                  
         (helper (cdr (p st)) (f c (p st)))
         (f c (epsilon-p st))))
    (helper str (cons "" ""))))
(define (combine-cs c1 c2)
  (if(char? (car c2)) (cons (string-append (car c1) (string (car c2))) (cdr c2))
     (cons (string-append (car c1) (car c2)) (cdr c2))))
;;;;;;;;;;;;;;;;;;;;;;;;;
;One-or-more_parser
(define(one-or-more p f)
  (λ(str)
    (let((res ((zero-or-more p f)str)))
      (if(string=? (car res) "") "fail"
         res))))
;;;;;;;;;;;;;;;;;;;;;;;;;;
;Whitespace_parser
(define whitespace-p 
  (λ(str)
    (define(helper st)
      (if(string=? st "") (epsilon-p st)
         (if(char=? (string-ref st 0) #\ ) (helper (substring st 1))
            (epsilon-p st))))
    (helper str)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Number_parser
(define number-p
  (λ(str)
    (if(string=? str "") ""
       (let* ((res (whitespace-p str))
              (res2 ((one-or-more single-digit-p combine-cs)(cdr res))))
         (if(pair? res2) (cons (num (string->number (car res2))) (cdr res2) )
            "fail")))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Identifier_parser
(define identifier-p
  (λ(str)
    (let*((st (whitespace-p str))
          (res ((one-or-more single-alphabet-p combine-cs)(cdr st))))
      (if(pair? res)
         (let((res2 ((one-or-more(alt single-digit-p single-alphabet-p )combine-cs )(cdr st))))
           (cons (ident (car res2)) (cdr res2)))
         "fail"))))
;Variable_parser
(define variable-p
  (λ(str)
    (let ((st (identifier-p str)))
      (if(equal? st "fail") "fail"
         (if(string=? (cdr st) "") st
            (let*((tx (cdr (whitespace-p (cdr st)))))             
              (if(and (char=? (string-ref tx 0) #\[) 
                      (not(equal? (expression-p (substring tx 1)) "fail"))
                      (char=? (string-ref (cdr (whitespace-p(cdr(expression-p (substring tx 1))))) 0) #\]))
                 (cons (gnode 'ARRAY (list (car st) (car (expression-p (substring tx 1))))) 
                       (substring (cdr (whitespace-p(cdr(expression-p (substring tx 1))))) 1))
                 st)))))))
;Term_parser
(define term-p
  (λ(str)
    (let ((res (number-p str)))
      (if(pair? res) res
         (let ((res2 (variable-p str)))
           (if(and (equal? res2 "fail") (char=? (string-ref (cdr(whitespace-p str)) 0) #\())
              (let ((res3 (expression-p  (substring (cdr(whitespace-p str)) 1 ))))
                (if(equal? res3 "fail") "fail"
                   (if(equal? (string-ref (cdr(whitespace-p (cdr res3))) 0) #\)) (cons (car res3) (substring (cdr(whitespace-p (cdr res3))) 1))
                      "fail")))                      
              res2))))))
;Expression-parser
(define expression-p
  (λ(str)
    (let ((ter (term-p str)))
      (if(pair? ter)
         (if(string=? (cdr(whitespace-p (cdr ter ))) "") ter
            (if (and(equal? (string-ref (cdr(whitespace-p (cdr ter ))) 0) #\+)
                    (> (string-length (cdr(whitespace-p (cdr ter )))) 1)
                    (pair?  (expression-p (substring (cdr(whitespace-p (cdr ter)))1))))
                (cons  (gnode 'PLUS (list (car ter) (car (expression-p (substring (cdr(whitespace-p (cdr ter))) 1)))))
                       (cdr (expression-p (substring (cdr(whitespace-p (cdr ter)))1))))
                ter))
         "fail"))))
;;;;;;;;;;;;;;;;;;;     
(define(position str ch)
  (let((lis (string->list str))) 
    (define(helper l1 x)
      (if(null? l1) "fail"
         (if(equal? (car l1) ch)   x
            (helper (cdr l1) (+ x 1)))))
    (helper lis 0)))

;Assignment_parser
(define assignment-p
  (λ(str)
    (let((equ (position str #\=)))
      (if(equal? equ "fail") "fail"
         (let*(
          (va (substring str 0 equ))
          (ex (substring str equ))
          (var  (variable-p va))
          (exp  (expression-p (substring (cdr (whitespace-p ex)) 1))))
      (if(and (pair? var)
              (string=? (cdr (whitespace-p (cdr var))) "")
              (pair? exp))
         (cons (gnode 'ASSIGN (list (car var) (car exp))) (cdr exp))
         "fail"))))))
