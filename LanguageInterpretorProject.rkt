#lang racket

(define mvalue
  (lambda (lis)
    (cond
      [(number? lis)lis]
      [(boolean? lis) lis]
      [(and (eq? (operator lis) '-) (null? (firstexpressioncdr lis))) (- (mvalue (firstexpression lis)))]
      [(eq? (operator lis) '*)(* (mvalue (firstexpression lis)) (mvalue (secondexpression lis)))]
      [(eq? (operator lis) '+)(+ (mvalue (firstexpression lis)) (mvalue (secondexpression lis)))]
      [(eq? (operator lis) '-)(- (mvalue (firstexpression lis)) (mvalue (secondexpression lis)))]
      [(eq? (operator lis) '/)(quotient (mvalue (firstexpression lis)) (mvalue (secondexpression lis)))]
      [(eq? (operator lis) '%)(modulo (mvalue (firstexpression lis)) (mvalue (secondexpression lis)))]
      [(and (eq? (operator lis) '==) (boolean? (mvalue (firstexpression lis))))(eq? (mvalue (firstexpression lis))(mvalue (secondexpression lis)))]
      [(and (eq? (operator lis) '!=) (boolean? (mvalue (firstexpression lis))))(not (eq? (mvalue (firstexpression lis))(mvalue (secondexpression lis))))];might have to check more here
      [(and (eq? (operator lis) '==) (number? (mvalue (firstexpression lis))))(= (mvalue (firstexpression lis))(mvalue (secondexpression lis)))]
      [(and (eq? (operator lis) '!=) (number? (mvalue (firstexpression lis))))(not (= (mvalue (firstexpression lis))(mvalue (secondexpression lis))))]
      [(eq? (operator lis) '<)(< (mvalue (firstexpression lis)) (mvalue (secondexpression lis)))]
      [(eq? (operator lis) '>)(> (mvalue (firstexpression lis)) (mvalue (secondexpression lis)))]
      [(eq? (operator lis) '>=)(>= (mvalue (firstexpression lis)) (mvalue (secondexpression lis)))]
      [(eq? (operator lis) '<=)(<= (mvalue (firstexpression lis)) (mvalue (secondexpression lis)))]
      [(and (eq? (operator lis) '!) (boolean? (mvalue (firstexpression lis))))(not (mvalue (firstexpression lis)))]
      [(and (eq? (operator lis) '||) (boolean? (mvalue (firstexpression lis))))(or (mvalue (firstexpression lis)) (mvalue (secondexpression lis)))]
      [(and (eq? (operator lis) '&&) (boolean? (mvalue (firstexpression lis))))(and (mvalue (firstexpression lis))(mvalue (secondexpression lis)))]
      )))

(define operator
  (lambda (lis)
    (car lis)))

(define firstexpression
  (lambda (lis)
    (cadr lis)))

(define secondexpression
  (lambda (lis)
    (caddr lis)))

(define firstexpressioncdr
  (lambda (lis)
    (cddr lis)))