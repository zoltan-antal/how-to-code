;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname parameterization-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ListOfString -> Boolean
;; produce true if los includes "UBC"
;(define (contains-ubc? los) false) ;stub
(define (contains-ubc? los) (contains? "UBC" los))

;; ListOfString -> Boolean
;; produce true if los includes "McGill"
;(define (contains-mcgill? los) false) ;stub
(define (contains-mcgill? los) (contains? "McGill" los))

;; String (listof String) -> Boolean
;; produce true if los includes s
(check-expect (contains? "UBC" empty) false)
(check-expect (contains? "UBC" (cons "McGill" empty)) false)
(check-expect (contains? "UBC" (cons "UBC" empty)) true)
(check-expect (contains? "UBC" (cons "McGill" (cons "UBC" empty))) true)
(check-expect (contains? "UBC" (cons "UBC" (cons "McGill" empty))) true)
(check-expect (contains? "Toronto" (cons "UBC" (cons "McGill" empty))) false)

(define (contains? s los)
  (cond [(empty? los) false]
        [else
         (if (string=? (first los) s)
             true
             (contains? s (rest los)))]))

;; ====================

;; ListOfNumber -> ListOfNumber
;; produce list of sqr of every number in lon
;(define (squares lon) empty) ;stub
(define (squares lon) (map2 sqr lon))

;; ListOfNumber -> ListOfNumber
;; produce list of sqrt of every number in lon
;(define (square-roots lon) empty) ;stub
(define (square-roots lon) (map2 sqrt lon))

;; (X -> Y) (listof X) -> (listof Y)
;; given fn and (list n0 n1 ...), produce (list (fn n0) (fn n1) ...)
(check-expect (map2 sqr empty) empty)
(check-expect (map2 sqr (list 2 4)) (list 4 16))
(check-expect (map2 sqrt (list 16 9)) (list 4 3))
(check-expect (map2 abs (list 2 -3 4)) (list 2 3 4))
(check-expect (map2 string-length (list "abc" "d" "ef")) (list 3 1 2))

(define (map2 fn lon)
  (cond [(empty? lon) empty]
        [else
         (cons (fn (first lon))
               (map2 fn (rest lon)))]))

;; ====================

;; ListOfNumber -> ListOfNumber
;; produce list with only positive? elements of lon
;(define (positive-only lon) empty) ;stub
(define (positive-only lon) (filter2 positive? lon))

;; ListOfNumber -> ListOfNumber
;; produce list with only negative? elements of lon
;(define (negative-only lon) empty) ;stub
(define (negative-only lon) (filter2 negative? lon))

;; (X -> Boolean) (listof X) -> (listof X)
;; given pred and list, produce list with elements for which pred produces true
(check-expect (filter2 positive? empty) empty)
(check-expect (filter2 positive? (list 1 -2 3 -4)) (list 1 3))
(check-expect (filter2 negative? (list 1 -2 3 -4)) (list -2 -4))

(define (filter2 pred lon)
  (cond [(empty? lon) empty]
        [else
         (if (pred (first lon))
             (cons (first lon)
                   (filter2 pred (rest lon)))
             (filter2 pred (rest lon)))]))
