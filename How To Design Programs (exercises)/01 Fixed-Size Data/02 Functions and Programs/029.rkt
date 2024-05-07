;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |029|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (profit price)
  (- (* (+ 120
           (* (/ 15 0.1)
              (- 5.0 price)))
        price)
     (* 0.04
        (+ 120
           (* (/ 15 0.1)
              (- 5.0 price))))))

(profit 0)
(profit 1)
(profit 2)
(profit 3)
(profit 4)
(profit 5)

(define (best-ticket-price p)
  (if (> (profit p) (profit (+ p 0.01)))
      p
      (best-ticket-price (+ p 0.01))))

(best-ticket-price 0)
