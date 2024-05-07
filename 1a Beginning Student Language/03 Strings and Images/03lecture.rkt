;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lecture) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(circle 50 "solid" "green")
(rectangle 200 40 "outline" "red")
(text "hello" 36 "orange")

(above (circle 10 "solid" "green")
         (circle 20 "solid" "yellow")
         (circle 30 "solid" "red"))

(beside (circle 10 "solid" "green")
         (circle 20 "solid" "blue")
         (circle 30 "solid" "red"))

(overlay (circle 10 "solid" "green")
         (circle 20 "solid" "white")
         (circle 30 "solid" "red"))