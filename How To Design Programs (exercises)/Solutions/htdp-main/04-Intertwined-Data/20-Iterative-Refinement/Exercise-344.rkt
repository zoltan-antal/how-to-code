;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Exercise-344) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; Exercise 344.
;; Redesign find-all from exercise 342 using ls-R from exercise 343.


(require 2htdp/abstraction)


;; A Path is [List-of String].

(define-struct file [name size content])
;; A File is a structure:
;;   (make-file String N String)

(define-struct dir [name dirs files])
;; A Dir is a structure:
;;    (make-dir String [List-of Dir] [List-of File])


(define Dir-Text
  (make-dir "Text"
            '()
            (list
             (make-file "part1" 99 "")
             (make-file "part2" 52 "")
             (make-file "part3" 17 ""))))

(define Dir-Code
  (make-dir "Code"
            '()
            (list
             (make-file "hang" 8 "")
             (make-file "draw" 2 ""))))

(define Dir-Docs
  (make-dir "Docs"
            '()
            (list
             (make-file "read!" 19 ""))))

(define Dir-Libs
  (make-dir "Libs"
            (list Dir-Code Dir-Docs)
            '()))

(define Dir-TS
  (make-dir "TS"
            (list Dir-Text Dir-Libs)
            (list
             (make-file "read!" 10 ""))))

(define Dir-Empty (make-dir "Empty" '() '()))


;; Dir -> [List-of Path]
;; Produces the paths to all files in the given Dir.
(check-expect (ls-R Dir-Empty) '())
(check-expect (ls-R Dir-Docs) '(("Docs" "read!")))
(check-expect (ls-R Dir-Text) '(("Text" "part1") ("Text" "part2") ("Text" "part3")))
(check-expect (ls-R Dir-Libs)
              '(("Libs" "Code" "hang")
                ("Libs" "Code" "draw")
                ("Libs" "Docs" "read!")))
(check-expect (ls-R Dir-TS)
              '(("TS" "read!")
                ("TS" "Text" "part1")
                ("TS" "Text" "part2")
                ("TS" "Text" "part3")
                ("TS" "Libs" "Code" "hang")
                ("TS" "Libs" "Code" "draw")
                ("TS" "Libs" "Docs" "read!")))
(define (ls-R d)
  (local (;; [List-of File] -> [List-of Path]
          (define (list-files lof)
            (map (lambda (f)
                   (append (list (dir-name d) (file-name f))))
                 lof))

          ;; [List-of Dir] -> [List-of Path]
          (define (list-dirs lod)
            (cond
              [(empty? lod) '()]
              [else (append
                     (map (lambda (l)
                            (cons (dir-name d) l))
                          (ls-R (first lod)))
                     (list-dirs (rest lod)))])))

    (append
     (list-files (dir-files d))
     (list-dirs (dir-dirs d)))))

;; Dir String -> [List-of Path]
;; Produces paths to the files with the given name.
(check-expect (find-all Dir-Empty "Test") '())
(check-expect (find-all Dir-TS "Test") '())
(check-expect (find-all Dir-TS "read!") '(("TS" "read!") ("TS" "Libs" "Docs" "read!")))
(check-expect (find-all Dir-TS "part1") '(("TS" "Text" "part1")))
(check-expect (find-all Dir-TS "hang") '(("TS" "Libs" "Code" "hang")))
(check-expect (find-all (make-dir "Test" (list Dir-TS) '()) "hang")
              '(("Test" "TS" "Libs" "Code" "hang")))
(define (find-all d name)
  (local ((define (by-file-name l)
            (local ((define (last-item l)
                      (match l
                        [(cons lst '()) lst]
                        [(cons fst rst) (last-item rst)])))
              (string=? name (last-item l)))))
    (filter by-file-name (ls-R d))))
