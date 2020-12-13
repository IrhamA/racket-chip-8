;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname emul) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;;------------------------------------------------------------------------------

;; Some code here like a string
(define program (string-append
  "6a026b0c6c3f6d0ca2eadab6dcd66e0022d4660368026060f015f0073000"
  "121ac717770869ffa2f0d671a2eadab6dcd66001e0a17bfe6004e0a17b02"
  "601f8b02dab6600ce0a17dfe600de0a17d02601f8d02dcd6a2f0d6718684"
  "8794603f8602611f871246021278463f1282471f69ff47006901d671122a"
  "68026301807080b5128a68fe630a807080d53f0112a2610280153f0112ba"
  "80153f0112c880153f0112c26020f01822d48e3422d4663e3301660368fe"
  "33016802121679ff49fe69ff12c87901490269016004f0187601464076fe"
  "126ca2f2fe33f265f12964146500d4557415f229d45500ee808080808080"
  "800000000000"))

;; A Byte is a Nat in the range [0, 255]

;; A Progam is a Str

;; A Ram is a (vectorof Byte)
(define ram (make-vector 4096 0))

;;------------------------------------------------------------------------------

;; (load-program! p n r) consumes a program (p) a number (n) and Ram (r) and
;; inserts the converted (p) into the right spot at (n) in (r)

;; load-program!: Program Nat Ram -> Void
(define (load-program! p n r)
  (cond [(string=? p "") 0]
        [(> (+ n (/ (string-length p) 2)) 4096)
         (error "Program cannot be loaded into memory at specified location")]
        [else (begin (vector-set! r n (hex->num (substring p 0 2)))
                     (load-program! (substring p 2 (string-length p))
                                    (add1 n) r))]))

(check-error (load-program! p 5555555 ram))

;;------------------------------------------------------------------------------

;; (hex->num str) consumes a string (str) and returns the hex value of (str)
;; hex->num: Str -> Nat
(define (hex->num str)
  (let (;; (char->num char) takes str (char) and produces corresponding hex num
        ;; char->num: Str -> Nat
        [char->num (λ (char)
          (cond [(string=? "a" char) 10]
                [(string=? "b" char) 11]
                [(string=? "c" char) 12]
                [(string=? "d" char) 13]
                [(string=? "e" char) 14]
                [(string=? "f" char) 15]
                [else (string->number char)]))])
    
  (cond [(string=? str "") 0]
        [else (+ (* (expt 16 (string-length (substring str 1 (string-length str))))
                    (char->num (substring str 0 1)))
                 (hex->num (substring str 1 (string-length str))))])))

;; Tests
(check-expect (hex->num "1af31b") 1766171)
(check-expect (hex->num "ba") 186)
(check-expect (hex->num "cc") 204)
(check-expect (hex->num "de4") 3556)

;;------------------------------------------------------------------------------