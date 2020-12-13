;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname emul) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
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

;; We need to be able to access the "program" string a byte at a time so we need
;; (program-first p) and (program-rest p)
;; Never mind it has to be loaded into ram so we must convert string->vector

;; We need some way to load this in from a file in the future
;; Eventually we will read this as numbers, not characters of a string

;; We need some way to store the register information, maybe we could use
;; (define-struct) or (class)

;; We need some way to store RAM data maybe (vector)

;; We need some way to output an image to the screen, likely (require sgl)
;; Except, keyboard input must be handled by racket/gui/base so



;; yo



