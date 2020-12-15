#lang racket

;; To-do: Maybe we should stop doing all this (begin ...) and Void and #:mutable
;; stuff. It may make the program easier to write but it's unracketlike

;;------------------------------------------------------------------------------

;; A Byte is a Nat in the range [0, 255]
;; A Word is a Nat in the range [0, 65535]

;; A HexChar is one of:
;;     #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\a #\b #\c #\d #\e #\f
;; A HexStr is a string consisting only of HexChars

;; A Program is a HexStr

;;------------------------------------------------------------------------------

(define pong (string-append
  "6a026b0c6c3f6d0ca2eadab6dcd66e0022d4660368026060f015f0073000"
  "121ac717770869ffa2f0d671a2eadab6dcd66001e0a17bfe6004e0a17b02"
  "601f8b02dab6600ce0a17dfe600de0a17d02601f8d02dcd6a2f0d6718684"
  "8794603f8602611f871246021278463f1282471f69ff47006901d671122a"
  "68026301807080b5128a68fe630a807080d53f0112a2610280153f0112ba"
  "80153f0112c880153f0112c26020f01822d48e3422d4663e3301660368fe"
  "33016802121679ff49fe69ff12c87901490269016004f0187601464076fe"
  "126ca2f2fe33f265f12964146500d4557415f229d45500ee808080808080"
  "800000000000"))

;;------------------------------------------------------------------------------

;; Test Ram
;; (define ram (make-ram))