#lang racket/gui

(require "arch.rkt")
(require "disassemble.rkt")
(require "display.rkt")
(require "ram.rkt")
(require "registers.rkt")
(require "util.rkt")
(require "converted.rkt")

;;------------------------------------------------------------------------------

;; These data definitions apply to all source files in this project:

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

(define test "613e620e630af329d125")
 
;;------------------------------------------------------------------------------
 
;; Basic virtual machine parameters
 
;; Ram is empty by default
(define ram (make-ram))
 
;; Registers
(define reg (registers 512 0 #xf00 0 0 (vector 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
 
;; 64x32 display
(define disp (make-display))

(load-program! pong 512 ram)
(init-cpu! ram reg disp (void))

(define file-len 50)
(define last-sound-time (current-milliseconds))

;;------------------------------------------------------------------------------
 
;; Special GUI parameters
 
;; Each pixel will become scale-factor pixels wide in the display window
(define scale-factor 10)
 
;;------------------------------------------------------------------------------
 
;; (draw-ram context ram x y) draws the contents of ram as 1-byte strings in a
;; 64x64 byte grid to the given device context, starting at position (x, y)

;; draw-ram: DC Ram Nat Nat -> Void
(define (draw-ram context ram x y)
   (cond [(equal? (+ x (* y 64)) max-ram) ""]
         [(zero? (modulo (add1 x) 64))
          (draw-ram context ram (modulo (add1 x) 64) (add1 y))]
         [else (begin
          (send context draw-text (dec->hex (ram-ref ram (+ x (* y 64))))
                (* x 16) (* y 9))
          (draw-ram context ram (add1 x) y))]))

;;------------------------------------------------------------------------------

;; (key-handler! key-event) updates all frames and steps the cpu if the provided
;; key-event is a spacebar press.

;; key-handler!: KeyEvent -> Void
(define (key-handler! key-event)
  (let ([keycode (send key-event get-key-code)])
    (cond [(equal? keycode #\space) (void)]
          [else (void)])))

;;------------------------------------------------------------------------------

;; (update) updates the cpu

;; uodate: Void -> Void
(define (update)
  (begin (cpu! ram reg disp (void))
         ;; (send ram-frame refresh)
         (send display-frame refresh)
         (if (and (> (registers-st reg) 0)
                  (> (- (current-milliseconds) last-sound-time) file-len))
             (begin (play-sound "resource/beep.wav" true)
                    (set! last-sound-time (current-milliseconds))) (void))
         (sleep/yield 1/1000)))

;;------------------------------------------------------------------------------

;; Ram viewer window
(define ram-frame
  (new frame% [label "racket-chip-8: ram-viewer"]
              [width 1010] [height 580]))

;; Ram viewer canvas
(define ram-canvas%
  (class canvas%
    (super-new)
    (define/override (on-char key-event) (key-handler! key-event))))

;; Creating an instance of the canvas to draw the ram contents
(define ram-viewer-canvas
  (new ram-canvas% [parent ram-frame]
                   [paint-callback
                    (λ (canvas context)
                      (send context set-font (make-font #:size 8))
                      (send context set-text-foreground "white")
                      (draw-ram context ram 0 0))]))

;; Set ram viewer background to black
(send ram-viewer-canvas set-canvas-background (make-object color%))

;;------------------------------------------------------------------------------

;; Display output window
(define display-frame
  (new frame% [label "racket-chip-8: display"]
              [width (* display-width scale-factor)]
              [height (* display-height scale-factor)]))

;; Setting up the display canvas
(define display-canvas%
  (class canvas%
    (super-new)
    (define/override (on-char key-event) (key-handler! key-event))))

;; Create a new instance of the display canvas
(define display-canvas
  (new display-canvas% [parent display-frame]
                       [paint-callback
                        (λ (canvas context)
                          (send context set-scale scale-factor scale-factor)
                          (send context draw-bitmap (make-object bitmap% disp 64 32) 0 0))]))

;; Set display output background to black
(send display-canvas set-canvas-background (make-object color%))

;;------------------------------------------------------------------------------

;; (send ram-frame show #t)
(send display-frame show #t)

;;------------------------------------------------------------------------------

(define (main)
  (begin (update) (main)))

(main)

;;------------------------------------------------------------------------------