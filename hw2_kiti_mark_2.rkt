; 2st homework of 4nd class
#lang racket

(require 2htdp/image)

(define s 500)

(define (half x)
    (/ x 2))

(define (find-hypotenuse-side opposite adjacent)
    (sqrt (+ (sqr opposite) (sqr adjacent))))

(define (find-hypotenuse-with-equal-side s)
    (find-hypotenuse-side s s))

(define (find-side-of-inner-square s)
    (find-hypotenuse-with-equal-side (half s)))

(define (draw-rose-sepals frame-size)
    (overlay
        (square frame-size "outline" "white")
        (square frame-size "solid" "seagreen")))

(define (draw-rose-petals frame-size)
    (overlay
        (rotate 45
            (draw-rose-sepals (find-hypotenuse-with-equal-side (half frame-size))))
        (overlay
            (circle (half frame-size) "outline" "white")
            (circle (half frame-size) "solid" "light red"))))

; # question 2, draw rose shape
(define (draw-rose-flower-with-recursion-with-frame-size frame-size number-of-petals)
    (define (draw-rose-petals-nest frame-size number-of-petals)
        (if (= number-of-petals 1)
            (draw-rose-petals frame-size)
            (underlay
                (draw-rose-petals frame-size)
                (rotate 45
                    (draw-rose-petals-nest (find-side-of-inner-square frame-size) (- number-of-petals 1))))))
    (overlay
        (draw-rose-petals-nest frame-size number-of-petals)
        (draw-rose-sepals frame-size)))

(draw-rose-flower-with-recursion-with-frame-size s 3)
