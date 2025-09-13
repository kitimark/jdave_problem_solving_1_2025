; 2st homework of 4nd class
#lang racket

(require 2htdp/image)

; # question 2, draw rose shape
(define s 500)

(define (half x)
    (/ x 2))

(define (find-hypotenuse-side opposite adjacent)
    (sqrt (+ (sqr opposite) (sqr adjacent))))

(define (find-hypotenuse-of-equal-side s)
    (find-hypotenuse-side s s))

(define (find-side-of-inner-square s)
    (find-hypotenuse-of-equal-side (half s)))

(define (draw-rose-sepals s)
    (square s "solid" "seagreen"))

(define (draw-rose-petals s)
    (overlay
        (rotate 45
            (draw-rose-sepals (find-hypotenuse-of-equal-side (half s))))
        (circle (half s) "solid" "light red")))

(define (draw-rose-flower-with-recursion-with-frame-size s n)
    (define (draw-rose-petals-nest s n)
        (if (= n 1)
            (draw-rose-petals s)
            (underlay
                (draw-rose-petals s)
                (rotate 45
                    (draw-rose-petals-nest (find-side-of-inner-square s) (- n 1))))))
    (overlay
        (draw-rose-petals-nest s n)
        (draw-rose-sepals s)))

(draw-rose-flower-with-recursion-with-frame-size s 3)
