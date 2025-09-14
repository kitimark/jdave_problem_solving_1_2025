#lang racket

(require 2htdp/image)

(define (half x) (/ x 2))

(define s 500)
(define e (exp 1))
(define star-yellow (circle 10 "solid" (color 253 238 0 50)))
(define star-blue (circle 10 "solid" (color 137 207 240 50)))
(define star-red (circle 10 "solid" (color 255 36 0 50)))
(define star-green (circle 10 "solid" (color 192 255 0 50)))

(define background-outline (circle (half s) "outline" (color 255 255 255 50)))
(define background-color (circle (half s) "outline" (color 0 0 0 100)))
(define background
    (overlay
        background-outline
        background-color))

(define a 1000)
(define b 0.015)
(define theta_p 46.5)
(define diff 0.075)

; sprial shape
; r(θ) = ae^(bθ)
(define (find-sprial-position-by-theta-on-x theta)
    (*
        (expt (* a e) (* b theta))
        (cos theta)))
(define (find-sprial-position-by-theta-on-y theta)
    (*
        (expt (* a e) (* b theta))
        (sin theta)))

(define (draw-spiral-recur star theta m bg)
    (if (< theta 10)
        bg
        (overlay/offset
            star
            (find-sprial-position-by-theta-on-x theta)
            (find-sprial-position-by-theta-on-y theta)
            (draw-spiral-recur star (- theta m) m bg))))

; Question: draw any images with 2htdp/image
;
; This is sprials abstract art
(define draw-image
    (overlay
        (overlay
            (rotate 315
                (draw-spiral-recur star-green theta_p diff background))
            (rotate 135
                (draw-spiral-recur star-green theta_p diff background)))
        (overlay
            (overlay
                (rotate 225
                    (draw-spiral-recur star-red theta_p diff background))
                (rotate 45
                    (draw-spiral-recur star-red theta_p diff background)))
            (overlay
                (overlay
                    (rotate 270
                        (draw-spiral-recur star-blue theta_p diff background))
                    (rotate 90
                        (draw-spiral-recur star-blue theta_p diff background)))
                (overlay
                    (rotate 180
                        (draw-spiral-recur star-yellow theta_p diff background))
                    (draw-spiral-recur star-yellow theta_p diff background))))))

draw-image
