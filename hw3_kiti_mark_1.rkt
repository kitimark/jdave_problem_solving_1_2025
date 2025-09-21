; 3st homework of 6th class
#lang racket

(require 2htdp/image)

(struct struct_rose_circle (radius) #:transparent)
(struct struct_rose_square (side) #:transparent)

(define (half n) (/ n 2))
(define (inner_rose_circle_size frame_size)
  (half frame_size))
(define (hypotenuse a b) (sqrt (+ (sqr a) (sqr b))))
(define (hypotenuse_equal a) (hypotenuse a a))
(define (inner_rose_square_size frame_size)
  (hypotenuse_equal (half frame_size)))

(struct struct_rose
  (struct_rose_square
   struct_inner_rose_circle
   struct_inner_rose_square) #:transparent)

(define (create_struct_rose frame_size)
  (struct_rose (struct_rose_square frame_size)
               (struct_rose_circle (inner_rose_circle_size frame_size))
               (struct_rose_square (inner_rose_square_size frame_size))))

(define (draw_bordered_shape shape shape_size fill_color border_color)
  (underlay (shape shape_size "solid" fill_color)
            (shape shape_size "outline" border_color)))

(define (draw_white_bordered_shape shape shape_size fill_color)
  (draw_bordered_shape shape shape_size fill_color "white"))

(define (draw_bordered_red_circle struct_rose_circle)
  (draw_white_bordered_shape circle (struct_rose_circle-radius struct_rose_circle) "light red"))

(define (draw_bordered_green_square struct_rose_square)
  (draw_white_bordered_shape square (struct_rose_square-side struct_rose_square) "seagreen"))

(define (tilt picture) (rotate 45 picture))

(define (draw_rose struct_rose)
  (underlay (draw_bordered_green_square (struct_rose-struct_rose_square struct_rose))
            (draw_bordered_red_circle (struct_rose-struct_inner_rose_circle struct_rose))
            (tilt
             (draw_bordered_green_square (struct_rose-struct_inner_rose_square struct_rose)))))

(draw_rose (create_struct_rose 200))
(draw_rose (create_struct_rose (inner_rose_square_size 200)))

; question 1, propose 1; draw the rose shape with infinite level
; draw_rose_recursive_with_level is rose drawer with specific level that's how much of rose
;   we would like to draw
(define (draw_rose_recursive_with_level frame_size level)
  (define (draw_rose_nest frame_size level)
    (if (= level 0)
        (draw_rose (create_struct_rose 0))
        (underlay
         (draw_rose (create_struct_rose frame_size))
         (tilt (draw_rose_nest (inner_rose_square_size frame_size) (- level 1))))))
  (draw_rose_nest frame_size level))

(draw_rose_recursive_with_level 200 3)
(draw_rose_recursive_with_level 200 10)

; (draw_bordered_green_square (struct_rose_square 100))
; (draw_bordered_green_square (struct_rose_square 10))
; (draw_bordered_green_square (struct_rose_square 1))
; (draw_bordered_green_square (struct_rose_square 0.1))

; question 1, propose 2; draw the rose shape with infinite level
; draw_rose_recursive_with_lowest_pixel is rose drawer with recursion of posibility that's
;   it can display. assume the lowset unit of screen that can display is 1 pixel.
(define (draw_rose_recursive_with_lowest_pixel frame_size)
  (define (draw_rose_nest frame_size)
    (if (< frame_size 1)
        (draw_rose (create_struct_rose 0))
        (underlay
         (draw_rose (create_struct_rose frame_size))
         (tilt (draw_rose_nest (inner_rose_square_size frame_size)))))
    )
  (draw_rose_nest frame_size))

(draw_rose_recursive_with_lowest_pixel 200)
(draw_rose_recursive_with_lowest_pixel 500)
