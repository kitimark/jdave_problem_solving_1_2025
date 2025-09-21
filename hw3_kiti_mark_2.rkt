; 3st homework of 6th class
#lang racket

(struct struct_rose_circle (radius) #:transparent)
(struct struct_rose_square (side) #:transparent)

(define (half n) (/ n 2))
(define (double x) (* x 2))
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

(define (calculate_circle_area struct_rose_circle)
  (* pi (sqr (struct_rose_circle-radius struct_rose_circle))))

(define (calculate_square_area struct_rose_square)
  (sqr (struct_rose_square-side struct_rose_square)))

(define (calculate_red_rose_region_area struct_rose)
  (- (calculate_circle_area (struct_rose-struct_inner_rose_circle struct_rose))
     (calculate_square_area (struct_rose-struct_inner_rose_square struct_rose))))

(create_struct_rose 100)
(calculate_circle_area (struct_rose-struct_inner_rose_circle (create_struct_rose 100)))
(calculate_square_area (struct_rose-struct_inner_rose_square (create_struct_rose 100)))
(calculate_red_rose_region_area (create_struct_rose 100))

; question 2, propose 1; calculate the red rose area with infinite level
; calculate_red_rose_region_recursive_with_lowest_pixel returns area with recursion level
(define (calculate_red_rose_region_recursive_with_level frame_size level)
  (define (calculate_red_rose_region_nest frame_size level)
    (if (= level 0)
        (calculate_red_rose_region_area (create_struct_rose 0))
        (+ (calculate_red_rose_region_area (create_struct_rose frame_size))
           (calculate_red_rose_region_nest (inner_rose_square_size frame_size) (- level 1)))))
  (calculate_red_rose_region_nest frame_size level))

; (calculate_red_rose_region_recursive_with_level 100 1)
; (calculate_red_rose_region_recursive_with_level 100 2)
; (calculate_red_rose_region_recursive_with_level 100 3)
(calculate_red_rose_region_recursive_with_level 100 10)

; question 2, propose 2; calculate the red rose area with infinite level
; calculate_red_rose_region_recursive_with_lowest_pixel returns area with recursion untils inner frame size lower than zero
(define (calculate_red_rose_region_recursive_with_lowest_pixel frame_size)
  (define (calculate_red_rose_region_nest frame_size)
    (if (< frame_size 1)
        (calculate_red_rose_region_area (create_struct_rose 0))
        (+ (calculate_red_rose_region_area (create_struct_rose frame_size))
           (calculate_red_rose_region_nest (inner_rose_square_size frame_size)))))
  (calculate_red_rose_region_nest frame_size))

(calculate_red_rose_region_recursive_with_lowest_pixel 100)
(calculate_red_rose_region_recursive_with_lowest_pixel 500)

(calculate_red_rose_region_recursive_with_level 100 1)
(calculate_red_rose_region_recursive_with_level 100 2)
(calculate_red_rose_region_recursive_with_level 100 3)
(calculate_red_rose_region_recursive_with_level 100 256)
(calculate_red_rose_region_recursive_with_level 100 1024)
(calculate_red_rose_region_recursive_with_level 100 2048)
(calculate_red_rose_region_recursive_with_level 100 4096)
(calculate_red_rose_region_recursive_with_level 100 8192)
(calculate_red_rose_region_recursive_with_level 100 16384)

(calculate_red_rose_region_area (create_struct_rose 100))
(calculate_red_rose_region_area (create_struct_rose (inner_rose_square_size 100)))
(calculate_red_rose_region_area (create_struct_rose (inner_rose_square_size (inner_rose_square_size 100))))

; question 2, propose 3; calculate the red rose area with infinite level
; calculate_red_rose_region_area_with_half_area_sum_approx returns red rose region area size
; we found the lower level each rose region will be a half of upper level
; for example;
; (/ (calculate_red_rose_region_area (create_struct_rose 100))
;    (calculate_red_rose_region_area (create_struct_rose (inner_rose_square_size 100))))
; 2853.981633974483 / 1426.9908169872415 = 2
;
; (/ (calculate_red_rose_region_area (create_struct_rose (inner_rose_square_size 100)))
;    (calculate_red_rose_region_area (create_struct_rose (inner_rose_square_size (inner_rose_square_size 100))))) --
; 1426.9908169872415 / 713.4954084936207 = 2
;
; therefore;
;   the red rose region area will be
;   red_rose_area(x) = red_rose_area_level(x) + (red_rose_area_level(x) / 2) + (red_rose_area_level(x) / 4) + ... + (red_rose_area_level(x) / (2n))
;   red_rose_area(x) = red_rose_area_level(x) * (1 + 1/2 + 1/4 ... + 1/(2n))
;   red_rose_area(x) = red_rose_area_level(x) * sum(n = 0 => infinity)((1/2)^n)
;   red_rose_area(x) = red_rose_area_level(x) * 2
;
; this is fastest and posible way to calculate area with infinity assumption
; because it's no need to calculate with recursion steps
(define (calculate_red_rose_region_area_with_half_area_sum_approx frame_size)
  (double (calculate_red_rose_region_area (create_struct_rose frame_size))))

(calculate_red_rose_region_area_with_half_area_sum_approx 100)
