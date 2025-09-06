; 1st homework of 2nd class
#lang racket

; export all of functions of this module for running in the console
(provide (all-defined-out))

(require rackunit)

(define (power_2 x) (expt x 2))

; # question 1; find circle area, given r is radius
(define (circle_area r)
    (* pi (power_2 r)))

; it is check result is pi * r^2 or not
; i'd like to keep pi is constant variable
; the expection; i'd like to know
; the circle_area function is return pi * (power 2 of radius) or not
(check-equal? (circle_area 1) pi "find circle area")
(check-equal? (circle_area 2) (* pi 4) "find circle area")
(check-equal? (circle_area 7) (* pi 49) "find circle area")
(check-equal? (circle_area (/ 4 2)) (* pi 4) "find circle area")

; # question 2; find square area, given s is side of square
(define (square_area s)
    (power_2 s))

(check-equal? (square_area 1) 1 "find square area")
(check-equal? (square_area 3) 9 "find square area")
(check-equal? (square_area 7) 49 "find square area")

; # question 3; find donut area, given r1 and r2 is redius of circles
;
; ## solution throught (try problem solving):
; this question doesn't given r1 is always more than r2
; we assume the larger circle should depends on which radius is largest
;
; the *donut_area* is large circle minus small circle
;   donut_area = large_circle - small_circle
; what if r1 is larger than r2, there will be;
;   donut_area = circle_area(r1) - circle_area(r2); r1 >= r2 -> e_1
; in the other hand, r2 is larger than r1, there will be;
;   donut_area' = circle_area(r2) - circle_area(r1); r2 >= r1 -> e_2
; if we take minus into e_2
;   -donut_area' = circle_area(r1) - circle_area(r2); r2 >= r1
;
; we will get minus of donut_area' but equation is close to e_1
; try to take absolute into this equation
;   | -donut_area' | = | circle_area(r1) - circle_area(r2) | ; r2 >= r1
; NOTE: i don't know, how to justify reasonable this assumption?
;   donut_area' = | circle_area(r1) - circle_area(r2) | ; r2 >= r1
;   donut_area = | -donut_area' |
; therefore, the donut area is absolute of circle_area(r1) - circle_area(r2)
;   donut_area = | circle_area(r1) - circle_area(r2) |
(define (donut_area r1 r2)
    (abs (- (circle_area r1) (circle_area r2))))

; test thought;
;   donut_area = | pi * r1^2 - pi * r2^2 |
;   donut_area = | pi * (r1^2 - r2^2) |
; pi is positive number
;   donut_area = pi * | (r1^2 - r2^2) |
; test expect: the value is pi * expect_value or not?
;   the expect_value should be | r1^2 - r2^2 |
;
; expect: | 2^2 - 1^2 | = 3
(check-equal? (donut_area 2 1) (* pi 3) "find donut area")
(check-equal? (donut_area 1 2) (* pi 3) "find donut area")

; expect | 3^2 - 2^2 | = 5
(check-equal? (donut_area 3 2) (* pi 5) "find donut area")
(check-equal? (donut_area 2 3) (* pi 5) "find donut area")

; # question 4; find this region area
; given, a large *outer square* has side length is *s*.
; and, *inner square* is placed such that its vertices lie on the midpoints of the *outer square*’s sides.
; and, a *circle* is then drawn so that it passes through the vertices of the inner square.
;
; find the *region area* that's inside a *circle* but outside the *inner square*
;
; ## solution throught (try problem solving):
; given
; - *s* is side length of *outer square*
; - *q4_outer_area* is area of outer sqaure
;
; NOTE: q4_* is stand for function of 4th question that related on *s* length
;   such as q4_circle_area is circle area that depends on *s* side length of *outer square*
;
; the region area inside *circle* but outside the *inner square* is;
;   q4_region_area(s) = q4_circle_area(s) - q4_inner_area(s)
(define (q4_region_area s)
    (- (q4_circle_area s) (q4_inner_area s)))

; find the *q4_region_area*
; the *s* length is diameter of circle
; therefore, the *q4_region_area* is circle has radius of *s*/2 (s is diameter of circle)
;   q4_region_area(s) = circle_area(s/2)
(define (q4_circle_area s)
    (circle_area (/ s 2)))

; find the *q4_inner_area*
; if we divide *q4_inner_area* with 2 diagonals, we will get 4 triangle inside of *q4_inner_area*.
; and each triangle area will be match each triangle area of *difference area* of *outer square*
; placed vertices lie on the midpoints of the *outer squarea*’s sides
; therefore,
;   area of *inner sqaure* is equal to area outside *inner square* but inside *outer square*
;   the *q4_inner_area* is *outer sqaure*/2
;       q4_inner_area(s) = q4_outer_area(s)/2
;       q4_inner_area(s) = square_area(s)/2
(define (q4_inner_area s)
    (/ (square_area s) 2))

; test thought;
; NOTE: i don't know, how to test it?
; but i'd like to make sure the equation of q4_region_area, does it rely on my proof or not?
; if we attach the pi variable
;   q4_region_area(s) = q4_circle_area(s) - q4_inner_area(s) -> e_1
;   q4_circle_area(s) = pi * (s/2)^2 -> e_2
;   q4_inner_area(s) = (s^2)/2 -> e_3
; assign e_2 and e_3 into e_1
;   q4_region_area(s) = (pi * (s/2)^2) - (s^2)/2
;
; s = 2; q4_region_area(2) = (pi * (2/2)^2) - (2^2)/2
; s = 2; q4_region_area(2) = (pi * 1) - 2
(check-equal? (q4_region_area 2) (- (* pi 1) 2) "find region result attach the pi variable")

; s = 4; q4_region_area(4) = (pi * (4/2)^2) - (4^2)/2
; s = 4; q4_region_area(4) = (pi * 4) - 8
(check-equal? (q4_region_area 4) (- (* pi 4) 8) "find region result attach the pi variable")

; s = 6; q4_region_area(6) = (pi * (6/2)^2) - (6^2)/2
; s = 6; q4_region_area(6) = (pi * 9) - 18
(check-equal? (q4_region_area 6) (- (* pi 9) 18) "find region result attach the pi variable")
