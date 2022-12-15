#lang racket

(require "../pt.rkt")

(module+ test
  (require rackunit))

(struct sensor (loc closest-beacon) #:transparent)

(define (sensor-x the-sensor)
  (pt-x (sensor-loc the-sensor)))

(define (sensor-y the-sensor)
  (pt-y (sensor-loc the-sensor)))

;; Parsing

(define (parse-point pt-str)
  (define coord-strings (regexp-match* #rx"-?[0-9]+" pt-str))
  (pt (string->number (first coord-strings))
      (string->number (second coord-strings))))

(define (parse-line line)
  (define point-strings (regexp-match* #rx"x=-?[0-9]+, y=-?[0-9]+" line))
  (sensor (parse-point (first point-strings))
          (parse-point (second point-strings))))

(define (parse-file filename)
  (map parse-line (file->lines filename)))

(define ex-input (parse-file "ex-input"))
(define my-input (parse-file "my-input"))

;; Part 1

(define (distance l r)
  (define delta (pt- l r))
  (+ (abs (pt-x delta))
     (abs (pt-y delta))))

(define (distance-to-sensor the-sensor point)
  (distance (sensor-loc the-sensor) point))

(define (distance-to-closest-beacon the-sensor)
  (distance-to-sensor the-sensor (sensor-closest-beacon the-sensor)))

(define (distance-to-y the-sensor the-y)
  (abs (- (sensor-y the-sensor) the-y)))

(define (covered-locations/at-y the-sensor the-y)
  (define beacon-dist (distance-to-closest-beacon the-sensor))
  (define y-dist (distance-to-y the-sensor the-y))
  (define spaces-toward-sensor (- beacon-dist y-dist))
  (define all-spaces
    (if (< spaces-toward-sensor 0)
      (set)
      (let* ([s-x (sensor-x the-sensor)]
             [xs (inclusive-range (- s-x spaces-toward-sensor)
                                  (+ s-x spaces-toward-sensor))])
        (list->set (map (λ (x) (pt x the-y)) xs)))))
  (set-subtract all-spaces (set (sensor-closest-beacon the-sensor)) ))

(module+ test
  (define s1 (sensor (pt 0 0) (pt 5 0)))
  (check-equal? (covered-locations/at-y s1 6)
                (set))
  (check-equal? (covered-locations/at-y s1 -6)
                (set))
  (check-equal? (covered-locations/at-y s1 5)
                (set (pt 0 5)))
  (check-equal? (covered-locations/at-y s1 3)
                (set (pt -2 3) (pt -1 3) (pt 0 3) (pt 1 3) (pt 2 3))))

(set-count (apply set-union (map (curryr covered-locations/at-y 10) ex-input)))
(set-count (apply set-union (map (curryr covered-locations/at-y 2000000) my-input)))

;; Part 2
