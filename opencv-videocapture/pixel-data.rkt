#lang racket/base

(provide (struct-out pixel-data)
	 pixel-offset
	 get-pixel
	 set-pixel!
	 clone-pixel-data)

(struct pixel-data (bytes width height stride channel-count alpha-channel? bytes-per-pixel)
	#:transparent)

(define (pixel-offset pd x y)
  (+ (* y (pixel-data-stride pd))
     (* x (pixel-data-bytes-per-pixel pd))))

(define (get-pixel pd x y [big-endian? (system-big-endian?)])
  (bytes->unsigned-integer (pixel-data-bytes pd)
			   (pixel-offset pd x y)
			   (pixel-data-bytes-per-pixel pd)
			   big-endian?))

(define (set-pixel! pd x y v [big-endian? (system-big-endian?)])
  (integer->bytes! (pixel-data-bytes pd)
		   (pixel-offset pd x y)
		   (pixel-data-bytes-per-pixel pd)
		   big-endian?
		   v))

(define (clone-pixel-data pd)
  (define bs (make-bytes (bytes-length (pixel-data-bytes pd))))
  (bytes-copy! bs 0 (pixel-data-bytes pd))
  (struct-copy pixel-data pd [bytes bs]))

(define (bytes->unsigned-integer bs offset count big-endian?)
  (if big-endian?
      (do ((i 0 (+ i 1))
	   (acc 0 (bitwise-ior (arithmetic-shift acc 8) (bytes-ref bs (+ offset i)))))
	  ((= i count) acc))
      (do ((i (- count 1) (- i 1))
	   (acc 0 (bitwise-ior (arithmetic-shift acc 8) (bytes-ref bs (+ offset i)))))
	  ((< i 0) acc))))

(define (integer->bytes count big-endian? n)
  (define bs (make-bytes count))
  (integer->bytes! bs 0 count big-endian? n)
  bs)

(define (integer->bytes! bs offset count big-endian? n)
  (if big-endian?
      (do ((i (- count 1) (- i 1))
	   (n n (arithmetic-shift n -8)))
	  ((< i 0) (void))
	(bytes-set! bs (+ offset i) (bitwise-and n 255)))
      (do ((i 0 (+ i 1))
	   (n n (arithmetic-shift n -8)))
	  ((= i count) (void))
	(bytes-set! bs (+ offset i) (bitwise-and n 255)))))

(module+ test
  (require rackunit)
  (check-equal? (bytes->unsigned-integer (bytes #x34 #x12) 0 2 #f) #x1234)
  (check-equal? (bytes->unsigned-integer (bytes #x34 #x12) 0 2 #t) #x3412)
  (check-equal? (integer->bytes 2 #f #x1234) (bytes #x34 #x12))
  (check-equal? (integer->bytes 2 #t #x3412) (bytes #x34 #x12)))
