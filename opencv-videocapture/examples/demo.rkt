#lang racket

(require "../main.rkt")

(define c (cv-create-camera-capture))
(cv-set-frame-dimensions! c 160 120)
(cv-named-window "test")

(define (dump-image-info im)
  (printf "size ~a x ~a\n" (IplImage-width im) (IplImage-height im))
  (printf "widthStep ~a\n" (IplImage-widthStep im))
  (printf "nChannels ~a, alphaChannel ~a, depth ~a\n"
	  (IplImage-nChannels im)
	  (IplImage-alphaChannel im)
	  (IplImage-depth im))
  (define bs (IplImage-pixel-bytes im))
  (define pixel-bytes (* (/ (IplImage-depth im) 8)
			 (+ (IplImage-nChannels im)
			    (IplImage-alphaChannel im))))
  (printf "~a bytes of data; first ~a bytes ~a\n"
	  (bytes-length bs)
	  pixel-bytes
	  (bytes->list (subbytes bs 0 pixel-bytes)))
  (newline))

(define start-time (current-inexact-milliseconds))

(void
 (let loop ((count 0))
   (define im (cv-query-frame c))
   (cv-show-image "test" im)

   (define now (current-inexact-milliseconds))
   (define delta (/ (- now start-time) 1000.0))
   (printf "~a frames/sec\n" (/ count delta))
   (dump-image-info im)

   (unless (cv-wait-key 5)
     (loop (+ count 1)))))

(cv-release-capture c)
