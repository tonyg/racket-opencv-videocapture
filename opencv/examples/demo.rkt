#lang racket

(require "../main.rkt")

(define c (cv-create-camera-capture))
(cv-set-frame-dimensions! c 160 120)
(cv-named-window "test")

(define (dump-image-info im)
  (printf "height ~a\n" (IplImage-height im))
  (printf "width ~a\n" (IplImage-width im))
  (printf "widthStep ~a\n" (IplImage-widthStep im))
  (printf "nChannels ~a\n" (IplImage-nChannels im))
  (printf "depth ~a\n" (IplImage-depth im))
  (printf "imageData ~a\n" (IplImage-imageData im))
  (printf "imageSize ~a\n" (IplImage-imageSize im))
  (newline))

(void
 (let loop ()
   (define im (cv-query-frame c))
   (dump-image-info im)
   (cv-show-image "test" im)
   (unless (cv-wait-key 5) (loop))))

(cv-release-capture c)
