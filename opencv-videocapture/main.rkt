#lang racket/base

(provide IplImage?
	 IplImage-nSize
	 IplImage-ID
	 IplImage-nChannels
	 IplImage-alphaChannel
	 IplImage-depth
	 IplImage-colorModel
	 IplImage-channelSeq
	 IplImage-dataOrder
	 IplImage-origin
	 IplImage-align
	 IplImage-width
	 IplImage-height
	 IplImage-roi
	 IplImage-maskROT
	 IplImage-imageId
	 IplImage-tileInfo
	 IplImage-imageSize
	 IplImage-imageData
	 IplImage-widthStep
	 IplImage-BorderMode
	 IplImage-BorderConst
	 IplImage-imageDataOrigin

	 IplImage-pixel-data

	 cv-create-camera-capture
	 cv-get-frame-dimensions
	 cv-set-frame-dimensions!
	 cv-query-frame
	 cv-release-capture

	 cv-named-window
	 cv-show-image
	 cv-wait-key)

(require ffi/unsafe)
(require ffi/unsafe/define)
(require "pixel-data.rkt")

(define highgui-lib (ffi-lib "libopencv_highgui" '("2.4" #f)))

(define-ffi-definer define-highgui highgui-lib #:default-make-fail make-not-available)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define _char _uint8)

(define-cstruct _IplImage
  ([nSize _int]
   [ID _int] ;; should be 0
   [nChannels _int]
   [alphaChannel _int]
   [depth _int]
   [colorModel (_array _char 4)]
   [channelSeq (_array _char 4)]
   [dataOrder _int]
   [origin _int]
   [align _int]
   [width _int]
   [height _int]
   [roi _pointer] ;; IplROI *
   [maskROT _pointer] ;; IplImage *
   [imageId _pointer]
   [tileInfo _pointer] ;; IplTileInfo *
   [imageSize _int] ;; in bytes ((height * widthStep) for interleaved data, per header)
   [imageData _pointer]
   [widthStep _int]
   [BorderMode (_array _int 4)]
   [BorderConst (_array _int 4)]
   [imageDataOrigin _pointer]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (IplImage-pixel-data im)
  (pixel-data (make-sized-byte-string (IplImage-imageData im) (IplImage-imageSize im))
	      (IplImage-width im)
	      (IplImage-height im)
	      (IplImage-widthStep im)
	      (IplImage-nChannels im)
	      (positive? (IplImage-alphaChannel im))
	      (* (/ (IplImage-depth im) 8)
		 (+ (IplImage-nChannels im)
		    (IplImage-alphaChannel im)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define CV_CAP_PROP_FRAME_WIDTH 3)
(define CV_CAP_PROP_FRAME_HEIGHT 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-highgui cvCreateCameraCapture (_fun _int -> _pointer))
(define-highgui cvGetCaptureProperty (_fun _pointer _int -> _double))
(define-highgui cvSetCaptureProperty (_fun _pointer _int _double -> _int))
(define-highgui cvQueryFrame (_fun _pointer -> _IplImage-pointer))
(define-highgui cvReleaseCapture (_fun (_ptr io _pointer) -> _void))

(define-highgui cvNamedWindow (_fun _string _int -> _int))
(define-highgui cvShowImage (_fun _string _IplImage-pointer -> _void))

(define-highgui cvWaitKey (_fun _int -> _int))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cv-create-camera-capture [camera-index 0])
  (cvCreateCameraCapture camera-index))

(define (cv-get-frame-dimensions c)
  (values (inexact->exact (truncate (cvGetCaptureProperty c CV_CAP_PROP_FRAME_WIDTH)))
	  (inexact->exact (truncate (cvGetCaptureProperty c CV_CAP_PROP_FRAME_HEIGHT)))))

(define (cv-set-frame-dimensions! c width height)
  (cvSetCaptureProperty c CV_CAP_PROP_FRAME_WIDTH (exact->inexact width))
  (cvSetCaptureProperty c CV_CAP_PROP_FRAME_HEIGHT (exact->inexact height))
  (void))

(define (cv-query-frame c)
  (cvQueryFrame c))

(define (cv-release-capture c)
  (cvReleaseCapture c))

(define (cv-named-window window-name [flags 0])
  (cvNamedWindow window-name flags))

(define (cv-show-image window-name im)
  (cvShowImage window-name im))

(define (cv-wait-key millisecs)
  (define n (cvWaitKey millisecs))
  (and (positive? n) (integer->char n)))
