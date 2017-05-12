
(defpackage #:bezier-shapes
  (:use #:clim #:clim-lisp #:clim-extensions #:mcclim-bezier)
  (:export #:bezier-ellipsoid-2-coords
           #:bezier-ellipsoid-4-coords
           #:bezier-rectangle-coords

           #:bezier-notched-rectangle

           #:draw-notched-rectangle))

(in-package #:bezier-shapes)

(defun bezier-ellipsoid-2-coords (x y width height &key (tallness 1))
  (let ((y-factor (* height (/ tallness (sqrt 2)))))
    (mcclim-bezier:relative-to-absolute-coord-seq (list x y
                                                        0 (- y-factor)
                                                        0 (- y-factor)
                                                        width 0
                                                        0 y-factor
                                                        0 y-factor
                                                        (- width) 0))))

(defun bezier-ellipsoid-4-coords (x y x-radius y-radius &key (x-stretch 1) (y-stretch 1))
  (let ((x-round (* x-stretch x-radius 0.551915024494))
        (y-round (* y-stretch y-radius 0.551915024494)))
    (mcclim-bezier:relative-to-absolute-coord-seq
     (list (- x x-radius) y
           0 (- y-round)
           (- x-round) 0
           x-radius (- y-radius)
           x-round 0
           0 (- y-round)
           x-radius y-radius
           0 y-round
           x-round 0
           (- x-radius) y-radius
           (- x-round) 0
           0 y-round
           (- x-radius) (- y-radius)))))

(defun bezier-rectangle-coords (x1 y1 x2 y2)
  (mcclim-bezier:relative-to-absolute-coord-seq (list x1 y1 0 0
                                                      0 0 (- x2 x1) 0 0 0
                                                      0 0 0 (- y2 y1) 0 0
                                                      0 0 (- x1 x2) 0 0 0
                                                      0 0 0 (- y1 y2))))

(defun bezier-notched-rectangle (x y width height radius)
  (let* ((p1 (make-bezier-area* (bezier-ellipsoid-4-coords x (+ y (/ height 2)) radius radius)))
         (p2 (make-bezier-area* (bezier-rectangle-coords x y (+ x width) (+ y height)))))
    (mcclim-bezier:region-difference p2 p1)))

(defun draw-notched-rectangle (sheet x y width height radius &key ink)
  (let ((r1 (bezier-notched-rectangle x y width height radius)))
    (apply #'mcclim-bezier:draw-bezier-design* sheet r1
           (when ink
             `(:ink ,ink)))))
