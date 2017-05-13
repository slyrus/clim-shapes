
(defpackage #:clim-shapes
  (:use #:clim #:clim-lisp #:clim-extensions #:mcclim-bezier)
  (:export #:bezier-ellipsoid-2-coords
           #:bezier-ellipsoid-4-coords
           #:bezier-rectangle-coords

           #:bezier-notched-rectangle

           #:draw-notched-rectangle*

           #:draw-arrow-rectangle*
           #:draw-text-rectangle*
           #:draw-text-ellipse*

           #:draw-notched-text-rectangle*

           #:draw-grid))

(in-package #:clim-shapes)

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

(defun draw-notched-rectangle* (sheet x y width height radius &key ink)
  (let ((r1 (bezier-notched-rectangle x y width height radius)))
    (apply #'mcclim-bezier:draw-bezier-design* sheet r1
           (when ink
             `(:ink ,ink)))))

(defun draw-notched-text-rectangle* (sheet text x y width height radius &key ink text-style)
  (apply #'draw-notched-rectangle* sheet x y width height radius
         (when ink
           `(:ink ,ink)))
  (apply #'draw-text* sheet
         text
         (+ x (/ radius 4) (/ width 2))
         (+ y (/ height 2))
         :align-x :center
         :align-y :center
         (when text-style
           `(:text-style ,text-style))))

(defun draw-arrow-rectangle* (sheet x1 y1 x2 y2
                              &rest args
                              &key filled ink (line-thickness 0) (arrow-width 0.20) (arrow-width-unit :percent)
                                   text
                                   (text-style (make-text-style :sans-serif :bold :normal)))
  (declare (ignore args))
  (let ((box-x2
         (case arrow-width-unit
           (:percent (- x2 (* (- x2 x1)
                              arrow-width)))
           (:absolute (- x2 (* (signum (- x2 x1))
                               arrow-width))))))
    (clim:draw-polygon* sheet (print (list x1 y1
                                           box-x2 y1
                                           x2 (+ y1 (/ (- y2 y1) 2))
                                           box-x2 y2
                                           x1 y2))
                        :ink ink
                        :filled filled
                        :line-thickness line-thickness)
    (when text
      (multiple-value-bind (width height)
          (text-size sheet text :text-style text-style)
        (draw-text* sheet
                    text
                    (+ x1 (/ (- box-x2 x1) 2))
                    (+ y1 (/ (- y2 y1) 2))
                    :align-x :center
                    :align-y :center
                    :text-style text-style)
        (values width height)))))

(defun draw-text-rectangle* (sheet x1 y1 x2 y2
                             &rest args
                             &key filled ink (line-thickness 0)
                                  text
                                  (text-style (make-text-style :sans-serif :bold :normal)))
  (declare (ignore args))
  (clim:draw-rectangle* sheet x1 y1 x2 y2
                        :ink ink
                        :filled filled
                        :line-thickness line-thickness)
  (when text
    (draw-text* sheet
                text
                (+ x1 (/ (- x2 x1) 2))
                (+ y1 (/ (- y2 y1) 2))
                :align-x :center
                :align-y :center
                :text-style text-style)))

(defun remove-keyword-arg (key args)
  (loop for (k v) on args by #'cddr
     unless (eql key k)
     append (list k v)))

(defun draw-text-ellipse* (sheet
                          text
                          center-x center-y
                          radius-1-dx radius-1-dy radius-2-dx radius-2-dy
                          &rest args
                          &key (filled t) (start-angle 0.0) (end-angle (* 2.0 pi))
                               ink clipping-region transformation line-style
                               line-thickness line-unit line-dashes line-cap-shape text-style)
  (declare (ignore filled start-angle end-angle
                   ink clipping-region transformation line-style
                   line-thickness line-unit line-dashes line-cap-shape))
  (apply #'clim:draw-ellipse*
         sheet
         center-x center-y
         radius-1-dx radius-1-dy radius-2-dx radius-2-dy
         (remove-keyword-arg :text-style args))
  (when text
    (draw-text* sheet
                text
                center-x center-y
                :align-x :center
                :align-y :center
                :text-style text-style)))

(defun draw-grid (sheet &key (x 300) (y 300) (x-incr 10) (y-incr 10) (line-thickness 2) (ink +black+))
  (loop for i from 0 to x by x-incr
     do
       (draw-line* sheet i 0 i y :line-thickness line-thickness :ink ink))
  (loop for i from 0 to y by y-incr
     do
       (draw-line* sheet 0 i x i :line-thickness line-thickness :ink ink)))

(defun draw-regular-polygon (sheet x y sides radius
                             &rest args
                             &key (angle 0)
                                  (filled t) ink clipping-region
                                  transformation line-style line-thickness
                                  line-unit line-dashes line-joint-shape line-cap-shape)
  (declare (ignore ink clipping-region transformation line-style line-thickness filled
		   line-unit line-dashes line-joint-shape line-cap-shape))
  (let* ((coords (loop for i below sides
                    with theta1 = angle
                    for x1 = (+ x (* radius (sin theta1)))
                    for y1 = (+ y (* radius (cos theta1)))
                    do (incf theta1 (/ (* 2 pi) sides))
                    collect (list x1 y1)))
         (points (mapcar (lambda (x) (apply #'clim:make-point x))
                         coords)))
    (apply #'draw-polygon sheet points (remove-keyword-arg :angle args))))
