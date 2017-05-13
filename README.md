# clim-shapes

A library of various shapes for use with implementation of the CLIM II
interface manager library, particulary McCLIM.


## bezier-ellipsoid-2-coords

```
bezier-ellipsoid-2-coords (x y width height &key (tallness 1))
```

## bezier-ellipsoid-4-coords
```
bezier-ellipsoid-4-coords (x y x-radius y-radius &key (x-stretch 1) (y-stretch 1))
```

## bezier-rectangle-coords
```
bezier-rectangle-coords (x1 y1 x2 y2)
```

## bezier-notched-rectangle
```
bezier-notched-rectangle (x y width height radius)
```

## draw-notched-rectangle*
```
draw-notched-rectangle* (sheet x y width height radius &key ink)
```

## draw-arrow-rectangle*
```
 draw-arrow-rectangle* (sheet x1 y1 x2 y2
                              &rest args
                              &key filled ink (line-thickness 0) (arrow-width 0.20) (arrow-width-unit :percent)
                                   text
                                   (text-style (make-text-style :sans-serif :bold :normal)))```

## draw-text-rectangle*
```
draw-text-rectangle* (sheet x1 y1 x2 y2
                             &rest args
                             &key filled ink (line-thickness 0)
                                  text
                                  (text-style (make-text-style :sans-serif :bold :normal)))
                                  ```

## draw-text-ellipse*
```
draw-text-ellipse* (sheet
                          text
                          center-x center-y
                          radius-1-dx radius-1-dy radius-2-dx radius-2-dy
                          &rest args
                          &key (filled t) (start-angle 0.0) (end-angle (* 2.0 pi))
                               ink clipping-region transformation line-style
                               line-thickness line-unit line-dashes line-cap-shape text-style)
```

## draw-notched-text-rectangle*
```
draw-notched-text-rectangle* (sheet text x y width height radius &key ink text-style)
```

## draw-grid
```
draw-grid (sheet &key (x 300) (y 300) (x-incr 10) (y-incr 10) (line-thickness 2) (ink +black+))
```
