
;;
;; ***************************************************
;; Starter Code
;; ***************************************************
;;

;; Diamond Gesture Constants
(define diamond (list (list 100 0) (list 200 100) (list 100 200)
                      (list 0 100) (list 100 0)))
(define diamond-b-box (list (list 0 0) (list 200 200)))
(define diamond-length 565.68)
(define scaled-diamond (list (list 150 0) (list 300 250) (list 150 500)
                             (list 0 250) (list 150 0)))

;; Test Gesture
(define mygest (list (list 100 0) (list 200 100) (list 100 200)
                     (list 0 100) (list 100 50)))
(define mygest-b-box (list (list 0 0) (list 200 200)))
(define mygest-length 536.07)
(define scaled-mygest (list (list 50 0) (list 100 25) (list 50 50)
                            (list 0 25) (list 50 12.5)))

(define tolerance 0.01) ; tolerance for check-within tests

(require "templates.rkt")

;; "templates.rkt" provides templates, a TemplateLibrary (see data definition)
;; It also provides the following test gestures for your recognizer: 
;;    testd testk tests testy testa testt


;; A Point is a (list Num Num)

;; A Gesture is a (listof (list Num Num))

;; A BoundingBox (BB) is a (list Point Point)
;; requires: the coordinate values in the first point are less than the
;;             respective values in the second point

;; A TemplateLibrary (TL) is a (listof (list Sym Gesture))
;; requires: the list is non-empty
;;           each Sym key is unqiue
;;           each Gesture value is not both vertical and horizontal
       


;; 3a)
;; These are helper functions. See assignment for design recipe requirements.

;; (get-x point) consumes a Point and produces its x-coordinate.
;; Examples:
(check-expect (get-x (list 4 3)) 4)
(check-expect (get-x (list 0 1)) 0)

;; get-x: Point -> Num
(define (get-x point)
  (first point))


;; (get-y point) consumes a Point and produces its y-coordinate.
;; Examples:
(check-expect (get-y (list 4 3)) 3)
(check-expect (get-y (list 0 1)) 1)

;; get-y: Point -> Num
(define (get-y point)
  (second point))


;; (translate-gesture gesture x-offset y-offset) produces a Gesture such
;;    that each Point in gesture has x-offset added to each
;;    x-coordinate, and y-offset added to each y-coordinate.
;; Examples:
(check-expect (translate-gesture (list (list 4 3) (list 0 1)) 2 1)
              (list (list 6 4) (list 2 2)))
(check-expect (translate-gesture (list (list 0 0) (list 9 12)) 9 5)
              (list (list 9 5) (list 18 17)))

;; translate-gesture: Gesture Num Num -> Gesture
(define (translate-gesture gesture x-offset y-offset)
  (cond [(empty? gesture) empty]
        [else (cons (list (+ (get-x (first gesture)) x-offset)
                          (+ (get-y (first gesture)) y-offset))
                    (translate-gesture (rest gesture) x-offset y-offset))]))


;; (scale-gesture gesture x-scale y-scale) produces a new Gesture such that
;;    each Point in gesture has x-scale multiplied to each x-coordinate,
;;    and y-scale multiplied to each y-coordinate.
;; Examples:
(check-expect (scale-gesture (list (list 4 3) (list 0 1)) 2 1)
              (list (list 8 3) (list 0 1)))
(check-expect (scale-gesture (list (list 0 0) (list 9 12)) 9 5)
              (list (list 0 0) (list 81 60)))

;; scale-gesture: Gesture Num Num -> Gesture
(define (scale-gesture gesture x-scale y-scale)
  (cond [(empty? gesture) empty]
        [else (cons (list (* (get-x (first gesture)) x-scale)
                          (* (get-y (first gesture)) y-scale))
                    (scale-gesture (rest gesture) x-scale y-scale))]))


;; (get-b-box gesture) produces gesture's BoundingBox.
;; Examples:
(check-expect (get-b-box (list (list 4 3) (list 7 5) (list 2 7)
                               (list 4 3)))
              (list (list 2 3) (list 7 7)))
(check-expect (get-b-box diamond) diamond-b-box)
(check-expect (get-b-box mygest) mygest-b-box)

;; get-b-box: Gesture -> BoundingBox
;; Requires:
;;   gesture is non-empty.
(define (get-b-box gesture)
  (list (list (min-x gesture) (min-y gesture))
        (list (max-x gesture) (max-y gesture))))


;; (min-x gesture) returns the smallest x-coordinate of the Points in
;;    gesture.
;; Examples:
(check-expect (min-x (list (list 4 3))) 4)
(check-expect (min-x (list (list 4 3) (list 7 5) (list 2 1))) 2)
(check-expect (min-x (list (list 2 1) (list 4 3) (list 7 5))) 2)

;; min-x: Gesture -> Num
;; Requires:
;;   gesture is non-empty.
(define (min-x gesture)
  (cond [(empty? (rest gesture)) (get-x (first gesture))]
        [else (min (get-x (first gesture))
                   (min-x (rest gesture)))]))


;; (max-x gesture) returns the largest x-coordinate of the Points in
;;    gesture.
;; Examples:
(check-expect (max-x (list (list 4 3))) 4)
(check-expect (max-x (list (list 4 3) (list 7 5) (list 2 1))) 7)
(check-expect (max-x (list (list 2 1) (list 4 3) (list 7 5))) 7)

;; max-x: Gesture -> Num
;; Requires:
;;    gesture is non-empty
(define (max-x gesture)
  (cond [(empty? (rest gesture)) (get-x (first gesture))]
        [else (max (get-x (first gesture))
                   (max-x (rest gesture)))]))


;; (min-y gesture) returns the smallest y-coordinate of the Points in
;;    gesture.
;; Examples:
(check-expect (min-y (list (list 4 3))) 3)
(check-expect (min-y (list (list 4 3) (list 7 5) (list 2 1))) 1)
(check-expect (min-y (list (list 4 3) (list 4 3) (list 7 5))) 3)

;; min-y: Gesture -> Num
;; Requires:
;;   gesture is non-empty.
(define (min-y gesture)
  (cond [(empty? (rest gesture)) (get-y (first gesture))]
        [else (min (get-y (first gesture))
                   (min-y (rest gesture)))]))


;; (max-y gesture) returns the largest y-coordinate of the Points in
;;    gesture.
;; Examples:
(check-expect (max-y (list (list 4 3))) 3)
(check-expect (max-y (list (list 4 3) (list 7 5) (list 2 1))) 5)
(check-expect (max-y (list (list 2 1) (list 4 3) (list 7 3))) 3)

;; max-y: Gesture -> Num
;; Requires:
;;   gesture is non-empty.
(define (max-y gesture)
  (cond [(empty? (rest gesture)) (get-y (first gesture))]
        [else (max (get-y (first gesture))
                   (max-y (rest gesture)))]))

;; 3b)
;; Full design recipe required.

;; (gesture-length gesture) produces the sum of the distances between
;;    adjacent points in gesture.
;; Examples:
(check-within (gesture-length diamond) diamond-length tolerance)
(check-within (gesture-length mygest) mygest-length tolerance)

;; gesture-length: Gesture -> Num
(define (gesture-length gesture)
  (cond [(or (empty? gesture) (empty? (rest gesture))) 0]
        [else (+ (distance (first gesture) (second gesture))
                 (gesture-length (rest gesture)))]))

;; Tests:
(check-expect (gesture-length empty) 0)
(check-expect (gesture-length (list (list 7 5))) 0)
(check-expect (gesture-length (list (list 0 0) (list 0 100))) 100)
(check-expect (gesture-length (list (list 0 0) (list 100 0)
                                    (list 0 0))) 200)


;; (distance point1 point2) calculates the length of the line between
;;    point1 and point2.
;; Examples:
(check-expect (distance (list 0 0) (list 4 3)) 5)
(check-within (distance (list 2.5 0) (list 6/4 2)) (sqrt 5) tolerance)

;; distance: Point Point -> Num
(define (distance point1 point2)
  (sqrt (+ (sqr (- (get-x point2) (get-x point1)))
           (sqr (- (get-y point2) (get-y point1))))))


;; (get-points g lst) produces a new Gesture where each Point in the new
;;   Gesture is created by indexing g at each element of lst.
;; Examples:
(check-expect (get-points diamond (list 0 0 2))
              (list (list 100 0) (list 100 0) (list 100 200)))
(check-expect (get-points mygest (list 0 0 2 4 4))
              (list (list 100 0) (list 100 0) (list 100 200) (list 100 50)
                    (list 100 50)))

;; get-points: Gesture (listof Nat) -> Gesture
;; Requires:
;;   lst is a non-decreasing list.
;;   Gesture is non-empty.
;;   the numbers in lst are <= (length g)

(define (get-points g lst)
  (cond [(empty? lst) empty]
        [else (cons (get-point g (first lst))
                    (get-points g (rest lst)))]))

;; Tests:
(check-expect (get-points mygest (list 4 3 2 1 0))
              (list (list 100 50) (list 0 100) (list 100 200)
                    (list 200 100) (list 100 0)))
(check-expect (get-points diamond empty) empty)
(check-expect (get-points diamond (list 0)) (list (list 100 0)))
(check-expect (get-points (list (list 5 2) (list 1 9) (list 0.5 200))
                          (list 0 1 2))
              (list (list 5 2) (list 1 9) (list 0.5 200)))
(check-expect (get-points (list (list 9/6 9)) (list 0))
              (list (list 9/6 9)))
        

;; (get-point g num) returns the Point at index [num] in g.
;; Examples:
(check-expect (get-point diamond 0) (list 100 0))
(check-expect (get-point diamond 1) (list 200 100))
(check-expect (get-point diamond 3) (list 0 100))
(check-expect (get-point diamond 4) (list 100 0))

;; get-point: Gesture Nat -> Point
;; Requires:
;;   num <= length(g)
(define (get-point gesture num)
  (cond [(zero? num) (first gesture)]
        [else (get-point (rest gesture) (sub1 num))]))


;; 3c) Starter code definitions

;; 3ci)
;;(five-sample gesture) produces a sampling of gesture 5 points
;;  the first, n/4th, n/2th, 3n/4th, and last point.
;; Examples:
(check-expect (five-sample (list (list 1 1) (list 2 2)))
              (list (list 1 1) (list 1 1) (list 2 2) (list 2 2) (list 2 2)))
(check-expect (five-sample (list (list 1 1) (list 2 2) (list 3 3) (list 4 4)
                                 (list 5 5) (list 6 6) (list 7 7) (list 8 8)))
              (list (list 1 1) (list 3 3) (list 5 5) (list 7 7) (list 8 8)))

;; five-sample: Gesture -> Gesture
;; Requires:
;;   gesture is non-empty
(define (five-sample gesture)
  (list (first gesture)
        (index-gesture gesture 0.25 (length gesture))
        (index-gesture gesture 0.50 (length gesture))
        (index-gesture gesture 0.75 (length gesture))
        (get-point gesture (- (length gesture) 1))))

;; Tests:
(check-expect (five-sample (list (list 1 1) (list 2 2) (list 3 3)
                                 (list 4 4)))
              (list (list 1 1) (list 2 2) (list 3 3) (list 4 4)
                    (list 4 4)))

(check-expect (five-sample (list (list 1 1)))
              (list (list 1 1) (list 1 1) (list 1 1) (list 1 1)
                    (list 1 1)))

(check-expect (five-sample (list (list 1 1) (list 2 2) (list 3 3)
                                 (list 4 4) (list 5 5)))
              (list (list 1 1) (list 2 2) (list 3 3) (list 4 4)
                    (list 5 5)))

(check-expect (five-sample (list (list 100 200)))
              (list (list 100 200) (list 100 200) (list 100 200)
                    (list 100 200) (list 100 200)))

;; (index-gesture g factor l) produces the Point in g at index
;;   (floor * (factor l))
;; Examples:
(check-expect (index-gesture diamond 0.25 (length diamond))
              (list 200 100))

;; index-gesture: Gesture Num Nat -> Point
;; Requires:
;;    g is non-empty.
;;    (* factor l) <= (length g)
(define (index-gesture g factor l)
  (get-point g (floor (* factor l))))


;; 3cii)

;; Test Gesture for Move-and-Scale
(define test0 (list (list 1 5) (list 3 4)))
(define test0-origin (list (list 0 1) (list 2 0)))

;;(move-and-scale gesture x-scale y-scale) moves gesture to (0, 0) and
;;  scales it by (x-scale)x(y-scale)
;; Examples:
(check-expect (move-and-scale (list (list 1 1)) 1 1) (list (list 0 0)))
(check-expect (move-and-scale (list (list 1 5) (list 3 4)) 1 2)
              (list (list 0 2) (list 2 0)))

;; move-and-scale: Gesture Num Num -> Gesture
;; requires: gesture is non-empty
;;           x-scale > 0
;;           y-scale > 0
(define (move-and-scale gesture x-scale y-scale)
  (scale-gesture (move-origin gesture) x-scale y-scale))

;; Tests:
(check-expect (move-and-scale (list (list 5 5) (list 2 2)) 3 0.5)
              (list (list 9 1.5) (list 0 0)))
(check-expect (move-and-scale diamond 1.5 2.5) scaled-diamond)
(check-expect (move-and-scale mygest 0.5 0.25) scaled-mygest)


;; (move-origin gesture) produces gesture placed at the origin (0, 0).
;; Examples:
(check-expect (move-origin diamond) diamond) ; diamond is at the origin
(check-expect (move-origin test0) test0-origin)

;; move-origin: Gesture -> Gesture
(define (move-origin gesture)
  (move gesture (min-x gesture) (min-y gesture)))


;; (move gesture num1 num2) moves all the Points in gesture by num1 units
;;    to the left and num2 units up.
;; Examples:
(check-expect (move (list (list 2 3) (list 6 1) (list 9 8)) 2 1)
              (list (list 0 2) (list 4 0) (list 7 7)))
(check-expect (move (list (list 1 1)) 1 1) (list (list 0 0)))
(check-expect (move empty 5 4) empty)

;; move: Gesture Num Num -> Gesture
(define (move gesture num1 num2)
  (cond [(empty? gesture) empty]
        [else (cons (list (- (get-x (first gesture)) num1)
                          (- (get-y (first gesture)) num2))
                    (move (rest gesture) num1 num2))]))


;; 3ciii)

(define min-width 30)
(define min-height 30)
(define norm-size 200)

;;(normalize-gesture gesture) normalizes gesture to (0,0) and a standard
;;    size.
;; Examples:
(check-within (normalize-gesture (list (list 0 0) (list 100 100)))
              (list (list 0 0) (list 200 200)) tolerance)
(check-within (normalize-gesture (list (list 100 0) (list 100 50)
                                       (list 200 50)))
              (list (list 0 0) (list 0 200) (list 200 200)) tolerance)

;; normalize-gesture: Gesture -> Gesture
;; requires: gesture is not both vertical and horizontal
;;           gesture is non-empty
(define (normalize-gesture gesture)
  (cond [(vertical? gesture)
         (move-and-scale gesture 1 (/ norm-size (max-y (move-origin gesture))))]
        [(horizontal? gesture)
         (move-and-scale gesture (/ norm-size (max-x (move-origin gesture))) 1)]
        [else (move-and-scale gesture
                              (/ norm-size (max-x (move-origin gesture)))
                              (/ norm-size (max-y (move-origin gesture))))]))

;; Tests:
(check-within (normalize-gesture (list (list 0 0) (list 100 30)))
              (list (list 0 0) (list 200 200)) tolerance)
(check-within (normalize-gesture (list (list 0 0) (list 100 29)))
              (list (list 0 0) (list 200 29)) tolerance)
(check-within (normalize-gesture (list (list 0 0) (list 30 100)))
              (list (list 0 0) (list 200 200)) tolerance)
(check-within (normalize-gesture (list (list 0 0) (list 29 100)))
              (list (list 0 0) (list 29 200)) tolerance)
(check-within (normalize-gesture (list (list 0 0) (list 400 400)))
              (list (list 0 0) (list 200 200)) tolerance)


;; horizontal?: Gesture -> Bool
(define (horizontal? gesture)
  (< (- (max-y gesture) (min-y gesture)) min-height))

;; Tests:
(check-expect (horizontal? (list (list 0 0) (list 100 29))) true)
(check-expect (horizontal? (list (list 0 0) (list 100 30))) false)
(check-expect (horizontal? (list (list 0 0) (list 100 31))) false)


;; vertical?: Gesture -> Bool
(define (vertical? gesture)
  (< (- (max-x gesture) (min-y gesture)) min-width))

;; Tests:
(check-expect (vertical? (list (list 0 0) (list 29 100))) true)
(check-expect (vertical? (list (list 0 0) (list 30 100))) false)
(check-expect (vertical? (list (list 0 0) (list 31 100))) false)


;; 3civ)

(define 5match-length 4)
(define 5match-gesture1 (list (list 10 10) (list 30 30) (list 50 50)
                              (list 70 70) (list 80 80)))
(define 5match-gesture2 (list (list 10 10) (list 20 20) (list 30 30)
                              (list 40 40) (list 40 40)))
                                                  

;;(geometric-5match gesture1 gesture2) produces the average distance
;;  between points in sub-sampled gesture1 and gesture2 after sub-sampling
;;   them with 5 points
;; Examples:
(check-within (geometric-5match 5match-gesture1 5match-gesture2)
              16.16 tolerance)
(check-within (geometric-5match diamond mygest) 10 tolerance)

;; geometric-5match: Gesture Gesture -> Num
;; requires: gesture1 and gesture2 are not both vertical and horizontal
(define (geometric-5match gesture1 gesture2)
  (* 1/5 (sum-distances (normalize-gesture (five-sample gesture1))
                        (normalize-gesture (five-sample gesture2))
                        5match-length)))


;; (sum-distances gesture1 gesture2 n) produces the sums of the distances
;;   from index 0 to index n in gesture1 to index n in gesture2.
;; Examples:
(check-expect (sum-distances (list (list 0 0) (list 30 30))
                             (list (list 10 0) (list 40 30)) 1) 20)
(check-within (sum-distances 5match-gesture1 5match-gesture2 5match-length)
              141.42 tolerance)

;; sum-distances: Gesture Gesture Nat -> Num
;; Requires:
;;   n <= number of Points in gesture1
;;   gesture1 and gesture2 are equal in number of Points
(define (sum-distances gesture1 gesture2 n)
  (cond [(zero? n)
         (distance (get-point gesture1 n) (get-point gesture2 n))]
        [else (+ (distance (get-point gesture1 n) (get-point gesture2 n))
                 (sum-distances gesture1 gesture2 (sub1 n)))]))


;; 3cv)

;; A Dictionary is a (listof (list Sym Num))
;;   Requires: Dictionary is non-empty
;;             Each Sym key is unique


;;(five-point-rec candidate template-library) produces the symbol in
;;  template-library closest to candidate
;; Examples:
(check-expect (five-point-rec testd templates) 'd)
(check-expect (five-point-rec testk templates) 'k)

;; five-point-rec Gesture TL -> Sym
;; Requires:
;;   candidate is not both vertical and horizontal
(define (five-point-rec candidate template-library)
  (cond [(= (geometric-5match candidate (second (first template-library)))
            (min-val candidate template-library
                     (geometric-5match candidate
                                       (second (first template-library)))))
         (first (first template-library))]
        [else (five-point-rec candidate (rest template-library))]))

;; Tests
(check-expect (five-point-rec tests templates) 's)
(check-expect (five-point-rec testy templates) 'y)
(check-expect (five-point-rec testa templates) 'a)
(check-expect (five-point-rec testt templates) 't)


;; (min-val candidate template-library min-sofar) produces the minimum
;;    geometric-5match distance on each gesture in template-library when
;;    compared with candidate, min-sofar is generated as template-libary
;;    is called recursively.
;; Examples:
(check-within (min-val tests templates
                       (geometric-5match tests (second (first templates))))
              0 tolerance)
(check-within (min-val testa templates
                       (geometric-5match testa (second (first templates))))
              0 tolerance)

;; min-val: Gesture TL Num -> Num
;;   Requires:
;;   template-library is non-empty/
;;   candidate is non-empty.
;;   candidate is not both a horizontal and vertical line.
(define (min-val candidate template-library min-sofar)
  (cond [(empty? (rest template-library))
         (min min-sofar
              (geometric-5match candidate
                                (second (first template-library))))]
        [else
         (min-val candidate (rest template-library)
                  (min min-sofar
                       (geometric-5match candidate (second (second template-library)))))]))


;; 3d)

;; Test Gesture
(define 9-point-gesture (list (list 50 100) (list 65 120) (list 75 130)
                              (list 75 140) (list 70 160) (list 65 150)
                              (list 60 140) (list 55 120) (list 50 100)))
;; (sub-sample gesture k) produces a sampling of gesture with k points.
;; Examples:
(check-expect (sub-sample diamond 5) (five-sample diamond))
(check-expect (sub-sample mygest 5) (five-sample mygest))

;; sub-sample: Gesture Nat -> Gesture
;;   Requires: gesture is non-empty.
;;             k > 2
(define (sub-sample gesture k)
  (sample-k/n gesture 0 k))

;; Tests:
(check-expect (sub-sample diamond 3) (list (list 100 0) (list 100 200)
                                           (list 100 0)))
(check-expect (sub-sample 9-point-gesture 9) 9-point-gesture)
(check-within (sub-sample tests 10)
              (list (list 234 64.5) (list 160 51.5) (list 112 72.5)
                    (list 96 119.5) (list 129 165.5) (list 191 185.5)
                    (list 225 227.5) (list 219 271.5) (list 155 294.5) 
                    (list 116 295.5)) tolerance)


;; (sample-k/n gesture k n) produces a sampling of gesture from index [n]
;;    to index [k] points.
;; Examples:
(check-expect (sample-k/n diamond 0 5) (five-sample diamond))
(check-expect (sample-k/n mygest 0 5) (five-sample mygest))

;; sample-k/n: Gesture Nat Nat -> Gesture
;;  Requires:
;;     k < n
(define (sample-k/n gesture k n)
  (cond [(= k (- n 1)) (list (get-point gesture (- (length gesture) 1)))]
        [else (cons (index-gesture gesture (/ k (- n 1)) (length gesture))
                    (sample-k/n gesture (add1 k) n))]))


;;(geometric-match gesture1 gesture2 k) produces the average distance
;;  between points in sub-sampled gesture1 and gesture2 after sub-sampling
;;  them with k points
;; Examples:
(check-within (geometric-match diamond mygest 7) 14.29 tolerance)
(check-within (geometric-match tests testa 12) 111.94 tolerance)

;; geometric-match: Gesture Gesture Nat -> Num
;; Requires:
;;   gesture1 and gesture2 are each not both vertical and horizontal
;;   k > 2
(define (geometric-match gesture1 gesture2 k)
  (* (/ 1 k) (sum-distances (normalize-gesture (sub-sample gesture1 k))
                            (normalize-gesture (sub-sample gesture2 k))
                            (- k 1))))

;; Tests
(check-within (geometric-match (second (fourth templates))
                               (second (fourth templates)) 15) 0 tolerance)


;;(k-point-rec candidate template-library) produces the symbol in
;;  template-library closest to candidate
;; Examples:
(check-expect (k-point-rec tests templates 10) 's)
(check-expect (k-point-rec testa templates 10) 'a)
(check-expect (k-point-rec testt templates 15) 't)

;; k-point-rec Gesture TL Nat-> Sym
;; requires: candidate is not both vertical and horizontal
(define (k-point-rec candidate template-library k)
  (cond [(= (geometric-match candidate (second (first template-library)) k)
            (min-val candidate template-library
                     (geometric-match candidate (second (first template-library)) k)))
         (first (first template-library))]
        [else (k-point-rec candidate (rest template-library) k)]))

;; Tests
(check-expect (k-point-rec testy templates 15) 'y)
(check-expect (k-point-rec testd templates 15) 'd)
(check-expect (k-point-rec testk templates 15) 'k)
