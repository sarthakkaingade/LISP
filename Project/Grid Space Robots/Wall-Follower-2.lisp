(setf gridsize '(6 6))
(setf SI '(s1 s2 s3 s4 s5 s6 s7 s8))
(setf FV '(x1 x2 x3 x4))
(setf outer-boundary '(1 6 1 6))

;Boolean OR function
(defun my-or (x y) (prog ()
                     (if (= x 1) (return 1))
                     (if (= y 1) (return 1))
                     (return 0)
                    )
)

;Boolean NOT function
(defun my-not (l) (if (= l 0) 1 0))

;Boolean AND function
(defun my-and (x y) (prog ()
                      (if (= x 0) (return nil))
                      (if (= y 0) (return nil))
                      (return 1)
                    )
)

;Function to calculate feature vector
;INPUTS - Sensor values lsit (s1 s2 s3 s4 s5 s6 s7 s8)
;OUTPUTS - Feature vector list (x1 x2 x3 x4)
(defun calc-feature-vector-2 (inputs) (prog (x1 x2 x3 x4)
                                        (if (not (= (length inputs) (length SI))) (return nil))
                                        (setq x1 (my-or (nth 0 inputs) (nth 7 inputs)))
                                        (setq x2 (my-or (nth 5 inputs) (nth 6 inputs)))
                                        (setq x3 (my-or (nth 3 inputs) (nth 4 inputs)))
                                        (setq x4 (my-or (nth 1 inputs) (nth 2 inputs)))
                                        (return (list x1 x2 x3 x4))
                                      )
)

;Function to simulate robot sensors
;INPUTS - Current location
;OUTPUTS - Sensor values lsit (s1 s2 s3 s4 s5 s6 s7 s8)
(defun calc-sensors-2 (currentloc) (prog (sensors s1 s2 s3 s4 s5 s6 s7 s8)
                                     (setq s1 0 s2 0 s3 0 s4 0 s5 0 s6 0 s7 0 s8 0)
                                     (if (equal (first currentloc) (first outer-boundary)) (go first-row))
                               loop1
                                     (if (equal (first currentloc) (second outer-boundary)) (go last-row))
                               loop2
                                     (if (equal (second currentloc) (third outer-boundary)) (go first-column))
                               loop3
                                     (if (equal (second currentloc) (fourth outer-boundary)) (go last-column))
                                 out
                                     (return (list s1 s2 s3 s4 s5 s6 s7 s8))
                           first-row
                                     (setq s1 1 s8 1 s7 1)
                                     (go loop1)
                            last-row
                                     (setq s3 1 s4 1 s5 1)
                                     (go loop2)
                        first-column
                                     (setq s1 1 s2 1 s3 1)
                                     (go loop3)
                         last-column
                                     (setq s5 1 s6 1 s7 1)
                                     (go out)
                                   )
)

;Function to calculate actions based on production rules
;INPUTS - Feature vector list (x1 x2 x3 x4)
;OUTPUTS - Action North / South / East / West
(defun calc-navigation-command-2 (featurevector) (prog ()
                                                   (if (my-and (second featurevector) (my-not (first featurevector))) (return 'north))
                                                   (if (my-and (first featurevector) (my-not (fourth featurevector))) (return 'west))
                                                   (if (my-and (fourth featurevector) (my-not (third featurevector))) (return 'south))
                                                   (if (my-and (third featurevector) (my-not (second featurevector))) (return 'east))
                                                   (return 'north)
                                                 )
)

;Function to update current location
;INPUTS - Current location, Action
;OUTPUTS - Updated location
(defun update-current-location (currentloc command) (prog ()
                                                      (if (equal command 'north) (return (list (- (first currentloc) 1) (second currentloc))))
                                                      (if (equal command 'south) (return (list (+ (first currentloc) 1) (second currentloc))))
                                                      (if (equal command 'east) (return (list (first currentloc) (+ (second currentloc) 1))))
                                                      (if (equal command 'west) (return (list (first currentloc) (- (second currentloc) 1))))
                                                     )
)

;Function to check boundary
;INPUTS - Current location
;OUTPUTS - True / False
(defun checkboundary (currentloc) (prog ()
                                    (if (equal (first currentloc) (first outer-boundary)) (return 'true))
                                    (if (equal (first currentloc) (second outer-boundary)) (return 'true))
                                    (if (equal (second currentloc) (third outer-boundary)) (return 'true))
                                    (if (equal (second currentloc) (fourth outer-boundary)) (return 'true))
                                    (return nil)
                                   )
)

;Function to check whether contouring completed or not
;INPUTS - First boundary hit location, Current location
;OUTPUTS - True / False
(defun checkcontour (firstboundaryhitloc currentloc) (prog ()
                                                       (if (null firstboundaryhitloc) (return 'false))
                                                       (if (equal firstboundaryhitloc currentloc) (return 'true))
                                                       (return 'false)
                                                      )
)

;Function to wall follow
;INPUTS - Start location
;OUTPUTS - Robot Motion
(defun wall-follow-2 (start-location) (prog (contourcompleted firstboundaryhit firstboundaryhitloc currentloc maxiterations i sensors featurevector navigatecommand)
                                        (setq firstboundaryhit nil)
                                        (setq firstboundaryhitloc nil)
                                        (setq currentloc start-location)
                                        (setq contourcompleted 'false)
                                        (setq maxiterations (+ (* (first gridsize) (second gridsize)) (first gridsize)))
                                        (setq i 1)
                                        ;(print maxiterations)
                                   loop
                                        (if (equal firstboundaryhit 'nil) (go cboundary))
                                  loop1
                                        (if (equal contourcompleted 'true) (return 'COMPLETED))
                                        (if (>= i maxiterations) (return 'MAXITERATIONS))
                                        (setq i (+ i 1))
                                        (setq sensors (calc-sensors-2 currentloc))
                                        (setq featurevector (calc-feature-vector-2 sensors))
                                        (if (null featurevector) (return 'ERRORFV))
                                        (setq navigatecommand (calc-navigation-command-2 featurevector))
                                        (setq currentloc (update-current-location currentloc navigatecommand))
                                        (print currentloc)
                                        (setq contourcompleted (checkcontour firstboundaryhitloc currentloc))
                                        (go loop)
                              cboundary
                                        (setq firstboundaryhit (checkboundary currentloc))
                                        (if (equal firstboundaryhit 'true) (setq firstboundaryhitloc currentloc))
                                        (go loop1)
                                      )
)
