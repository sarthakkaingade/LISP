(setf gridsize '(6 6))
(setf SI '(s1 s2 s3 s4 s5 s6 s7 s8))
(setf FV '(w1 w2 w3 w4 w5 w6 w7 w8))
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
;OUTPUTS - Feature vector list (w1 w2 w3 w4 w5 w6 w7 w8)
(defun calc-feature-vector-impaired (inputs prev-command prev-inputs) (prog (w1 w2 w3 w4 w5 w6 w7 w8)
                                                                        (if (not (= (length inputs) (length SI))) (return nil))
                                                                        (setq w1 0 w2 (second inputs) w3 0 w4 (fourth inputs) w5 0 w6 (sixth inputs) w7 0 w8 (eighth inputs))
                                                                        (if (equal prev-command 'east) (setq w1 (second prev-inputs)))
                                                                        (if (equal prev-command 'south) (setq w3 (fourth prev-inputs)))
                                                                        (if (equal prev-command 'west) (setq w5 (sixth prev-inputs)))
                                                                        (if (equal prev-command 'north) (setq w7 (eighth prev-inputs)))
                                                                        (return (list w1 w2 w3 w4 w5 w6 w7 w8))
                                                                      )
)

;Function to simulate robot sensors
;INPUTS - Current location
;OUTPUTS - Sensor values lsit (s1 s2 s3 s4 s5 s6 s7 s8)
(defun calc-sensors-impaired (currentloc) (prog (sensors s1 s2 s3 s4 s5 s6 s7 s8)
                                            (setq s1 0 s2 0 s3 0 s4 0 s5 0 s6 0 s7 0 s8 0)
                                            (if (equal (first currentloc) (first outer-boundary)) (setq s2 1))
                                            (if (equal (first currentloc) (second outer-boundary)) (setq s6 1))
                                            (if (equal (second currentloc) (third outer-boundary)) (setq s8 1))
                                            (if (equal (second currentloc) (fourth outer-boundary)) (setq s4 1))
                                            (return (list s1 s2 s3 s4 s5 s6 s7 s8))
                                           )
)

;Function to calculate actions based on production rules
;INPUTS - Feature vector list (w1 w2 w3 w4 w5 w6 w7 w8)
;OUTPUTS - Action North / South / East / West
(defun calc-navigation-command-impaired (featurevector) (prog ()
                                                          (if (my-and (second featurevector) (my-not (fourth featurevector))) (return 'east))
                                                          (if (my-and (fourth featurevector) (my-not (sixth featurevector))) (return 'south))
                                                          (if (my-and (sixth featurevector) (my-not (eighth featurevector))) (return 'west))
                                                          (if (my-and (eighth featurevector) (my-not (second featurevector))) (return 'north))
                                                          (if (equal (first featurevector) '1) (return 'north))
                                                          (if (equal (third featurevector) '1) (return 'east))
                                                          (if (equal (fifth featurevector) '1) (return 'south))
                                                          (if (equal (seventh featurevector) '1) (return 'west))
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
(defun wall-follow-with-sensors-impaired (start-location) (prog (contourcompleted firstboundaryhit firstboundaryhitloc currentloc maxiterations i sensors featurevector navigatecommand prev-command prev-sensors)
                                      (setq firstboundaryhit nil)
                                      (setq firstboundaryhitloc nil)
                                      (setq currentloc start-location)
                                      (setq contourcompleted 'false)
                                      (setq prev-command 'nil)
                                      (setq prev-sensors '(0 0 0 0 0 0 0 0))
                                      (setq maxiterations (+ (* (first gridsize) (second gridsize)) (first gridsize)))
                                      (setq i 1)
                                      (print '(WALL FOLLOWER WITH SENSORY IMPAIREMENT))
                                      ;(print maxiterations)
                                 loop
                                      (if (equal firstboundaryhit 'nil) (go cboundary))
                                loop1
                                      (if (equal contourcompleted 'true) (return 'COMPLETED))
                                      (if (>= i maxiterations) (return 'MAXITERATIONS))
                                      (setq i (+ i 1))
                                      (setq sensors (calc-sensors-impaired currentloc))
                                      (setq featurevector (calc-feature-vector-impaired sensors prev-command prev-sensors))
                                      (if (null featurevector) (return 'ERRORFV))
                                      (setq navigatecommand (calc-navigation-command-impaired featurevector))
                                      (setq currentloc (update-current-location currentloc navigatecommand))
                                      (print currentloc)
                                      (setq contourcompleted (checkcontour firstboundaryhitloc currentloc))
                                      (setq prev-command navigatecommand)
                                      (setq prev-sensors sensors)
                                      (go loop)
                        cboundary
                                      (setq firstboundaryhit (checkboundary currentloc))
                                      (if (equal firstboundaryhit 'true) (setq firstboundaryhitloc currentloc))
                                      (go loop1)
                                    )
)