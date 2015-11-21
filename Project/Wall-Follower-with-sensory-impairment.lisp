(setf gridsize '(6 6))
(setf SI '(s1 s2 s3 s4 s5 s6 s7 s8))
(setf FV '(w1 w2 w3 w4 w5 w6 w7 w8))
(setf outer-boundary '(1 6 1 6))

(defun my-or (x y) (prog ()
                     (if (= x 1) (return 1))
                     (if (= y 1) (return 1))
                     (return 0)
                    )
)

(defun my-not (l) (if (= l 0) 1 0))

(defun my-and (x y) (prog ()
                      (if (= x 0) (return nil))
                      (if (= y 0) (return nil))
                      (return 1)
                    )
)

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

(defun calc-sensors-impaired (currentloc) (prog (sensors s1 s2 s3 s4 s5 s6 s7 s8)
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
                                            (setq s2 1)
                                            (go loop1)
                                   last-row
                                            (setq s6 1)
                                            (go loop2)
                               first-column
                                            (setq s8 1)
                                            (go loop3)
                                last-column
                                            (setq s4 1)
                                            (go out)
                                           )
)

(defun calc-navigation-command (featurevector) (prog ()
                                                 (if (my-and (fourth featurevector) (my-not (first featurevector))) (return 'north))
                                                 (if (my-and (third featurevector) (my-not (fourth featurevector))) (return 'west))
                                                 (if (my-and (second featurevector) (my-not (third featurevector))) (return 'south))
                                                 (if (my-and (first featurevector) (my-not (second featurevector))) (return 'east))
                                                 (return 'north)
                                                )
)

(defun update-current-location (currentloc command) (prog ()
                                                      (if (equal command 'north) (return (list (- (first currentloc) 1) (second currentloc))))
                                                      (if (equal command 'south) (return (list (+ (first currentloc) 1) (second currentloc))))
                                                      (if (equal command 'east) (return (list (first currentloc) (+ (second currentloc) 1))))
                                                      (if (equal command 'west) (return (list (first currentloc) (- (second currentloc) 1))))
                                                     )
)

(defun checkboundary (currentloc) (prog ()
                                    (if (equal (first currentloc) (first outer-boundary)) (return 'true))
                                    (if (equal (first currentloc) (second outer-boundary)) (return 'true))
                                    (if (equal (second currentloc) (third outer-boundary)) (return 'true))
                                    (if (equal (second currentloc) (fourth outer-boundary)) (return 'true))
                                    (return nil)
                                   )
)

(defun checkcontour (firstboundaryhitloc currentloc) (prog ()
                                                       (if (null firstboundaryhitloc) (return 'false))
                                                       (if (equal firstboundaryhitloc currentloc) (return 'true))
                                                       (return 'false)
                                                      )
)

(defun wall-follow (start-location) (prog (contourcompleted firstboundaryhit firstboundaryhitloc currentloc maxiterations i sensors featurevector navigatecommand)
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
                                      (setq sensors (calc-sensors currentloc))
                                      (setq featurevector (calc-feature-vector sensors))
                                      (if (null featurevector) (return 'ERRORFV))
                                      (setq navigatecommand (calc-navigation-command featurevector))
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