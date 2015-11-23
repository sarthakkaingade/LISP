(setf gridsize '(8 8))
(setf SI '(s1 s2 s3 s4 s5 s6 s7 s8))
(setf FV '(x1 x2 x3 x4))
(setf outer-boundary '(1 8 1 8))
(setf obstacles '((3 4) (3 5) (3 6)))

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

(defun calc-sensors (currentloc) (prog (s1 s2 s3 s4 s5 s6 s7 s8 i obstacle)
                                  (setq s1 0 s2 0 s3 0 s4 0 s5 0 s6 0 s7 0 s8 0)
                                  (setq i 0)
                                  (if (equal (first currentloc) (first outer-boundary)) (setq s1 1 s2 1 s3 1))
                                  (if (equal (first currentloc) (second outer-boundary)) (setq s5 1 s6 1 s7 1))
                                  (if (equal (second currentloc) (third outer-boundary)) (setq s1 1 s8 1 s7 1))
                                  (if (equal (second currentloc) (fourth outer-boundary)) (setq s3 1 s4 1 s5 1))
                             loop
                                  (if (= i (length obstacles)) (go out))
                                  (setq obstacle (nth i obstacles))
                                  (if (= (first currentloc) (- (first obstacle) 1)) (go up))
                            loop1
                                  (if (= (first currentloc) (+ (first obstacle) 1)) (go down))
                            loop2
                                  (if (= (second currentloc) (- (second obstacle) 1)) (go left))
                            loop3
                                  (if (= (second currentloc) (+ (second obstacle) 1)) (go right))
                            loop4
                                  (setq i (+ i 1))
                                  (go loop)
                               up
                                  (if (= (second currentloc) (- (second obstacle) 1)) (setq s5 1))
                                  (if (= (second currentloc) (second obstacle)) (setq s6 1))
                                  (if (= (second currentloc) (+ (second obstacle) 1)) (setq s7 1))
                                  (go loop1)
                             down
                                  (if (= (second currentloc) (- (second obstacle) 1)) (setq s3 1))
                                  (if (= (second currentloc) (second obstacle)) (setq s2 1))
                                  (if (= (second currentloc) (+ (second obstacle) 1)) (setq s1 1))
                                  (go loop2)
                             left
                                  (if (= (first currentloc) (first obstacle)) (setq s4 1))
                                  (go loop3)
                            right
                                  (if (= (first currentloc) (first obstacle)) (setq s8 1))
                                  (go loop4)
                              out
                                  (return (list s1 s2 s3 s4 s5 s6 s7 s8))
                                 )
)

(defun calc-navigation-command-obstacle (featurevector) (prog ()
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

(defun checkobstacleboundary (currentloc) (prog (i obstacle)
                                            (setq i 0)
                                       loop
                                            (if (= i (length obstacles)) (go out))
                                            (setq obstacle (nth i obstacles))
                                            (if (= (first currentloc) (- (first obstacle) 1)) (go up))
                                      loop1
                                            (if (= (first currentloc) (+ (first obstacle) 1)) (go down))
                                      loop2
                                            (if (= (second currentloc) (- (second obstacle) 1)) (go left))
                                      loop3
                                            (if (= (second currentloc) (+ (second obstacle) 1)) (go right))
                                      loop4
                                            (setq i (+ i 1))
                                            (go loop)
                                         up
                                            (if (= (second currentloc) (- (second obstacle) 1)) (return 'true))
                                            (if (= (second currentloc) (second obstacle)) (return 'true))
                                            (if (= (second currentloc) (+ (second obstacle) 1)) (return 'true))
                                            (go loop1)
                                       down
                                            (if (= (second currentloc) (- (second obstacle) 1)) (return 'true))
                                            (if (= (second currentloc) (second obstacle)) (return 'true))
                                            (if (= (second currentloc) (+ (second obstacle) 1)) (return 'true))
                                            (go loop2)
                                       left
                                            (if (= (first currentloc) (first obstacle)) (return 'true))
                                            (go loop3)
                                      right
                                            (if (= (first currentloc) (first obstacle)) (return 'true))
                                            (go loop4)
                                        out
                                            (return nil)
                                          )
)

(defun checkcontour (firstboundaryhitloc currentloc) (prog ()
                                                       (if (null firstboundaryhitloc) (return 'false))
                                                       (if (equal firstboundaryhitloc currentloc) (return 'true))
                                                       (return 'false)
                                                      )
)

(defun checkobstaclecontour (firstobstaclehitloc currentloc) (prog ()
                                                               (if (null firstobstaclehitloc) (return 'false))
                                                               (if (equal firstobstaclehitloc currentloc) (return 'true))
                                                               (return 'false)
                                                             )
)

(defun wall-follow (start-location) (prog (contourcompleted firstboundaryhit firstboundaryhitloc currentloc maxiterations i sensors featurevector navigatecommand firstobstaclehit firstobstaclehitloc obstaclecompleted)
                                      (setq firstboundaryhit nil)
                                      (setq firstboundaryhitloc nil)
                                      (setq firstobstaclehit nil)
                                      (setq firstobstaclehitloc nil)
                                      (setq currentloc start-location)
                                      (setq contourcompleted 'false)
                                      (setq obstaclecompleted 'false)
                                      (setq maxiterations (* (* (first gridsize) (second gridsize)) 2))
                                      (setq i 1)
                                      ;(print maxiterations)
                                 loop
                                      (if (equal firstboundaryhit 'nil) (go cboundary))
                                loop1
                                      (if (equal firstobstaclehit 'nil) (go cobstacle))
                                loop2
                                      (if (equal contourcompleted 'true) (return 'WALL-COMPLETED))
                                      (if (equal obstaclecompleted 'true) (return 'OBSTACLE-COMPLETED))
                                      (if (>= i maxiterations) (return 'MAXITERATIONS))
                                      (setq i (+ i 1))
                                      (setq sensors (calc-sensors currentloc))
                                      (setq featurevector (calc-feature-vector sensors))
                                      (if (null featurevector) (return 'ERRORFV))
                                      (setq navigatecommand (calc-navigation-command-obstacle sensors))
                                      (setq currentloc (update-current-location currentloc navigatecommand))
                                      (print currentloc)
                                      (setq contourcompleted (checkcontour firstboundaryhitloc currentloc))
                                      (setq obstaclecompleted (checkobstaclecontour firstobstaclehitloc currentloc))
                                      (go loop)
                            cboundary
                                      (setq firstboundaryhit (checkboundary currentloc))
                                      (if (equal firstboundaryhit 'true) (setq firstboundaryhitloc currentloc))
                                      (go loop1)
                            cobstacle
                                      (setq firstobstaclehit (checkobstacleboundary currentloc))
                                      (if (equal firstobstaclehit 'true) (setq firstobstaclehitloc currentloc))
                                      (go loop2)
                                    )
)