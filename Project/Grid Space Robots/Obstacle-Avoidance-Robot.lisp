(setf gridsize '(8 8))
(setf SI '(s1 s2 s3 s4 s5 s6 s7 s8))
(setf outer-boundary '(1 8 1 8))
(setf obstacles '((3 4) (3 5) (3 6) (5 3) (5 4)))

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

;Function to simulate robot sensors
;INPUTS - Current location
;OUTPUTS - Sensor values list (s1 s2 s3 s4 s5 s6 s7 s8)
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

;Function to simulate robot sensors with (s2 s4 s6 s8) working properly
;INPUTS - Current location
;OUTPUTS - Sensor values list (s1 s2 s3 s4 s5 s6 s7 s8)
(defun calc-sensors-impaired-odd  (currentloc prev-command prev-sensors) (prog (s1 s2 s3 s4 s5 s6 s7 s8 i obstacle)
                                                                           (setq s1 0 s2 0 s3 0 s4 0 s5 0 s6 0 s7 0 s8 0)
                                                                           (setq i 0)
                                                                           (if (equal (first currentloc) (first outer-boundary)) (setq s2 1))
                                                                           (if (equal (first currentloc) (second outer-boundary)) (setq s6 1))
                                                                           (if (equal (second currentloc) (third outer-boundary)) (setq s8 1))
                                                                           (if (equal (second currentloc) (fourth outer-boundary)) (setq s4 1))
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
                                                                           (if (= (second currentloc) (second obstacle)) (setq s6 1))
                                                                           (go loop1)
                                                                      down
                                                                           (if (= (second currentloc) (second obstacle)) (setq s2 1))
                                                                           (go loop2)
                                                                      left
                                                                           (if (= (first currentloc) (first obstacle)) (setq s4 1))
                                                                           (go loop3)
                                                                     right
                                                                           (if (= (first currentloc) (first obstacle)) (setq s8 1))
                                                                           (go loop4)
                                                                       out
                                                                           (if (equal prev-command 'east) (setq s1 (second prev-sensors)))
                                                                           (if (equal prev-command 'south) (setq s3 (fourth prev-sensors)))
                                                                           (if (equal prev-command 'west) (setq s5 (sixth prev-sensors)))
                                                                           (if (equal prev-command 'north) (setq s7 (eighth prev-sensors)))
                                                                           (return (list s1 s2 s3 s4 s5 s6 s7 s8))
                                                                         )
)

;Function to simulate robot sensors with (s1 s3 s5 s7) working properly
;INPUTS - Current location
;OUTPUTS - Sensor values list (s1 s2 s3 s4 s5 s6 s7 s8)
(defun calc-sensors-impaired-even (currentloc prev-command prev-sensors boundaryhit) (prog (s1 s2 s3 s4 s5 s6 s7 s8 i obstacle)
                                                                                       (setq s1 0 s2 0 s3 0 s4 0 s5 0 s6 0 s7 0 s8 0)
                                                                                       (setq i 0)
                                                                                       (if (equal (first currentloc) (first outer-boundary)) (setq s1 1 s3 1))
                                                                                       (if (equal (first currentloc) (second outer-boundary)) (setq s5 1 s7 1))
                                                                                       (if (equal (second currentloc) (third outer-boundary)) (setq s1 1 s7 1))
                                                                                       (if (equal (second currentloc) (fourth outer-boundary)) (setq s3 1 s5 1))
                                                                                  loop
                                                                                       (if (= i (length obstacles)) (go out))
                                                                                       (setq obstacle (nth i obstacles))
                                                                                       (if (= (first currentloc) (- (first obstacle) 1)) (go up))
                                                                                 loop1
                                                                                       (if (= (first currentloc) (+ (first obstacle) 1)) (go down))
                                                                                 loop2
                                                                                       (setq i (+ i 1))
                                                                                       (go loop)
                                                                                    up
                                                                                       (if (= (second currentloc) (- (second obstacle) 1)) (setq s5 1))
                                                                                       (if (= (second currentloc) (+ (second obstacle) 1)) (setq s7 1))
                                                                                       (go loop1)
                                                                                  down
                                                                                       (if (= (second currentloc) (- (second obstacle) 1)) (setq s3 1))
                                                                                       (if (= (second currentloc) (+ (second obstacle) 1)) (setq s1 1))
                                                                                       (go loop2)
                                                                                   out
                                                                                       (if (equal boundaryhit 'true) (setq s2 (if (my-and s1 s3) 1 0)) (setq s2 (my-or s1 s3)))
                                                                                       (setq s4 (if (my-and s3 s5) 1 0))
                                                                                       (setq s6 (if (my-and s5 s7) 1 0))
                                                                                       (setq s8 (if (my-and s7 s1) 1 0))
                                                                                       (if (equal prev-command 'east) (setq s2 (third prev-sensors)))
                                                                                       (if (equal prev-command 'south) (setq s4 (fifth prev-sensors)))
                                                                                       (if (equal prev-command 'west) (setq s6 (seventh prev-sensors)))
                                                                                       (if (equal prev-command 'north) (setq s8 (first prev-sensors)))
                                                                                       (return (list s1 s2 s3 s4 s5 s6 s7 s8))
                                                                                     )
)

;Function to calculate actions based on production rules
;INPUTS - Sensor values list (s1 s2 s3 s4 s5 s6 s7 s8)
;OUTPUTS - Action North / South / East / West
(defun calc-navigation-command-obstacle (featurevector boundaryhit) (prog ()
                                                                      (if (equal boundaryhit 'nil) (go OA))
                                                                      (if (my-and (second featurevector) (my-not (fourth featurevector))) (return 'east))
                                                                      (if (my-and (fourth featurevector) (my-not (sixth featurevector))) (return 'south))
                                                                      (if (my-and (sixth featurevector) (my-not (eighth featurevector))) (return 'west))
                                                                      (if (my-and (eighth featurevector) (my-not (second featurevector))) (return 'north))
                                                                   OA
                                                                      (if (equal (second featurevector) '0) (return 'north))
                                                                      (if (equal (fourth featurevector) '0) (return 'east))
                                                                      (if (equal (sixth featurevector) '0) (return 'south))
                                                                      (if (equal (eight featurevector) '0) (return 'west))
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

;Function to wall follow with obstacle avoidance
;INPUTS - Start location
;OUTPUTS - Robot Motion
(defun wall-follow-obstacle (start-location) (prog (contourcompleted firstboundaryhit firstboundaryhitloc currentloc maxiterations i sensors navigatecommand)
                                               (setq firstboundaryhit nil)
                                               (setq firstboundaryhitloc nil)
                                               (setq currentloc start-location)
                                               (setq contourcompleted 'false)
                                               (setq maxiterations (* (* (first gridsize) (second gridsize)) 2))
                                               (setq i 1)
                                               ;(print maxiterations)
                                          loop
                                               (if (equal firstboundaryhit 'nil) (go cboundary))
                                         loop1
                                               (if (equal contourcompleted 'true) (return 'WALL-FOLLOWED-WITH-OBSTACLE-AVOIDANCE))
                                               (if (>= i maxiterations) (return 'MAXITERATIONS))
                                               (setq i (+ i 1))
                                               (setq sensors (calc-sensors currentloc))
                                               (setq navigatecommand (calc-navigation-command-obstacle sensors firstboundaryhit))
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

;Function to wall follow with obstacle avoidance with (s2 s4 s6 s8) working properly
;INPUTS - Start location
;OUTPUTS - Robot Motion
(defun wall-follow-obstacle-impaired-odd  (start-location) (prog (contourcompleted firstboundaryhit firstboundaryhitloc currentloc maxiterations i sensors navigatecommand prev-command prev-sensors)
                                                             (setq firstboundaryhit nil)
                                                             (setq firstboundaryhitloc nil)
                                                             (setq currentloc start-location)
                                                             (setq contourcompleted 'false)
                                                             (setq maxiterations (* (* (first gridsize) (second gridsize)) 2))
                                                             (setq i 1)
                                                             ;(print maxiterations)
                                                        loop
                                                             (if (equal firstboundaryhit 'nil) (go cboundary))
                                                       loop1
                                                             (if (equal contourcompleted 'true) (return 'WALL-FOLLOWED-WITH-OBSTACLE-AVOIDANCE))
                                                             (if (>= i maxiterations) (return 'MAXITERATIONS))
                                                             (setq i (+ i 1))
                                                             (setq sensors (calc-sensors-impaired-odd currentloc prev-command prev-sensors))
                                                             (setq navigatecommand (calc-navigation-command-obstacle sensors firstboundaryhit))
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


;Function to wall follow with obstacle avoidance with (s1 s3 s5 s7) working properly
;INPUTS - Start location
;OUTPUTS - Robot Motion
(defun wall-follow-obstacle-impaired-even  (start-location) (prog (contourcompleted firstboundaryhit firstboundaryhitloc currentloc maxiterations i sensors navigatecommand prev-command prev-sensors)
                                                             (setq firstboundaryhit nil)
                                                             (setq firstboundaryhitloc nil)
                                                             (setq currentloc start-location)
                                                             (setq contourcompleted 'false)
                                                             (setq maxiterations (* (* (first gridsize) (second gridsize)) 2))
                                                             (setq i 1)
                                                             ;(print maxiterations)
                                                        loop
                                                             (if (equal firstboundaryhit 'nil) (go cboundary))
                                                       loop1
                                                             (if (equal contourcompleted 'true) (return 'WALL-FOLLOWED-WITH-OBSTACLE-AVOIDANCE))
                                                             (if (>= i maxiterations) (return 'MAXITERATIONS))
                                                             (setq i (+ i 1))
                                                             (setq sensors (calc-sensors-impaired-even currentloc prev-command prev-sensors firstboundaryhit))
                                                             (setq navigatecommand (calc-navigation-command-obstacle sensors firstboundaryhit))
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
