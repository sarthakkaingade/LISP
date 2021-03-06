;Function to generate Jumping Peg Triangle
;INPUTS - Depth, Space-position, Element-to-represent (E.g - "P"), Element-to-denote-space (E.g - "_")
;OUTPUTS - Single list containing jumping peg triangle
(defun generate-jumping-peg (n pos elem1 elem2) (prog (i lis)
                                                  (setq i 1)
                                                  (setq size (- (* 2 n) 1))
                                             loop
                                                  (if (= i (+ n 1)) (go insert-elem2))
                                                  (setq list (generate-row i size elem1))
                                                  (setq lis (append lis list))
                                                  ;(print list)
                                                  (setq i (+ i 1))
                                                  (go loop)
                                     insert-elem2
                                                  (setq pos (calc-pos lis pos))
                                                  (setq lis (myreplace lis pos elem2))
                                                  (format t "~%Generated Peg Triangle")
                                                  (print-peg lis n)
                                                  (return lis)
                                                )
)

;Function to calculate position of space in list based on user input
;INPUTS - Single list containing jumping peg triangle, space position
;OUTPUTS - Position Number
(defun calc-pos (lis pos) (prog (p local_pos)
                            (setq p 0)
                            (setq i 0)
                       loop
                            (setq i (+ i 1))
                            (setq local_pos (+ (position 'p lis :start (+ p 1) :test #'equal) 1))
                            (setq p local_pos)
                            (if (= i pos) (return local_pos) (go loop))
                          )
)

;Function to generate each row of jumping peg triangle
;INPUTS - Row number, size of row, Element-to-represent (E.g - "P")
;OUTPUTS - Row list
(defun generate-row (n size element) (prog (i list middle)
                                       (setq i (- n 1))
                                       (setq middle (/ (+ size 1) 2))
                                       (loop for i from 1 to size do (setq list (append list (cons '\. nil))))
                                       (if (= (mod n 2) 1) (setq list (myreplace list middle element)))
                                       (if (> n 1) (go loop) (return list)) 
                                  loop
                                       (setq list (myreplace list (+ middle i) element))
                                       (setq list (myreplace list (- middle i) element))
                                       (setq i (- i 2))
                                       (if (< i 0) (return list) (go loop))
                                     )
)

;Function to replace element in peg triangle
;INPUTS - Single list containing jumping peg triangle, position, Element-to-represent (E.g - "P")
;OUTPUTS - Single list containing replaced element in jumping peg triangle
(defun myreplace (list pos element) (prog (l)
                                      (setq i 1)
                                 loop
                                      (if (= i (+ (length list) 1)) (return l))
                                      (if (= i pos) (setq l (append l (cons element nil))) (setq l (append l (cons (nth (- i 1) list) nil))))
                                      (setq i (+ i 1))
                                      (go loop)
                                    )
)

;Function to print peg triangle
;INPUTS - Single list containing jumping peg triangle, depth
;OUTPUTS - Jumping peg triangle
(defun print-peg (list depth) (prog (size l1 i start end)
                          (setq size (- (* 2 depth) 1))
                          (setq start 0)
                          (setq end size)
                     loop
                          (loop for i from start to (- end 1) do (setq l1 (append l1 (cons (nth i list) nil))))
                          ;(print l1)
                          (format t "~%~a" l1)
                          (setq l1 '())
                          (if (= end (length list)) (return nil))
                          (setq start end)
                          (setq end (+ end size))
                          (go loop)
                        )
)

;Main Function to solve jumping peg problem
;INPUTS - Depth, Space Position
;OUTPUTS - Result
(defun jumping-pegs (depth space-pos) (prog ()
                                        (return (graph-search-astar (generate-jumping-peg depth space-pos 'P '_) depth))
                                      )
)

;Function for graph searching using astar algorithm
;INPUTS -  Single list containing jumping peg triangle, Depth
;OUTPUTS - Result
(defun graph-search-astar (init size) (prog (graph open closed start-node temp-graph j node-count N P D F abc)
                                        (setq node-count 2 start-node (append (list '1 '-1 '0 (- (count 'P init :test #'equal) 1)) init))
                                        (setq graph (append graph (cons start-node nil)) open (append open (cons start-node nil)))
                                        (setq abc 0)
                                   loop
                                        (if (null open) (go unsuccessful-exit))
                                        (setq closed (append closed (cons (car open) nil)) open (cdr open))
                                        (if (= (count 'P (cddddr (car (last closed))) :test #'equal) 1) (go out))
                                        (setq temp-graph (expand (cddddr (car (last closed))) size))
                                        (if (null temp-graph) (go loop))
                                        (setq j 0)
                                  loop1
                                        (setq N node-count P (nth 0 (car (last closed))) D (+ (nth 2 (car (last closed))) 1) )
                                        (setq F (+ (- (count 'P (nth j temp-graph) :test #'equal) 1) D))
                                        (if (not (find (nth j temp-graph) (mapcar #'cddddr graph) :test #'equal)) (setq graph ( append graph (cons (append (list N P D F) (nth j temp-graph)) nil)) open ( append open (cons (append (list N P D F) (nth j temp-graph)) nil))))
                                        (if (find (nth j temp-graph) (mapcar #'cddddr graph) :test #'equal)
							(if (< D (nth 2 (nth (position (nth j temp-graph) (mapcar #'cddddr graph) :test #'equal) graph)) ) (replace-node graph (append (list N P D F) (nth j temp-graph)) (position (nth j temp-graph) (mapcar #'cddddr graph) :test #'equal)))
					)
                                        (if (find (nth j temp-graph) (mapcar #'cddddr open) :test #'equal) 
							(if (< D (nth 2 (nth (position (nth j temp-graph) (mapcar #'cddddr open) :test #'equal) open)) ) (replace-node open (append (list N P D F) (nth j temp-graph)) (position (nth j temp-graph) (mapcar #'cddddr open) :test #'equal)))
                                        )
                                        (if (find (nth j temp-graph) (mapcar #'cddddr closed) :test #'equal) 
							(if (< D (nth 2 (nth (position (nth j temp-graph) (mapcar #'cddddr closed) :test #'equal) closed)) ) (replace-node closed (append (list N P D F) (nth j temp-graph)) (position (nth j temp-graph) (mapcar #'cddddr closed) :test #'equal)))
                                        )
                                        (setq j (+ j 1) node-count (+ node-count 1))
                                        (if (not (= j (length temp-graph))) (go loop1))
                                        (sort open #'< :key #'fourth)
                                        (setq open (secondary-sort open))
                                        (setq abc (+ abc 1))
                                        (if (not (>= abc 1999)) (go loop) (return 'MAX-ITERATIONS))
                      unsuccessful-exit
                                        (print 'Goal-Cannot-be-Achieved)
                                        (return nil)
                                    out
                                        (multi-print (trace-path graph (car (last closed))) size)
                                        (print 'Goal-Achieved)
                                    )
)

;Function to sort OPEN list based on depth of node
;INPUTS - OPEN list
;OUTPUTS - Sorted OPEN list
(defun secondary-sort (list1) (prog (temp-list result i j)
                                (if (or (null list1) (= (length list1) 1)) (return list1))
                                (setq i 0)
                           loop
                                (if (= (nth 3 (nth (+ i 1) list1)) (nth 3 (nth i list1))) (go sort-temp-list) (setq result (append result (cons (nth i list1) nil))) )
                                (go continue)
                      main-loop
                                (setq result (append result temp-list))
                                (setq temp-list '())
                       continue
                                (setq i (+ i 1))
                                (if (not (= i (- (length list1) 1))) (go loop))
                                (setq result (append result (list (nth i list1))))
                                (return result)
                 sort-temp-list
                                (setq j i)
                          loop1
                                (if (= (nth 3 (nth (+ j 1) list1)) (nth 3 (nth j list1))) (setq temp-list (append temp-list (cons (nth j list1) nil))) (go process))
                                (setq j (+ j 1))
                                (if (not (= j (- (length list1) 1))) (go loop1))
                                (setq temp-list (append temp-list (cons (nth j list1) nil)))
                                (sort temp-list #'< :key #'third)
                                (setq result (append result temp-list))
                                (return result)
                        process
                                (setq temp-list (append temp-list (cons (nth j list1) nil)))
                                (sort temp-list #'< :key #'third)
                                (setq i j)
                                (go main-loop)
                              )
)

;Function to trace path from goal node to start node
;INPUTS - Graph, Goal node
;OUTPUTS - List containing traced path
(defun trace-path (graph goal) (prog (path i count)
                                 (setq path (append path (cons goal nil)))
                                 (setq parent (cadr (car path)))
                                 (setq count 1)
                            loop
                                 (setq i 0)
                           loop1
                                 (if (= (car (nth i (reverse graph))) parent) (setq path (append (cons (nth i (reverse graph)) nil) path)) (go continue))
                                 (go next)
                        continue
                                 (setq i (+ i 1))
                                 (if (not (= i (length graph))) (go loop1))
                            next
                                 (setq count (+ count 1))
                                 (setq parent (cadr (car path)))
                                 (if ( not (= (cadr (car path)) -1)) (go loop))
                                 (return (mapcar #'cddddr path))
                               )
)

;Function to replace node
;INPUTS - Graph, Node, position
;OUTPUTS - Updated Graph
(defun replace-node (graph node pos) (prog (i new-graph temp-node)
                                       (setq i 0)
                                       (setq temp-node (append (cons (car (nth pos graph)) 'nil) (cons (cadr node) 'nil) (cddr (nth pos graph))))
                                  loop
                                       (if (= i pos) (setq new-graph (append new-graph (cons temp-node nil))) (setq new-graph (append new-graph (cons (nth i graph) nil))))
                                       (setq i (+ i 1))
                                       (if (= i (length graph)) (return new-graph) (go loop))
                                     )
)

;Function to multiprint moves required to achieve goal
;INPUTS - list containing traced path to start node from goal node, Size of each row
;OUTPUTS - Print
(defun multi-print (list1 size) (prog (i temp)
                                  (setq temp list1)
                                  (setq i 0)
                             loop
                                  (format t "~%~%MOVE ~a" (+ i 1))
                                  ;(print (+ i 1))
                                  (print-peg (car temp) size)
                                  (setq temp (cdr temp))
                                  (setq i (+ i 1))
                                  (if (not (= i (length list1))) (go loop))
                                )
)

;Function to expand nodes
;INPUTS - Peg Triangle, Depth
;OUTPUTS - list of expanded nodes
(defun expand (node size) (prog (successors pos dlu dru dld drd l r)
                            (setq pos (position '_ node :start 0  :test #'equal))
                            (if (not pos) (return successors))
                       loop
                            (setq dlu (diag-left-up node pos size) dru (diag-right-up node pos size) dld (diag-left-down node pos size) drd (diag-right-down node pos size) l (left node pos size) r (right node pos size))
                            (if dlu (setq successors (append successors (cons dlu nil))))
                            (if dru (setq successors (append successors (cons dru nil))))
                            (if dld (setq successors (append successors (cons dld nil))))
                            (if drd (setq successors (append successors (cons drd nil))))
                            (if l (setq successors (append successors (cons l nil))))
                            (if r (setq successors (append successors (cons r nil))))
                            (setq pos (position '_ node :start (+ pos 1)  :test #'equal))
                            (if pos (go loop) (return successors))
                          )
)

;Function to check and return, if "P" is located at upper left diagonal position of "_"
;INPUTS - Peg Triangle, position of "_", Depth
;OUTPUTS - Successor node
(defun diag-left-up (node pos size) (prog (temp)
                                      (setq i 0)
                                      (if (and (> (- pos (* 4 size)) 0) (not (equal '* (nth (- pos (* 4 size)) node))) (not (equal '_ (nth (- pos (* 4 size)) node))) (not (equal '* (nth (- pos (* 2 size)) node))) (not (equal '_ (nth (- pos (* 2 size)) node))) ) (go loop) (return nil))
                                 loop
                                      (cond
                                       ((equal i (- pos (* 4 size))) (setq temp (append temp (cons '_ 'nil))))
                                       ((equal i (- pos (* 2 size))) (setq temp (append temp (cons '_ 'nil))))
                                       ((equal i pos) (setq temp (append temp (cons 'P 'nil))))
                                       (t (setq temp (append temp (cons (nth i node) 'nil))))
                                      )						
                                      (setq i (+ i 1))
                                      (if (not (equal i (length node))) (go loop))
                                      (return temp)
                                    )
)

;Function to check and return, if "P" is located at lower right diagonal position of "_"
;INPUTS - Peg Triangle, position of "_", Depth
;OUTPUTS - Successor node
(defun diag-right-down (node pos size) (prog (temp)
                                         (setq i 0)
                                         (if (and (< (+ pos (* 4 size)) (length node)) (not (equal '* (nth (+ pos (* 4 size)) node))) (not (equal '_ (nth (+ pos (* 4 size)) node))) (not (equal '* (nth (+ pos (* 2 size)) node))) (not (equal '_ (nth (+ pos (* 2 size)) node))) ) (go loop) (return nil))
                                    loop
                                         (cond
                                          ((equal i (+ pos (* 4 size))) (setq temp (append temp (cons '_ 'nil))))
                                          ((equal i (+ pos (* 2 size))) (setq temp (append temp (cons '_ 'nil))))
                                          ((equal i pos) (setq temp (append temp (cons 'P 'nil))))
                                          (t (setq temp (append temp (cons (nth i node) 'nil))))
                                         )						
                                         (setq i (+ i 1))
                                         (if (not (equal i (length node))) (go loop))
                                         (return temp)
                                       )
)

;Function to check and return, if "P" is located at upper right diagonal position of "_"
;INPUTS - Peg Triangle, position of "_", Depth
;OUTPUTS - Successor node
(defun diag-right-up (node pos size) (prog (temp)
                                       (if (= pos (- (length node) 1)) (return nil))
                                       (setq i 0)
                                       (if (and (> (- pos (- (* 4 size) 4)) 0) (not (equal '* (nth (- pos (- (* 4 size) 4)) node))) (not (equal '_ (nth (- pos (- (* 4 size) 4)) node))) (not (equal '* (nth (- pos (- (* 2 size) 2)) node))) (not (equal '_ (nth (- pos (- (* 2 size) 2)) node))) ) (go loop) (return nil))
                                  loop
                                       (cond
                                        ((equal i (- pos (- (* 4 size) 4))) (setq temp (append temp (cons '_ 'nil))))
                                        ((equal i (- pos (- (* 2 size) 2))) (setq temp (append temp (cons '_ 'nil))))
                                        ((equal i pos) (setq temp (append temp (cons 'P 'nil))))
                                        (t (setq temp (append temp (cons (nth i node) 'nil))))
                                       )						
                                       (setq i (+ i 1))
                                       (if (not (equal i (length node))) (go loop))
                                       (return temp)
                                     )
)

;Function to check and return, if "P" is located at lower left diagonal position of "_"
;INPUTS - Peg Triangle, position of "_", Depth
;OUTPUTS - Successor node
(defun diag-left-down (node pos size) (prog (temp)
                                        (setq i 0)
                                        (if (and (< (+ pos (- (* 4 size) 4)) (- (length node) 1)) (not (equal '* (nth (+ pos (- (* 4 size) 4)) node))) (not (equal '_ (nth (+ pos (- (* 4 size) 4)) node))) (not (equal '* (nth (+ pos (- (* 2 size) 2)) node))) (not (equal '_ (nth (+ pos (- (* 2 size) 2)) node))) ) (go loop) (return nil))
				   loop
                                        (cond
                                         ((equal i (+ pos (- (* 4 size) 4))) (setq temp (append temp (cons '_ 'nil))))
                                         ((equal i (+ pos (- (* 2 size) 2))) (setq temp (append temp (cons '_ 'nil))))
                                         ((equal i pos) (setq temp (append temp (cons 'P 'nil))))
                                         (t (setq temp (append temp (cons (nth i node) 'nil))))
                                        )						
                                        (setq i (+ i 1))
                                        (if (not (equal i (length node))) (go loop))
                                        (return temp)
                                      )
)

;Function to check and return, if "P" is located at left position of "_"
;INPUTS - Peg Triangle, position of "_", Depth
;OUTPUTS - Successor node
(defun left (node pos size) (prog (temp)
                              (setq i 0)
                              (if (and (> (- pos 4) (- (- pos (mod pos (- (* 2 size) 1))) 1)) (not (equal '* (nth (- pos 4) node))) (not (equal '_ (nth (- pos 4) node))) (not (equal '* (nth (- pos 2) node))) (not (equal '_ (nth (- pos 2) node))) ) (go loop) (return nil))
		         loop
                              (cond
                               ((equal i (- pos 4)) (setq temp (append temp (cons '_ 'nil))))
                               ((equal i (- pos 2)) (setq temp (append temp (cons '_ 'nil))))
                               ((equal i pos) (setq temp (append temp (cons 'P 'nil))))
                               (t (setq temp (append temp (cons (nth i node) 'nil))))
                              )						
                              (setq i (+ i 1))
                              (if (not (equal i (length node))) (go loop))
                              (return temp)
                            )
)

;Function to check and return, if "P" is located at right position of "_"
;INPUTS - Peg Triangle, position of "_", Depth
;OUTPUTS - Successor node
(defun right (node pos size) (prog (temp)
                               (setq i 0)
                               (if (and (< (+ pos 4) (+ pos (- (- (* 2 size) 1) (mod pos (- (* 2 size) 1))))) (not (equal '* (nth (+ pos 4) node))) (not (equal '_ (nth (+ pos 4) node))) (not (equal '* (nth (+ pos 2) node))) (not (equal '_ (nth (+ pos 2) node))) ) (go loop) (return nil))
                          loop
                               (cond
                                ((equal i (+ pos 4)) (setq temp (append temp (cons '_ 'nil))))
                                ((equal i (+ pos 2)) (setq temp (append temp (cons '_ 'nil))))
                                ((equal i pos) (setq temp (append temp (cons 'P 'nil))))
                                (t (setq temp (append temp (cons (nth i node) 'nil))))
                               )						
                               (setq i (+ i 1))
                               (if (not (equal i (length node))) (go loop))			
                               (return temp)
                             )
)

