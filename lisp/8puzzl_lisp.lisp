;;; eightpuzzle.lisp
;;; Enhanced 8-puzzle A* solver (Common Lisp)
;;; Representation: state is a list of 9 integers (0 = blank)
;;; Goal: (1 2 3 4 5 6 7 8 0)

(in-package :cl-user)

;;; ============================================================================
;;; Constants and Configuration
;;; ============================================================================

(defparameter *goal-state* '(1 2 3 4 5 6 7 8 0))
(defparameter *grid-size* 3)

;;; ============================================================================
;;; Core Utility Functions
;;; ============================================================================

(defun index->rowcol (idx)
  "Convert linear index (0..8) to (row col) coordinates."
  (values (truncate idx *grid-size*) (mod idx *grid-size*)))

(defun rowcol->index (row col)
  "Convert (row col) coordinates to linear index."
  (+ (* row *grid-size*) col))

(defun swap-at (state i j)
  "Return new state with elements at indices i and j swapped."
  (let ((v (copy-seq (coerce state 'vector))))
    (rotatef (aref v i) (aref v j))
    (coerce v 'list)))

;;; ============================================================================
;;; Heuristic Functions
;;; ============================================================================

(defun manhattan-distance (state)
  "Compute Manhattan distance heuristic for given state.
   Sums the distance of each tile from its goal position."
  (loop for tile in state
        for idx from 0
        when (/= tile 0)
        sum (multiple-value-bind (curr-row curr-col) (index->rowcol idx)
              (let ((goal-idx (1- tile)))  ; tile 1..8 maps to index 0..7
                (multiple-value-bind (goal-row goal-col) (index->rowcol goal-idx)
                  (+ (abs (- curr-row goal-row))
                     (abs (- curr-col goal-col))))))))

(defun linear-conflict (state)
  "Enhanced heuristic: Manhattan + Linear Conflict.
   Adds 2 for each pair of tiles in their goal row/col but reversed."
  (let ((md (manhattan-distance state))
        (conflicts 0))
    ;; Check rows
    (dotimes (row *grid-size*)
      (let ((tiles-in-row '()))
        (dotimes (col *grid-size*)
          (let* ((idx (rowcol->index row col))
                 (tile (nth idx state)))
            (when (and (/= tile 0)
                       (= row (truncate (1- tile) *grid-size*)))
              (push (cons tile col) tiles-in-row))))
        (loop for (t1 . c1) in tiles-in-row do
          (loop for (t2 . c2) in tiles-in-row do
            (when (and (< t1 t2) (> c1 c2))
              (incf conflicts))))))
    ;; Check columns
    (dotimes (col *grid-size*)
      (let ((tiles-in-col '()))
        (dotimes (row *grid-size*)
          (let* ((idx (rowcol->index row col))
                 (tile (nth idx state)))
            (when (and (/= tile 0)
                       (= col (mod (1- tile) *grid-size*)))
              (push (cons tile row) tiles-in-col))))
        (loop for (t1 . r1) in tiles-in-col do
          (loop for (t2 . r2) in tiles-in-col do
            (when (and (< t1 t2) (> r1 r2))
              (incf conflicts))))))
    (+ md (* 2 conflicts))))

;;; ============================================================================
;;; State Space Navigation
;;; ============================================================================

(defun get-neighbors (state)
  "Generate all valid neighbor states (one move from current state)."
  (let* ((blank-idx (position 0 state))
         (row (truncate blank-idx *grid-size*))
         (col (mod blank-idx *grid-size*))
         (neighbors '()))
    ;; Try all four directions
    (when (> row 0)  ; Move tile down (blank up)
      (push (swap-at state blank-idx (- blank-idx *grid-size*)) neighbors))
    (when (< row (1- *grid-size*))  ; Move tile up (blank down)
      (push (swap-at state blank-idx (+ blank-idx *grid-size*)) neighbors))
    (when (> col 0)  ; Move tile right (blank left)
      (push (swap-at state blank-idx (1- blank-idx)) neighbors))
    (when (< col (1- *grid-size*))  ; Move tile left (blank right)
      (push (swap-at state blank-idx (1+ blank-idx)) neighbors))
    neighbors))

;;; ============================================================================
;;; Solvability Check
;;; ============================================================================

(defun count-inversions (state)
  "Count inversions in the puzzle (ignoring blank tile).
   An inversion is a pair of tiles where the larger appears before smaller."
  (let ((tiles (remove 0 state)))
    (loop for i from 0 below (length tiles)
          sum (loop for j from (1+ i) below (length tiles)
                    count (> (nth i tiles) (nth j tiles))))))

(defun solvable-p (state)
  "Check if puzzle configuration is solvable.
   For 3x3 puzzle with blank at bottom-right in goal, solvable iff inversions are even."
  (evenp (count-inversions state)))

;;; ============================================================================
;;; A* Search Algorithm
;;; ============================================================================

(defun reconstruct-path (came-from final-state)
  "Reconstruct solution path by following came-from pointers."
  (loop with path = '()
        with current = final-state
        do (push current path)
        while (gethash current came-from)
        do (setf current (gethash current came-from))
        finally (return path)))

(defun a-star-search (start-state &key (heuristic #'manhattan-distance))
  "A* search from start-state to *goal-state*.
   Returns: list of states from start to goal, or NIL if unsolvable.
   Optional: specify heuristic function (default: manhattan-distance)."
  (unless (solvable-p start-state)
    (format t "~%Puzzle configuration is UNSOLVABLE (odd inversions).~%")
    (return-from a-star-search nil))
  
  (let ((open-set (list start-state))
        (came-from (make-hash-table :test #'equal))
        (g-score (make-hash-table :test #'equal))
        (f-score (make-hash-table :test #'equal))
        (closed-set (make-hash-table :test #'equal))
        (nodes-expanded 0))
    
    ;; Initialize scores for start state
    (setf (gethash start-state g-score) 0)
    (setf (gethash start-state f-score) (funcall heuristic start-state))
    
    (loop while open-set do
      (incf nodes-expanded)
      
      ;; Find state in open-set with lowest f-score
      (let ((current (reduce (lambda (a b)
                               (if (< (gethash a f-score most-positive-fixnum)
                                      (gethash b f-score most-positive-fixnum))
                                   a b))
                             open-set)))
        
        ;; Goal reached?
        (when (equal current *goal-state*)
          (format t "~%Solution found! Nodes expanded: ~d~%" nodes-expanded)
          (return-from a-star-search (reconstruct-path came-from current)))
        
        ;; Move current from open to closed
        (setf open-set (remove current open-set :test #'equal))
        (setf (gethash current closed-set) t)
        
        ;; Explore neighbors
        (dolist (neighbor (get-neighbors current))
          (unless (gethash neighbor closed-set)
            (let ((tentative-g (1+ (gethash current g-score))))
              (when (< tentative-g (gethash neighbor g-score most-positive-fixnum))
                ;; This path to neighbor is better
                (setf (gethash neighbor came-from) current)
                (setf (gethash neighbor g-score) tentative-g)
                (setf (gethash neighbor f-score)
                      (+ tentative-g (funcall heuristic neighbor)))
                (unless (member neighbor open-set :test #'equal)
                  (push neighbor open-set))))))))
    
    ;; No solution found (shouldn't happen for solvable puzzles)
    (format t "~%Search exhausted. Nodes expanded: ~d~%" nodes-expanded)
    nil))

;;; ============================================================================
;;; Display Functions
;;; ============================================================================

(defun print-state (state &optional (label ""))
  "Pretty-print a puzzle state as a 3x3 grid."
  (when (> (length label) 0)
    (format t "~%~a~%" label))
  (format t "+---+---+---+~%")
  (dotimes (row *grid-size*)
    (format t "|")
    (dotimes (col *grid-size*)
      (let ((tile (nth (rowcol->index row col) state)))
        (format t " ~a |" (if (zerop tile) " " tile))))
    (format t "~%+---+---+---+~%")))

(defun print-solution (path)
  "Print entire solution path with move numbers."
  (format t "~%========== SOLUTION PATH ==========~%")
  (loop for state in path
        for move from 0
        do (print-state state (format nil "Move ~d:" move)))
  (format t "~%Total moves: ~d~%~%" (1- (length path))))

;;; ============================================================================
;;; Main Solver Interface
;;; ============================================================================

(defun solve-puzzle (start-state &key (verbose t) (heuristic #'linear-conflict))
  "Solve 8-puzzle from start-state.
   Options:
     :verbose t/nil - print solution path
     :heuristic - heuristic function to use"
  (when verbose
    (print-state start-state "Initial State:"))
  
  (let ((path (a-star-search start-state :heuristic heuristic)))
    (cond
      ((null path)
       (format t "~%No solution found.~%"))
      (verbose
       (print-solution path))
      (t
       (format t "~%Solution found in ~d moves.~%" (1- (length path)))))
    path))

;;; ============================================================================
;;; Example Puzzles
;;; ============================================================================

(defparameter *example-easy* '(1 2 3 4 5 6 7 8 0))      ; Already solved
(defparameter *example-simple* '(1 2 3 4 5 6 0 7 8))    ; 2 moves
(defparameter *example-medium* '(7 2 4 5 0 6 8 3 1))    ; ~20 moves
(defparameter *example-hard* '(8 6 7 2 5 4 3 0 1))      ; ~30 moves

;;; ============================================================================
;;; Auto-run Example
;;; ============================================================================

(defun run-examples ()
  "Run all example puzzles."
  (format t "~%~%====== TESTING 8-PUZZLE SOLVER ======~%")
  (solve-puzzle *example-simple* :verbose nil)
  (terpri)
  (solve-puzzle *example-medium* :verbose t))

;;; ============================================================================
;;; Auto-run Demo
;;; ============================================================================

;;; Uncomment the line below to run examples automatically when file is loaded
(run-examples)

;;; REPL Usage:
;;; (solve-puzzle *example-hard*)
;;; (solve-puzzle '(3 1 2 4 5 6 7 8 0) :heuristic #'manhattan-distance)
