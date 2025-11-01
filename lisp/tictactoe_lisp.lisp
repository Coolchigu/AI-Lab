;;; tictactoe.lisp
;;; Tic-Tac-Toe game with Minimax AI (Common Lisp)
;;; Board representation: list of 9 elements (NIL, X, or O)
;;; Positions numbered 1-9:
;;;   1 | 2 | 3
;;;   ---------
;;;   4 | 5 | 6
;;;   ---------
;;;   7 | 8 | 9

(in-package :cl-user)

;;; ============================================================================
;;; Constants and Configuration
;;; ============================================================================

(defparameter *player-x* 'X)
(defparameter *player-o* 'O)
(defparameter *empty* nil)

(defparameter *winning-lines* 
  '((0 1 2)  ; Top row
    (3 4 5)  ; Middle row
    (6 7 8)  ; Bottom row
    (0 3 6)  ; Left column
    (1 4 7)  ; Middle column
    (2 5 8)  ; Right column
    (0 4 8)  ; Diagonal \
    (2 4 6)) ; Diagonal /
  "All possible winning combinations (0-indexed).")

;;; ============================================================================
;;; Board Operations
;;; ============================================================================

(defun make-empty-board ()
  "Create a new empty 3x3 board."
  (make-list 9 :initial-element *empty*))

(defun copy-board (board)
  "Return a copy of the board."
  (copy-list board))

(defun get-cell (board position)
  "Get the value at position (1-9). Returns X, O, or NIL."
  (nth (1- position) board))

(defun set-cell (board position player)
  "Return new board with player's mark at position (1-9)."
  (let ((new-board (copy-board board)))
    (setf (nth (1- position) new-board) player)
    new-board))

(defun valid-move-p (board position)
  "Check if move at position (1-9) is valid."
  (and (>= position 1)
       (<= position 9)
       (null (get-cell board position))))

(defun get-empty-cells (board)
  "Return list of empty position numbers (1-9)."
  (loop for cell in board
        for pos from 1
        when (null cell)
        collect pos))

;;; ============================================================================
;;; Win Detection
;;; ============================================================================

(defun check-line (board line player)
  "Check if player has won on this line (list of 3 indices)."
  (every (lambda (idx) (eq (nth idx board) player)) line))

(defun winner (board)
  "Return X, O, or NIL if no winner yet."
  (cond
    ((some (lambda (line) (check-line board line *player-x*)) *winning-lines*)
     *player-x*)
    ((some (lambda (line) (check-line board line *player-o*)) *winning-lines*)
     *player-o*)
    (t nil)))

(defun board-full-p (board)
  "Check if board has no empty cells."
  (notany #'null board))

(defun game-over-p (board)
  "Check if game is over (win or draw)."
  (or (winner board) (board-full-p board)))

;;; ============================================================================
;;; Display Functions
;;; ============================================================================

(defun cell-display (cell position)
  "Return display string for a cell."
  (cond
    ((eq cell *player-x*) "X")
    ((eq cell *player-o*) "O")
    (t (format nil "~d" position))))

(defun print-board (board)
  "Display the board in a nice format."
  (format t "~%")
  (format t " ~a | ~a | ~a ~%"
          (cell-display (nth 0 board) 1)
          (cell-display (nth 1 board) 2)
          (cell-display (nth 2 board) 3))
  (format t "-----------~%")
  (format t " ~a | ~a | ~a ~%"
          (cell-display (nth 3 board) 4)
          (cell-display (nth 4 board) 5)
          (cell-display (nth 5 board) 6))
  (format t "-----------~%")
  (format t " ~a | ~a | ~a ~%"
          (cell-display (nth 6 board) 7)
          (cell-display (nth 7 board) 8)
          (cell-display (nth 8 board) 9))
  (format t "~%"))

;;; ============================================================================
;;; Minimax Algorithm with Alpha-Beta Pruning
;;; ============================================================================

(defun opponent (player)
  "Return the opponent of the given player."
  (if (eq player *player-x*) *player-o* *player-x*))

(defun evaluate-board (board player)
  "Evaluate board from player's perspective.
   Returns: +10 if player wins, -10 if opponent wins, 0 for draw/ongoing."
  (let ((win (winner board)))
    (cond
      ((eq win player) 10)
      ((eq win (opponent player)) -10)
      (t 0))))

(defun minimax (board player maximizing-player depth alpha beta)
  "Minimax algorithm with alpha-beta pruning.
   Returns: (score . best-move) where best-move is position number or NIL."
  (let ((game-winner (winner board)))
    (cond
      ;; Terminal states
      (game-winner
       (cons (evaluate-board board maximizing-player) nil))
      ((board-full-p board)
       (cons 0 nil))
      
      ;; Maximizing player's turn
      ((eq player maximizing-player)
       (let ((max-eval most-negative-fixnum)
             (best-move nil))
         (dolist (move (get-empty-cells board))
           (let* ((new-board (set-cell board move player))
                  (eval-score (car (minimax new-board 
                                           (opponent player)
                                           maximizing-player
                                           (1+ depth)
                                           alpha
                                           beta))))
             ;; Prefer shorter paths to victory
             (let ((adjusted-score (if (> eval-score 0)
                                      (- eval-score depth)
                                      (+ eval-score depth))))
               (when (> adjusted-score max-eval)
                 (setf max-eval adjusted-score)
                 (setf best-move move))
               (setf alpha (max alpha adjusted-score))
               (when (>= alpha beta)
                 (return)))))
         (cons max-eval best-move)))
      
      ;; Minimizing player's turn
      (t
       (let ((min-eval most-positive-fixnum)
             (best-move nil))
         (dolist (move (get-empty-cells board))
           (let* ((new-board (set-cell board move player))
                  (eval-score (car (minimax new-board 
                                           (opponent player)
                                           maximizing-player
                                           (1+ depth)
                                           alpha
                                           beta))))
             ;; Prefer shorter paths to defeat for opponent
             (let ((adjusted-score (if (< eval-score 0)
                                      (+ eval-score depth)
                                      (- eval-score depth))))
               (when (< adjusted-score min-eval)
                 (setf min-eval adjusted-score)
                 (setf best-move move))
               (setf beta (min beta adjusted-score))
               (when (>= alpha beta)
                 (return)))))
         (cons min-eval best-move))))))

(defun get-best-move (board player)
  "Get the best move for player using minimax algorithm.
   Returns position number (1-9)."
  (cdr (minimax board player player 0 most-negative-fixnum most-positive-fixnum)))

;;; ============================================================================
;;; Game Interface
;;; ============================================================================

(defun get-human-move (board)
  "Prompt human player for move and validate it."
  (loop
    (format t "Enter your move (1-9): ")
    (finish-output)
    (let ((input (read)))
      (cond
        ((not (numberp input))
         (format t "Invalid input. Please enter a number.~%"))
        ((not (valid-move-p board input))
         (format t "Invalid move. Cell ~d is not available.~%" input))
        (t (return input))))))

(defun play-game (&key (human-first t) (human-player *player-x*))
  "Main game loop. Play Tic-Tac-Toe against AI.
   Options:
     :human-first t/nil - Does human play first?
     :human-player X/O - Which symbol does human use?"
  (let ((board (make-empty-board))
        (ai-player (opponent human-player))
        (current-player (if human-first human-player ai-player)))
    
    (format t "~%========================================~%")
    (format t "   TIC-TAC-TOE: Human vs AI~%")
    (format t "========================================~%")
    (format t "You are playing as: ~a~%" human-player)
    (format t "AI is playing as: ~a~%" ai-player)
    
    (print-board board)
    
    ;; Main game loop
    (loop until (game-over-p board) do
      (let (move)
        (if (eq current-player human-player)
            ;; Human turn
            (progn
              (format t "~%Your turn (~a):~%" human-player)
              (setf move (get-human-move board)))
            ;; AI turn
            (progn
              (format t "~%AI's turn (~a)...~%" ai-player)
              (setf move (get-best-move board ai-player))
              (format t "AI chooses position ~d~%" move)))
        
        ;; Make the move
        (setf board (set-cell board move current-player))
        (print-board board)
        
        ;; Switch players
        (setf current-player (opponent current-player))))
    
    ;; Game over - announce result
    (format t "~%========================================~%")
    (let ((game-winner (winner board)))
      (cond
        ((eq game-winner human-player)
         (format t "   CONGRATULATIONS! You won!~%"))
        ((eq game-winner ai-player)
         (format t "   AI wins! Better luck next time.~%"))
        (t
         (format t "   It's a DRAW!~%"))))
    (format t "========================================~%~%")))

;;; ============================================================================
;;; AI vs AI Demo
;;; ============================================================================

(defun ai-vs-ai ()
  "Watch AI play against itself."
  (let ((board (make-empty-board))
        (current-player *player-x*))
    
    (format t "~%========================================~%")
    (format t "   TIC-TAC-TOE: AI vs AI~%")
    (format t "========================================~%")
    (print-board board)
    
    (loop until (game-over-p board) do
      (format t "~%~a's turn...~%" current-player)
      (let ((move (get-best-move board current-player)))
        (format t "~a chooses position ~d~%" current-player move)
        (setf board (set-cell board move current-player))
        (print-board board)
        (sleep 0.5)  ; Pause for readability
        (setf current-player (opponent current-player))))
    
    (format t "~%Result: ~a~%~%" 
            (let ((w (winner board)))
              (if w (format nil "~a wins!" w) "Draw!")))))

;;; ============================================================================
;;; Auto-run Demo
;;; ============================================================================

;;; Automatically start a game: Human vs AI
(format t "~%~%========================================~%")
(format t "   STARTING: Human vs AI~%")
(format t "========================================~%")
(play-game :human-first t :human-player *player-x*)

(format t "~%~%To play again, use:~%")
(format t "  (play-game)                              ; Play as X, go first~%")
(format t "  (play-game :human-first nil)             ; AI goes first~%")
(format t "  (play-game :human-player *player-o*)     ; Play as O~%")
(format t "  (ai-vs-ai)                               ; Watch AI vs AI~%")
