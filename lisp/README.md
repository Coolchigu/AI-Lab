# Artificial Intelligence Practicals using Lisp

---

## 🧩 Practical 1: Tic-Tac-Toe Game using Lisp

### 🔹 Problem Statement
The Tic-Tac-Toe Game is a classic example of game playing and decision-making in Artificial Intelligence.  
The objective is to simulate a human vs computer game where the computer uses logical strategies to win or block the player.

The program implements Tic-Tac-Toe using Lisp with a one-dimensional board array representation and basic AI rules for move selection.

---

### 🎯 Objective
- Implement Tic-Tac-Toe logic using Lisp.  
- Use a one-dimensional array to represent the 3×3 board.  
- Apply AI logic to determine the computer’s move.  
- Check for win conditions and display the result interactively.

---

### 🧠 Knowledge Representation

| Concept | Representation in Lisp |
|----------|------------------------|
| Empty Cell | Represented by value `2` |
| Computer’s Move | Represented by value `3 (X)` |
| Player’s Move | Represented by value `5 (O)` |
| Board | One-dimensional array of size 10 (indices 1–9 used) |

---

### ⚙️ Functions Overview

| Function | Purpose |
|-----------|----------|
| `(is-blank pos)` | Checks if a given position is blank. |
| `(any-blank)` | Finds any available blank position. |
| `(make2)` | Chooses center or non-corner positions strategically. |
| `(posswin player-val)` | Checks if the player can win on the next move. |
| `(make-move pos)` | Places a mark (X or O) on the board. |
| `(display-board)` | Prints the current board state. |
| `(user-move)` | Accepts move input from the user. |
| `(check-win)` | Determines if there is a winner. |
| `(main)` | Controls the full game loop. |

---

### 🧩 Algorithm (Game Logic)

**Steps:**
1. Initialize the board with all positions blank (`2`).  
2. Assign `X` to the computer and `O` to the user.  
3. Alternate turns between computer and player.  
4. For each computer turn:  
   - Check if it can win (`posswin 3`).  
   - Else check if it needs to block (`posswin 5`).  
   - Else choose center or next blank cell.  
5. For each user turn:  
   - Input position and mark it as `O`.  
6. After each move, check for a winner using `check-win`.  
7. Stop the game when either player wins or all cells are filled.

---

### 💻 Sample Execution
```text
- - - 
- - - 
- - -
Enter your move (1-9): 1
Computer plays at 5
X - -
- O -
- - -
Enter your move (1-9): 4
Computer plays at 7
X - -
X O -
O - -
Enter your move (1-9): 3
Computer plays at 2
X O X
X O -
O - -
Enter your move (1-9): 7
Computer plays at 8
X O X
X O -
O O -
O wins!
```

---

### 📚 Use Cases
- **Artificial Intelligence:** Demonstrates rule-based AI decision-making.  
- **Game Development:** Simple model of turn-based logic games.  
- **Problem Solving:** Tests recursion and decision trees in Lisp.  
- **Learning Tool:** Helps understand Lisp loops, arrays, and conditions.

---

### ⏱️ Time and Space Complexity

| Operation | Time Complexity | Space Complexity |
|------------|----------------|-----------------|
| Checking Winning Combinations | O(1) | O(1) |
| Player Move Validation | O(1) | O(1) |
| Full Game Execution | O(N) (N ≤ 9) | O(N) |

The program runs efficiently in constant time per move, with linear time for the full game sequence.

---

### 📁 Files Used

| Filename | Description |
|-----------|-------------|
| `TicTacToe.lisp` | Lisp implementation of Tic-Tac-Toe game logic. |
| `README.md` | Documentation for the Tic-Tac-Toe implementation. |

---

## 🧠 Practical 2: 8-Puzzle Problem using Lisp (A* Search)

### 🔹 Problem Statement
The **8-Puzzle Problem** is a classic example of **state-space search** in Artificial Intelligence.  
The goal is to reach the target configuration of tiles by sliding the blank (0) in a 3×3 grid using valid moves (up, down, left, right).

This implementation uses the **A\*** search algorithm with the **Manhattan distance heuristic** to find the optimal solution path from the start state to the goal state.

---

### 🎯 Objective
- Implement the 8-Puzzle Problem using **A\*** search in Lisp.  
- Represent puzzle states as one-dimensional lists of 9 integers.  
- Use the **Manhattan distance** as a heuristic for estimating the cost to goal.  
- Verify if a given configuration is **solvable** before solving.  
- Print each intermediate state until the goal is reached.

---

### 🧠 Knowledge Representation

| Concept | Representation in Lisp |
|----------|------------------------|
| State | List of 9 integers, e.g., `(1 2 3 4 5 6 7 8 0)` |
| Blank Tile | Represented by `0` |
| Goal State | `(1 2 3 4 5 6 7 8 0)` |
| Move | Swapping blank (0) with adjacent tile |
| Heuristic | Manhattan distance between current and goal positions |
| Solvability | Determined using inversion count |

---

### ⚙️ Functions Overview

| Function | Purpose |
|-----------|----------|
| `(index->rowcol idx)` | Returns row and column for a given index. |
| `(manhattan state)` | Calculates total Manhattan distance for a state. |
| `(swap-at state i j)` | Swaps elements `i` and `j` in a state. |
| `(neighbors state)` | Returns a list of valid next moves from current state. |
| `(solvable-p state)` | Checks if a given configuration can be solved. |
| `(a-star start)` | Performs A\* search from start to goal state. |
| `(reconstruct-path came-from current)` | Reconstructs the final solution path. |
| `(print-state state)` | Displays the state in 3×3 format. |
| `(solve-and-print start)` | Solves the puzzle and prints each step. |

---

### 🧩 Algorithm (A\* Search Logic)

**Steps:**
1. Input the **initial state** of the puzzle (list of 9 integers).  
2. Verify **solvability** using inversion count.  
3. Initialize the **open list** with the start state and the **closed list** as empty.  
4. Calculate the cost `f(n) = g(n) + h(n)` for each state:  
   - `g(n)` = cost to reach the state  
   - `h(n)` = Manhattan heuristic  
5. Repeatedly choose the state with the **lowest f(n)** value.  
6. Generate all possible **neighbor states** (valid moves).  
7. For each neighbor:  
   - Compute tentative cost and update if better.  
   - Add to open list if not already visited.  
8. Stop when the goal `(1 2 3 4 5 6 7 8 0)` is reached.  
9. Reconstruct and display the path from start to goal.

---

### 💻 Sample Execution
```text
* (solve-and-print example-2)

Start:
-----------
1  2  3
4  5  6
_  7  8
-----------
Solution found in 2 moves.
-----------
1  2  3
4  5  6
_  7  8
-----------
-----------
1  2  3
4  5  6
7  _  8
-----------
-----------
1  2  3
4  5  6
7  8  _
-----------
```

---

### 📚 Use Cases
- **Artificial Intelligence:** Demonstrates the A\* informed search algorithm.  
- **Heuristic Search:** Uses Manhattan distance to guide efficient pathfinding.  
- **State-Space Exploration:** Models transitions between valid board configurations.  
- **Teaching Tool:** Useful for understanding heuristics and optimal search strategies.

---

### ⏱️ Time and Space Complexity

| Operation | Time Complexity | Space Complexity |
|------------|----------------|-----------------|
| Manhattan Distance | O(9) | O(1) |
| Neighbor Generation | O(4) | O(1) |
| A\* Search (Worst Case) | O(b^d) where b=4, d=depth | O(b^d) |

The A\* algorithm efficiently finds the optimal path but may consume exponential space depending on puzzle depth.

---

### 📁 Files Used

| Filename | Description |
|-----------|-------------|
| `8Puzzle.lisp` | Lisp implementation of the 8-Puzzle solver using A\* search. |
| `README.md` | Documentation for the 8-Puzzle and Tic-Tac-Toe implementations. |

---


