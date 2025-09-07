# Tic Tac Toe using MinMax with Alpha-Beta Pruning  

## Aim  
The aim of this project is to implement a **Tic Tac Toe game** where the user competes against the computer.  
The computer uses the **MinMax algorithm with Alpha-Beta Pruning** to efficiently choose the optimal moves, ensuring it never loses while reducing unnecessary computations.  

---

## Data Structures Used  
- **Vector (`vector<char>`)**:  
  - Represents the 3x3 game board.  
  - Each element stores `'X'` (user), `'O'` (AI), or `' '` (empty).  

- **2D Array (`int wins[8][3]`)**:  
  - Stores all possible winning positions (rows, columns, diagonals).  

- **Constants**:  
  - `USER = 'X'` → Player symbol.  
  - `AI = 'O'` → Computer symbol.  
  - `EMPTY = ' '` → Empty cell.  

---

## Algorithm (MinMax with Alpha-Beta Pruning)  

1. **State Generation**  
   - The game board is represented as a vector of 9 cells.  
   - On each turn:  
     - The user places `'X'` in the chosen position.  
     - The AI calculates its move using **MinMax with pruning**.  

2. **AI Move Selection (Alpha-Beta MinMax)**  
   - **Terminal Check**:  
     - If a player has won → return `+1` (User win) or `-1` (AI win).  
     - If no moves left → return `0` (Draw).  

   - **Recursive Case**:  
     - If it is AI’s turn:  
       - Try all empty cells by placing `'O'`.  
       - Recursively call `MinMax` for the user’s turn.  
       - Track the **minimum score**.  
       - Update `beta`. If `alpha >= beta`, prune remaining branches.  
     - If it is User’s turn:  
       - Try all empty cells by placing `'X'`.  
       - Recursively call `MinMax` for the AI’s turn.  
       - Track the **maximum score**.  
       - Update `alpha`. If `alpha >= beta`, prune remaining branches.  

   - **Move Evaluation**:  
     - The `findMove()` function tests all valid moves.  
     - For each move, it simulates AI placement and calls `MinMax`.  
     - The move with the lowest score is chosen as the AI’s optimal move.  

3. **Winner Detection**  
   - Uses the predefined `wins` array of winning triplets.  
   - Returns:  
     - `+1` if User wins.  
     - `-1` if AI wins.  
     - `0` if draw or ongoing game.  

---

## Utility Functions  
- **`printBoard(board)`** → Displays the current board.  
- **`checkWinner(board)`** → Detects if either player has won.  
- **`movesLeft(board)`** → Checks for available empty spaces.  
- **`MinMax(board, playerAI, alpha, beta)`** → Recursive MinMax with Alpha-Beta pruning.  
- **`findMove(board)`** → Finds the AI’s best move.  

---

## Use Cases  
- Demonstrating **Alpha-Beta Pruning optimization** in MinMax.  
- Efficient AI implementation for **two-player deterministic games**.  
- Educational project for **game theory, recursion, and pruning**.  
- Can be extended to more complex games (Connect Four, simplified Chess).  

---

## Advantages  
- **Optimal gameplay**: The AI never loses.  
- **Alpha-Beta Pruning** reduces unnecessary branches → much faster than plain MinMax.  
- Demonstrates practical use of **recursion, backtracking, and pruning**.  
- Works well for small board games like Tic Tac Toe.  

---

## Limitations  
- Still has **exponential time complexity** in the worst case, though improved by pruning.  
- Cannot scale efficiently to very large games (like Chess or Go) without heuristics.  
- Purely rule-based → no learning from past games.  

---

## Time Complexity  
- **MinMax without pruning**: `O(b^d)` where  
  - `b = branching factor (≤ 9)`  
  - `d = depth of the game tree (≤ 9)`  
- **With Alpha-Beta Pruning**:  
  - Best case → **O(b^(d/2))**  
  - Worst case → same as plain MinMax (`O(b^d)`)  

---

## Space Complexity  
- Maximum recursion depth = 9 moves.  
- Each board uses **O(9) = O(1)** space.  
- **Overall Space Complexity**: **O(d) = O(9) = O(1)** (constant space).  

---

## 🖥️ Sample Output  
```text
Tic Tac Toe (User=X, Computer=O)

Board:
 1 | 2 | 3
---+---+---
 4 | 5 | 6
---+---+---
 7 | 8 | 9

Enter your move (1-9): 5

Board:
 1 | 2 | 3
---+---+---
 4 | X | 6
---+---+---
 7 | 8 | 9

Computer chooses position 1

Board:
 0 | 2 | 3
---+---+---
 4 | X | 6
---+---+---
 7 | 8 | 9

Enter your move (1-9): 9

Board:
 0 | 2 | 3
---+---+---
 4 | X | 6
---+---+---
 7 | 8 | X

Computer chooses position 3

Board:
 0 | 2 | 0
---+---+---
 4 | X | 6
---+---+---
 7 | 8 | X

Enter your move (1-9): 2

Board:
 0 | X | 0
---+---+---
 4 | X | 6
---+---+---
 7 | 8 | X

Computer chooses position 8

Board:
 0 | X | 0
---+---+---
 4 | X | 6
---+---+---
 7 | 0 | X

Enter your move (1-9): 7

Board:
 0 | X | 0
---+---+---
 4 | X | 6
---+---+---
 X | 0 | X

Computer chooses position 4

Board:
 0 | X | 0
---+---+---
 0 | X | 6
---+---+---
 X | 0 | X

Enter your move (1-9): 6

Board:
 0 | X | 0
---+---+---
 0 | X | X
---+---+---
 X | 0 | X

It's a Draw!
```

