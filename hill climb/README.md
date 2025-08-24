# Solving 8-Puzzle Using Hill Climbing  

## Aim  
The aim of this project is to solve the **8-puzzle problem** using the **Hill Climbing algorithm** with a simple heuristic function. The algorithm attempts to reach the goal state from a given initial configuration by minimizing the heuristic cost (misplaced tiles).  

---

## Data Structures Used  
1. **`vector<vector<int>>` (Board)** – Represents the puzzle board as a 3x3 matrix.  
2. **`vector<pair<int, Board>>` (Queue)** – Stores states along with their heuristic values for processing.  
3. **`vector<Board>` (Visited List)** – Keeps track of already visited states to avoid revisiting.  
4. **`pair<int,int>`** – Used to represent the position of the blank tile (0).  

---

## ⚙️ Algorithm – Hill Climbing for 8 Puzzle  
1. Start with the initial configuration of the board.  
2. Compute the heuristic value **h(n)** (number of misplaced tiles).  
3. Generate possible child states by moving the blank tile (`0`) **up, down, left, right**.  
4. Evaluate each child using the heuristic.  
5. Choose the child with the **lowest heuristic value**.  
6. If no better child is found (local maxima), terminate.  
7. Repeat until the **goal state** is reached or no improvement is possible.  

---

## State Generation  
The blank tile (`0`) can be moved in **four directions** (if valid):  
- **Up** → `up(Board s)`  
- **Down** → `down(Board s)`  
- **Left** → `left(Board s)`  
- **Right** → `right(Board s)`  

Each function swaps the blank tile with its neighboring tile to produce a new board configuration.  

---

## Utility Functions  
- **`heuristic(Board curr, Board goal)`** → Returns the number of misplaced tiles.  
- **`find_pos(Board s)`** → Finds the position of the blank tile (0).  
- **`compare(Board a, Board b)`** → Checks if two boards are the same.  
- **`up/down/left/right(Board s)`** → Generates child states.  
- **`search(Board start, Board goal)`** → Performs hill climbing search and prints the result.  

---

## Use Cases  
- Solving small-scale puzzle problems.  
- Understanding **local search algorithms** (Hill Climbing).  
- Demonstrating concepts of **heuristic functions** in Artificial Intelligence.  
- Comparing heuristic-based approaches with **A*** or **BFS/DFS**.  

---

## Time Complexity  
- **State Generation**: O(1) per move.  
- **Heuristic Calculation**: O(9) = O(1).  
- **Search Loop**: Worst case explores multiple states → O(b^d),  
  where **b = branching factor (≤4)**, **d = depth of solution**.  
- **Overall Complexity**: Exponential in worst case.  

## Space Complexity  
- Storing visited states: O(n), where n is the number of unique states encountered.  
- Each state requires O(9) = O(1) space.  
- **Overall Space**: O(n).  

---


## Sample Output  

### Example 1 – Solvable Case  
Initial:  
```text
1 2 3
4 5 6
8 7 0
```
Goal:
```text
1 2 3
4 5 6
7 8 0
```
Output:
```text
Heuristic of child greater than parent: 3 > 2
Execution time: 2 ms
```

---

## Limitations  
- Can get stuck in **local maxima** or **plateaus**.  
- Not guaranteed to find the optimal solution.  
- Works best for demonstrating **hill climbing principles** rather than solving large puzzles.  

