# Path Finding Using A* Search Algorithm  

## Aim  
The aim of this project is to implement the **A\* Search Algorithm** to find the optimal path from an initial position to a goal position in a 2D grid, while avoiding obstacles (rivers). The algorithm guarantees an optimal solution by combining the cost to reach a node (g) with the estimated cost to reach the goal (h).  

---

## Data Structures Used  
1. **`vector<int>`** – Represents a position on the grid as `{row, col}`.  
2. **`priority_queue<Node>`** – Stores nodes to be expanded, ordered by `f = g + h`.  
3. **`unordered_map<string, vector<int>> (um)`** – Maps encoded positions to their coordinates.  
4. **`unordered_map<string, string> (parent)`** – Tracks parent of each state for path reconstruction.  
5. **`unordered_map<string, double> (g_cost)`** – Stores the best-known cost from start to a node.  
6. **`set<string>` (closed set)** – Keeps track of already expanded nodes.  
7. **`vector<vector<int>>` (river)** – Grid representation marking obstacles (`1` for river, `0` for free cell).  

---

## Algorithm – A* Search  
1. **Initialize**:  
   - Push the start node into the priority queue with `f = h(start)`.  
   - Set its `g_cost = 0`.  
2. **Loop until goal is found or queue is empty**:  
   - Pop node with smallest `f`.  
   - If it matches the goal, reconstruct the path.  
   - Otherwise, expand all valid neighbors:  
     - Compute step cost (`1` for straight, `1.5` for diagonal).  
     - Update `g_cost` and push into the queue if a better path is found.  
   - Mark node as visited in the closed set.  
3. **Path Reconstruction**: Trace parents back from goal to start.  
4. **Output**: Display path coordinates and visual grid with:  
   - `i` → initial  
   - `f` → goal  
   - `*` → path  
   - `#` → river (obstacle)  

---

## State Generation  
The blank position (agent) can move in **8 directions** if valid and not blocked:  
- Up `(x-1, y)`  
- Down `(x+1, y)`  
- Left `(x, y-1)`  
- Right `(x, y+1)`  
- Diagonal moves: `(x±1, y±1)`  

---

## Utility Functions  
- **`checkGoal(curr, goal)`** → Checks if current position equals the goal.  
- **`encode(curr)`** → Converts coordinates into a unique string key.  
- **`isValid(x,y,size,river)`** → Validates if a position is within bounds and not a river.  
- **`h_value(curr, goal)`** → Manhattan distance heuristic.  
- **`genMove(state, size, river)`** → Generates all valid moves.  
- **`Path(parent, goalKey, um, size, river, initial, goal)`** → Reconstructs and prints the final path.  
- **`solve(initial, goal, size, river)`** → Main A* implementation.  

---

## Use Cases  
- Pathfinding in **robotics and navigation systems**.  
- Route optimization in **maps and games**.  
- Grid-based **AI search problems**.  
- Demonstrating **informed search algorithms** in Artificial Intelligence.  

---

## Time Complexity  
- Each state expansion considers at most **8 neighbors**.  
- Priority queue operations: **O(log n)** per insertion.  
- Worst-case: **O(E log V)** where V = number of cells, E = number of edges (≤ 8V).  
- **Overall Complexity**: O(V log V).  

## Space Complexity  
- Stores `g_cost`, `parent`, and `um` for visited states: **O(V)**.  
- Each state requires O(1) space.  
- **Overall Space**: O(V).  

---

## Sample Output  

### Example 1 – With Obstacles  
Input:
```text
Enter size of matrix:5
Enter coordinates of initial position(indexed from 0):0 0
Enter  coordinates of goal position(indexed from 0):2 3
Enter size of river:3
Enter points of river:
Point 1:1 1
Point 2:2 2
Point 3:3 3
```
Output:
```text
Path Matrix:
i * . . . 
. # * . .
. . # f .
. . . # .
. . . . .

Path found:
(0,0) (0,1) (1,2) (2,3)
```


---

## Advantages of A*  
- Guarantees the **optimal path** if heuristic is admissible.  
- Efficient compared to uninformed searches like BFS/DFS.  
- Handles weighted costs (e.g., diagonal movement with cost 1.5).  

## Limitations  
- Memory-intensive for large grids.  
- Performance depends heavily on the heuristic.  
- May still explore many states in worst-case scenarios.  

