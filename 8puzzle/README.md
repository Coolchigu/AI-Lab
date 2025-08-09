# 8-Puzzle Solver

A Python implementation of the classic 8-puzzle problem using three different search algorithms: Depth-First Search (DFS), Breadth-First Search (BFS), and Best-First Search.

## Problem Description

The 8-puzzle consists of a 3×3 grid with 8 numbered tiles and one blank space. The objective is to rearrange the tiles from an initial configuration to a goal configuration by sliding tiles into the blank space.

**Example:**

```
Initial State:          Goal State:
1 2 3                   1 2 3
4 5 6                   4 5 6
7 8 _                   7 8 _
```

## Algorithms Implemented

### 1. Depth-First Search (DFS)
- Explores as far as possible along each branch before backtracking
- Uses a stack (LIFO) data structure
- **Time Complexity:** O(b^m) where b is branching factor and m is maximum depth
- **Space Complexity:** O(bm)
- May not find the optimal solution

### 2. Breadth-First Search (BFS)
- Explores all nodes at the present depth before moving to nodes at the next depth level
- Uses a queue (FIFO) data structure
- **Time Complexity:** O(b^d) where d is depth of optimal solution
- **Space Complexity:** O(b^d)
- Guarantees the optimal solution (shortest path)

### 3. Best-First Search
- Uses a heuristic function to estimate the cost to reach the goal
- Explores the most promising nodes first based on heuristic value
- Common heuristics:
  - **Manhattan Distance:** Sum of horizontal and vertical distances of tiles from their goal positions
  - **Misplaced Tiles:** Number of tiles not in their goal position
- **Time Complexity:** O(b^d) in worst case
- **Space Complexity:** O(b^d)
- Often finds optimal or near-optimal solutions efficiently

## Features

- Implementation of three search algorithms
- Visualization of solution path
- Performance metrics (nodes explored, time taken, solution depth)
- Solvability checker (not all initial states are solvable)
- Step-by-step solution display

## Requirements

```
Python 3.7+
```

Optional for visualization:
```
numpy
matplotlib
```

## Installation

1. Clone the repository:
```bash
git clone https://github.com/yourusername/8-puzzle-solver.git
cd 8-puzzle-solver
```

2. Install dependencies (if using visualization):
```bash
pip install -r requirements.txt
```

## Usage

### Basic Usage

```python
from puzzle_solver import PuzzleSolver

# Define initial and goal states
initial_state = [[1, 2, 3], [4, 5, 6], [7, 8, 0]]
goal_state = [[1, 2, 3], [4, 5, 6], [7, 8, 0]]

# Create solver instance
solver = PuzzleSolver(initial_state, goal_state)

# Solve using different algorithms
bfs_solution = solver.solve_bfs()
dfs_solution = solver.solve_dfs()
best_first_solution = solver.solve_best_first()

# Display results
solver.display_solution(bfs_solution)
solver.print_statistics()
```

### Command Line Interface

```bash
python main.py --algorithm bfs --initial "1,2,3,4,0,5,7,8,6"
python main.py --algorithm dfs --initial "1,2,3,4,0,5,7,8,6"
python main.py --algorithm best-first --initial "1,2,3,4,0,5,7,8,6"
```

### Available Options

- `--algorithm`: Choose algorithm (bfs, dfs, best-first)
- `--initial`: Initial state as comma-separated values (0 represents blank)
- `--goal`: Goal state (optional, defaults to solved state)
- `--visualize`: Display step-by-step visualization
- `--max-depth`: Maximum depth for DFS (default: 50)

## Project Structure

```
8-puzzle-solver/
│
├── main.py                 # Main entry point
├── puzzle_solver.py        # Core solver implementation
├── algorithms/
│   ├── bfs.py             # BFS implementation
│   ├── dfs.py             # DFS implementation
│   └── best_first.py      # Best-First Search implementation
├── utils/
│   ├── heuristics.py      # Heuristic functions
│   ├── validator.py       # Solvability checker
│   └── visualizer.py      # Solution visualization
├── tests/
│   └── test_solver.py     # Unit tests
├── requirements.txt        # Python dependencies
└── README.md              # This file
```

## Example Output

```
Algorithm: Breadth-First Search
Initial State:
1 2 3
4 0 5
7 8 6

Solution found in 3 moves!
Nodes explored: 12
Time taken: 0.003 seconds
Memory used: 1.2 MB

Solution Path:
Step 0:          Step 1:          Step 2:          Step 3:
1 2 3            1 2 3            1 2 3            1 2 3
4 0 5     →      4 5 0     →      4 5 6     →      4 5 6
7 8 6            7 8 6            7 8 0            7 8 0
```

## Algorithm Comparison

| Algorithm | Completeness | Optimality | Time Complexity | Space Complexity |
|-----------|--------------|------------|-----------------|------------------|
| DFS | No (with cycle detection: Yes) | No | O(b^m) | O(bm) |
| BFS | Yes | Yes | O(b^d) | O(b^d) |
| Best-First | Yes | Depends on heuristic | O(b^d) | O(b^d) |

## Testing

Run unit tests:
```bash
python -m pytest tests/
```

Run with coverage:
```bash
python -m pytest --cov=. tests/
```

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/AmazingFeature`)
3. Commit your changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Acknowledgments

- Based on classical AI search algorithms
- Inspired by Russell & Norvig's "Artificial Intelligence: A Modern Approach"
- Manhattan distance heuristic for Best-First Search

## References

- Russell, S., & Norvig, P. (2020). *Artificial Intelligence: A Modern Approach* (4th ed.)
- Cormen, T. H., et al. (2009). *Introduction to Algorithms* (3rd ed.)

