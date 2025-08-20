# Water Jug Problem Solver

## Problem Statement
The **Water Jug Problem** is a classic puzzle:  
You are given two jugs with known capacities `x` and `y`. You need to measure out exactly `goal` liters of water using only the following allowed operations:
1. Fill a jug completely.
2. Empty a jug completely.
3. Pour water from one jug into the other until one jug is either full or empty.

This program solves the problem using a step-by-step simulation of operations.

---

## Objective
- To determine if the **goal volume** can be measured using the given jug sizes.
- If possible, show the sequence of jug states `(amount_x, amount_y)` that leads to the goal.
- Verify solvability using the **greatest common divisor (GCD)** rule:  
  The goal is achievable if and only if `goal` is a multiple of `gcd(x, y)` and `goal ≤ max(x, y)`.

---

## Data Structures Used
- **Integer Variables**:
  - `x`, `y`: Capacities of the jugs.
  - `amount_x`, `amount_y`: Current volumes of water in each jug.
- **Functions**: Used to represent state-changing operations.

---

## Algorithms

### 1. State Generation
State is represented as `(amount_x, amount_y)` — volumes of water in each jug.
The possible transitions are:
- **Fill** a jug:
  - `fill_x()`: Set `amount_x = x`
  - `fill_y()`: Set `amount_y = y`
- **Empty** a jug:
  - `empty_x()`: Set `amount_x = 0`
  - `empty_y()`: Set `amount_y = 0`
- **Pour** water from one jug into another until one is empty or full:
  - `pour_x_to_y()`: Pour from jug X to Y.
  - `pour_y_to_x()`: Pour from jug Y to X.

### 2. Goal Checking
```cpp
bool reached_goal(int goal) {
    return (amount_x == goal || amount_y == goal);
}
```

### 3. Solvability Check (Mathematical)
```cpp
gcd_val = gcd(x, y)
if (goal > max(x, y) || goal % gcd_val != 0):
    Not achievable
```

---

## Utility Functions
- **fill_x / fill_y**: Fill the jug to its capacity.
- **empty_x / empty_y**: Empty the jug.
- **pour_x_to_y / pour_y_to_x**: Transfer water between jugs.
- **reached_goal**: Check if current state has the goal volume.

---

## Use Cases
1. **Measuring specific quantities** where only certain container sizes are available.
2. **Real-world applications**:
   - Mixing solutions in a lab.
   - Cooking measurements without precise measuring tools.
   - Resource allocation in manufacturing.

---

## Sample Run

### Input:
```
Enter capacity of jug x: 4
Enter capacity of jug y: 3
Enter goal volume: 2
```

### Output:
```
Starting state:
(0, 0)
(4, 0)
(1, 3)
(1, 0)
(0, 1)
(4, 1)
(2, 3)
Goal 2 reached.
```

---

## Example Explanation
For **x = 4**, **y = 3**, **goal = 2**:
1. Fill jug X: `(4, 0)`
2. Pour from X to Y: `(1, 3)`
3. Empty jug Y: `(1, 0)`
4. Pour from X to Y: `(0, 1)`
5. Fill jug X: `(4, 1)`
6. Pour from X to Y: `(2, 3)` — goal reached.

---

## Complexity
- **Time Complexity**: O(max(x, y)) steps in worst case.
- **Space Complexity**: O(1) since only current state is stored.

---
