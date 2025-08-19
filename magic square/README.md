# Magic Square Generator

This project generates **Magic Squares** of any order `n ≥ 3` using different algorithms depending on the parity of `n`:

1. **Odd Order Magic Square** (n is odd) — Siamese method
2. **Doubly Even Order Magic Square** (n divisible by 4) — Inversion method
3. **Singly Even Order Magic Square** (n divisible by 2 but not by 4) — Strachey method

---

## **1. Odd Order Magic Square (Siamese Method)**
- Works for odd values of `n` (e.g., 3, 5, 7...).
- Start at the middle row and last column, placing numbers sequentially.
- If the next position is out of bounds, wrap around.
- If the next position is already filled or both row and column go out of bounds, move one step left instead.

**Pseudocode:**
```text
Initialize n×n matrix with 0
Set position: row = n / 2, col = n - 1
For num from 1 to n*n:
    Place num at (row, col)
    If num % n == 0:
        Move left(col--)
    Else:
        Move up(row--) and move right(col++)
    Wrap around if row < 0 → row = n - 1
    Wrap around if col == n → col = 0
```

---

## **2. Doubly Even Order Magic Square (Inversion Method)**
- Works when `n % 4 == 0` (e.g., 4, 8, 12...).
- Fill matrix sequentially from 1 to n².
- Invert specific cells based on position patterns.

**Pseudocode:**
```text
Fill matrix with numbers from 1 to n*n
For each cell (i, j):
    If (i % 4 == j % 4) OR (i % 4 + j % 4 == 3):
        Replace value with (n*n + 1 - value)
```

---

## **3. Singly Even Order Magic Square (Strachey Method)**
- Works when `n % 2 == 0` but `n % 4 != 0` (e.g., 6, 10, 14...).
- Divide matrix into four quadrants.
- Fill each quadrant using the odd-order method.
- Swap certain columns between quadrants.

**Pseudocode:**
```text
Let halfN = n / 2
Generate odd-order magic square of size halfN
Place copies into 4 quadrants with offsets:
    Top-left: original values
    Top-right: + 2 * (halfN^2)
    Bottom-left: + 3 * (halfN^2)
    Bottom-right: + (halfN^2)
Determine k = (n - 2) / 4
Swap columns in first k and last k positions between top and bottom halves
Swap two middle cells for symmetry correction
```

---

## **Time Complexity Analysis**

| Method                 | Time Complexity | Space Complexity | Notes |
|------------------------|----------------|-----------------|-------|
| Odd Order (Siamese)    | O(n²)          | O(n²)           | Simple and efficient |
| Doubly Even (Inversion)| O(n²)          | O(n²)           | Fast, pattern-based |
| Singly Even (Strachey) | O(n²)          | O(n²)           | More complex due to quadrant swaps |

---

## **How to Run**
1. Compile and execute:
    ```bash
    g++ magic_square.cpp -o magic_square
    ./magic_square
    ```
2. Enter the order of the magic square when prompted.

---

## **Example Output (n = 3)**

```
Enter order of magic square: 3
2 7 6
9 5 1
4 3 8
```

---

## **Conclusion**
- **Odd Order**: Best for small odd sizes — clean and quick.
- **Doubly Even**: Pattern-based, very fast.
- **Singly Even**: Most complex but handles remaining cases.
