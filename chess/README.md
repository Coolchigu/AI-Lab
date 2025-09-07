## AIM
To implement a **2D Chess Game** with **Human vs AI** functionality using **Pygame** and **Alpha-Beta Pruning** for AI decision-making.

---

## ALGORITHM

### 1. Board Representation
- The board is an **8×8 grid** represented as a 2D array.  
- Each square contains either `None` or a `Piece` object.  
- `Piece` object properties:  
  - `type` → Pawn, Rook, Knight, Bishop, Queen, King  
  - `color` → White or Black  
  - `position` → current coordinates `(x, y)`  

### 2. Move Generation
- For a selected piece, generate all **legal moves** based on chess rules.  
- Moves are filtered for:  
  - Board boundaries  
  - Captures  
  - Check prevention  

### 3. Human Interaction
- **Mouse click** selects a piece.  
- **Highlighting** shows valid moves.  
- Clicking a valid destination moves the piece.  

### 4. AI Opponent (Alpha-Beta Pruning)
- AI evaluates board states to a certain **depth** (e.g., 3 moves ahead).  
- **Evaluation Function**:  
  - Piece values (Pawn=1, Knight/Bishop=3, Rook=5, Queen=9, King=∞)  
  - Positional advantage (central control)  
  - Mobility (number of legal moves)  
- **Alpha-Beta Pruning** reduces unnecessary search branches, optimizing AI computation.  

### 5. Game States
- **Check** → King in danger is highlighted.  
- **Checkmate / Game Over** → No legal moves for current player.  
- **Forfeit** → Player can end the game using a button.  

---

## TIME COMPLEXITY
- **AI Move Calculation:** O(b^d) where `b` = average legal moves per position, `d` = depth of search.  
- **Alpha-Beta Pruning** reduces branches, improving efficiency closer to O(b^(d/2)) in best-case scenarios.  
- **Human Moves** are instantaneous, just move validation.
