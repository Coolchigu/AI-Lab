import sys

# --- Piece Classes ---

class Piece:
    """
    The base class for all chess pieces.
    """
    def __init__(self, color, symbol):
        self.color = color
        self.symbol = symbol if color == 'w' else symbol.lower()
        self.has_moved = False

    def __repr__(self):
        """Returns the piece's symbol for printing the board."""
        return self.symbol

    def get_moves(self, board, r, c):
        """
        Abstract method to be overridden by each piece.
        Returns a list of (row, col) tuples.
        """
        raise NotImplementedError

    def _get_line_moves(self, board, r, c, dr, dc):
        """Helper for Rooks, Bishops, Queens: Gets all moves in a line."""
        moves = []
        nr, nc = r + dr, c + dc
        while 0 <= nr < 8 and 0 <= nc < 8:
            dest = board[nr][nc]
            if dest is None:
                moves.append((nr, nc))
            elif dest.color != self.color:
                moves.append((nr, nc))  # Capture
                break  # Stop after capture
            else:
                break  # Blocked by own piece
            nr, nc = nr + dr, nc + dc
        return moves

    def _get_pawn_moves(self, board, r, c):
        """Helper for Pawn moves (the most complex)."""
        moves = []
        direction = -1 if self.color == 'w' else 1
        
        # 1. Forward 1
        nr, nc = r + direction, c
        if 0 <= nr < 8 and board[nr][nc] is None:
            moves.append((nr, nc))

            # 2. Forward 2 (from start)
            if not self.has_moved and board[r + 2 * direction][c] is None:
                moves.append((r + 2 * direction, c))

        # 3. Captures
        for dc in [-1, 1]:
            nr, nc = r + direction, c + dc
            if 0 <= nr < 8 and 0 <= nc < 8:
                dest = board[nr][nc]
                if dest is not None and dest.color != self.color:
                    moves.append((nr, nc))
        
        # Note: En Passant and Promotion are not implemented
        return moves


class King(Piece):
    def __init__(self, color):
        super().__init__(color, 'K')

    def get_moves(self, board, r, c):
        moves = []
        for dr in [-1, 0, 1]:
            for dc in [-1, 0, 1]:
                if dr == 0 and dc == 0:
                    continue
                nr, nc = r + dr, c + dc
                if 0 <= nr < 8 and 0 <= nc < 8:
                    dest = board[nr][nc]
                    if dest is None or dest.color != self.color:
                        moves.append((nr, nc))
        # Note: Castling is not implemented
        return moves


class Queen(Piece):
    def __init__(self, color):
        super().__init__(color, 'Q')

    def get_moves(self, board, r, c):
        moves = []
        for dr in [-1, 0, 1]:
            for dc in [-1, 0, 1]:
                if dr == 0 and dc == 0:
                    continue
                moves.extend(self._get_line_moves(board, r, c, dr, dc))
        return moves


class Rook(Piece):
    def __init__(self, color):
        super().__init__(color, 'R')

    def get_moves(self, board, r, c):
        moves = []
        moves.extend(self._get_line_moves(board, r, c, 1, 0))
        moves.extend(self._get_line_moves(board, r, c, -1, 0))
        moves.extend(self._get_line_moves(board, r, c, 0, 1))
        moves.extend(self._get_line_moves(board, r, c, 0, -1))
        return moves


class Bishop(Piece):
    def __init__(self, color):
        super().__init__(color, 'B')

    def get_moves(self, board, r, c):
        moves = []
        moves.extend(self._get_line_moves(board, r, c, 1, 1))
        moves.extend(self._get_line_moves(board, r, c, 1, -1))
        moves.extend(self._get_line_moves(board, r, c, -1, 1))
        moves.extend(self._get_line_moves(board, r, c, -1, -1))
        return moves


class Knight(Piece):
    def __init__(self, color):
        super().__init__(color, 'N')

    def get_moves(self, board, r, c):
        moves = []
        possible_moves = [
            (r - 2, c - 1), (r - 2, c + 1), (r - 1, c - 2), (r - 1, c + 2),
            (r + 1, c - 2), (r + 1, c + 2), (r + 2, c - 1), (r + 2, c + 1)
        ]
        for nr, nc in possible_moves:
            if 0 <= nr < 8 and 0 <= nc < 8:
                dest = board[nr][nc]
                if dest is None or dest.color != self.color:
                    moves.append((nr, nc))
        return moves


class Pawn(Piece):
    def __init__(self, color):
        super().__init__(color, 'P')

    def get_moves(self, board, r, c):
        return self._get_pawn_moves(board, r, c)


# --- Game Class ---

class Game:
    """
    The main class that manages the board, pieces, and game loop.
    """
    def __init__(self):
        self.board = self._create_empty_board()
        self._setup_pieces()
        self.turn = 'w'

    def _create_empty_board(self):
        """Creates an 8x8 list of lists, all initialized to None."""
        return [[None for _ in range(8)] for _ in range(8)]

    def _setup_pieces(self):
        """Places all 32 pieces on the board."""
        # Place Rooks, Knights, Bishops
        for c, P in enumerate([Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]):
            self.board[0][c] = P('b')
            self.board[7][c] = P('w')
        
        # Place Pawns
        for c in range(8):
            self.board[1][c] = Pawn('b')
            self.board[6][c] = Pawn('w')

    def print_board(self):
        """Prints the current board state to the console."""
        print("\n  a b c d e f g h")
        print(" +-----------------+")
        for r in range(8):
            row_str = f"{8 - r}|"
            for c in range(8):
                piece = self.board[r][c]
                row_str += f" {piece if piece else '.'}"
            print(f"{row_str} |{8 - r}")
        print(" +-----------------+")
        print("  a b c d e f g h\n")

    def _parse_move(self, move_str):
        """
        Parses a move string (e.g., "e2 e4") into ((r,c), (r,c)) coordinates.
        Returns (from_sq, to_sq) or (None, None) if invalid.
        """
        try:
            from_str, to_str = move_str.split()
            if len(from_str) != 2 or len(to_str) != 2:
                return None, None
            
            from_c = ord(from_str[0]) - ord('a')
            from_r = 8 - int(from_str[1])
            to_c = ord(to_str[0]) - ord('a')
            to_r = 8 - int(to_str[1])

            if not (0 <= from_r < 8 and 0 <= from_c < 8 and 0 <= to_r < 8 and 0 <= to_c < 8):
                return None, None
                
            return (from_r, from_c), (to_r, to_c)
        except Exception:
            return None, None

    def make_move(self, from_sq, to_sq):
        """
        Attempts to make a move. Returns True if successful, False otherwise.
        """
        from_r, from_c = from_sq
        to_r, to_c = to_sq
        
        piece = self.board[from_r][from_c]
        
        # 1. Check if there's a piece
        if piece is None:
            print("Error: No piece at start square.", file=sys.stderr)
            return False
            
        # 2. Check if it's the correct player's turn
        if piece.color != self.turn:
            print("Error: Not your piece.", file=sys.stderr)
            return False
            
        # 3. Check if the move is in the piece's pseudo-legal moves
        #    THIS IS THE SIMPLIFIED PART. It doesn't check for "check".
        possible_moves = piece.get_moves(self.board, from_r, from_c)
        if (to_r, to_c) not in possible_moves:
            print(f"Error: Invalid move for {piece.__class__.__name__}.", file=sys.stderr)
            return False
            
        # --- If all checks pass, make the move ---
        captured = self.board[to_r][to_c]
        if captured:
            print(f"Captured {captured.__class__.__name__}!")

        self.board[to_r][to_c] = piece
        self.board[from_r][from_c] = None
        piece.has_moved = True
        
        # Switch turns
        self.turn = 'b' if self.turn == 'w' else 'w'
        return True


    def run(self):
        """The main game loop."""
        while True:
            self.print_board()
            player = "White" if self.turn == 'w' else "Black"
            move_str = input(f"{player}'s turn. Enter move (e.g., e2 e4) or 'q' to quit: ")

            if move_str.lower() == 'q':
                print("Exiting game.")
                break

            from_sq, to_sq = self._parse_move(move_str)
            if from_sq is None:
                print("Error: Invalid move format. Use 'a1 h8' format.", file=sys.stderr)
                continue
            
            self.make_move(from_sq, to_sq)


# --- Main execution ---

if __name__ == "__main__":
    game = Game()
    game.run()
