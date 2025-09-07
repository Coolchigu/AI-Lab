#include<bits/stdc++.h>
using namespace std;

const char USER = 'X';
const char AI = '0';
const char EMPTY = ' ';

void printBoard(const vector<char>& b) {
    cout << "\nBoard:\n";
    for (int r = 0; r < 3; ++r) {
        cout << " ";
        for (int c = 0; c < 3; ++c) {
            int i = r * 3 + c;
            cout << (b[i] == EMPTY ? char('1' + i) : b[i]);
            if (c < 2) cout << " | ";
        }
        cout << "\n";
        if (r < 2) cout << "---+---+---\n";
    }
    cout << "\n";
}

int checkWinner(vector<char>& b) {
    int wins[8][3] = {
        {0,1,2}, {3,4,5}, {6,7,8},
        {0,3,6}, {1,4,7}, {2,5,8},
        {0,4,8}, {2,4,6}
    };
    for (auto& w : wins) {
        if (b[w[0]] != EMPTY && b[w[0]] == b[w[1]] && b[w[1]] == b[w[2]]) {
            return b[w[0]] == AI ? -1 : 1;
        }
    }
    return 0;
}

bool movesLeft(vector<char>& b) {
    for (char c : b) {
        if (c == EMPTY) return true;
    }
    return false;
}

int MinMax(vector<char>& b, bool playerAI, int alpha, int beta) {
    int score = checkWinner(b);
    if (score == 1 || score == -1) return score;
    if (!movesLeft(b)) return 0;

    if (playerAI) {
        int best = 1000;
        for (int i = 0; i < 9; i++) {
            if (b[i] == EMPTY) {
                b[i] = AI;
                best = min(best, MinMax(b, false, alpha, beta));
                b[i] = EMPTY;
                beta = min(beta, best);
                if (alpha >= beta) break;
            }
        }
        return best;
    } else { 
        int best = -1000;
        for (int i = 0; i < 9; i++) {
            if (b[i] == EMPTY) {
                b[i] = USER;
                best = max(best, MinMax(b, true, alpha, beta));
                b[i] = EMPTY;
                alpha = max(alpha, best);
                if (alpha >= beta) break;
            }
        }
        return best;
    }
}

int findMove(vector<char>& b) {
    int bestVal = 1000;
    int bestMove = -1;

    for (int i = 0; i < 9; i++) {
        if (b[i] == EMPTY) {
            b[i] = AI;
            int moveVal = MinMax(b, false, -1000, 1000);
            b[i] = EMPTY;

            if (moveVal < bestVal) {
                bestMove = i;
                bestVal = moveVal;
            }
        }
    }
    return bestMove;
}

int main() {
    vector<char> board(9, EMPTY);
    cout << "Tic Tac Toe (User=X, Computer=O)" << endl;
    printBoard(board);

    while (true) {
        int userMove;
        cout << "Enter your move (1-9): ";
        cin >> userMove;
        userMove--;
        if (userMove < 0 || userMove > 8 || board[userMove] != EMPTY) {
            cout << "Invalid Move! Try again." << endl;
            continue;
        }
        board[userMove] = USER;
        printBoard(board);

        if (checkWinner(board) == 1) {
            cout << "You win!" << endl;
            break;
        }
        if (!movesLeft(board)) {
            cout << "It's a Draw!" << endl;
            break;
        }

        int moveAi = findMove(board);
        board[moveAi] = AI;
        cout << "Computer chooses position " << moveAi + 1 << endl;
        printBoard(board);

        if (checkWinner(board) == -1) {
            cout << "Computer Wins!" << endl;
            break;
        }
        if (!movesLeft(board)) {
            cout << "It's a Draw!" << endl;
            break;
        }
    }
    return 0;
}
