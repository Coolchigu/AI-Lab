#include <bits/stdc++.h>
using namespace std;

char board[3][3] = {{' ',' ',' '},{' ',' ',' '},{' ',' ',' '}};

void printBoard(){
    cout << "\n";
    for(int i=0;i<3;i++){
        for(int j=0;j<3;j++){
            cout << board[i][j];
            if(j<2) cout << " | ";
        }
        cout << "\n";
        if(i<2) cout << "--+---+--\n";
    }
    cout << "\n";
}

bool movesLeft(){
    for(int i=0;i<3;i++)
        for(int j=0;j<3;j++)
            if(board[i][j]==' ') return true;
    return false;
}

int evaluate(){
    // Rows
    for(int i=0;i<3;i++){
        if(board[i][0]==board[i][1] && board[i][1]==board[i][2]){
            if(board[i][0]=='X') return +10;
            else if(board[i][0]=='O') return -10;
        }
    }
    // Cols
    for(int j=0;j<3;j++){
        if(board[0][j]==board[1][j] && board[1][j]==board[2][j]){
            if(board[0][j]=='X') return +10;
            else if(board[0][j]=='O') return -10;
        }
    }
    // Diagonals
    if(board[0][0]==board[1][1] && board[1][1]==board[2][2]){
        if(board[0][0]=='X') return +10;
        else if(board[0][0]=='O') return -10;
    }
    if(board[0][2]==board[1][1] && board[1][1]==board[2][0]){
        if(board[0][2]=='X') return +10;
        else if(board[0][2]=='O') return -10;
    }
    return 0;
}

int minimax(int depth, bool isMax){
    int score = evaluate();
    if(score==10 || score==-10) return score;
    if(!movesLeft()) return 0;

    if(isMax){
        int best=-1000;
        for(int i=0;i<3;i++){
            for(int j=0;j<3;j++){
                if(board[i][j]==' '){
                    board[i][j]='X';
                    best=max(best,minimax(depth+1,false));
                    board[i][j]=' ';
                }
            }
        }
        return best;
    } else {
        int best=1000;
        for(int i=0;i<3;i++){
            for(int j=0;j<3;j++){
                if(board[i][j]==' '){
                    board[i][j]='O';
                    best=min(best,minimax(depth+1,true));
                    board[i][j]=' ';
                }
            }
        }
        return best;
    }
}

pair<int,int> findBestMove(){
    int bestVal=-1000;
    pair<int,int> bestMove={-1,-1};

    for(int i=0;i<3;i++){
        for(int j=0;j<3;j++){
            if(board[i][j]==' '){
                board[i][j]='X';
                int moveVal=minimax(0,false);
                board[i][j]=' ';
                if(moveVal>bestVal){
                    bestMove={i,j};
                    bestVal=moveVal;
                }
            }
        }
    }
    return bestMove;
}

int main(){
    cout << "Tic Tac Toe (You: O, AI: X)\n";
    printBoard();

    while(true){
        int row,col;
        cout << "Enter your move (row col 0-2): ";
        cin >> row >> col;
        if(board[row][col]!=' '){
            cout << "Invalid move! Try again.\n";
            continue;
        }
        board[row][col]='O';
        printBoard();

        if(evaluate()==-10){ cout<<"You win!\n"; break; }
        if(!movesLeft()){ cout<<"Draw!\n"; break; }

        auto aiMove=findBestMove();
        board[aiMove.first][aiMove.second]='X';
        cout << "AI played:\n";
        printBoard();

        if(evaluate()==10){ cout<<"AI wins!\n"; break; }
        if(!movesLeft()){ cout<<"Draw!\n"; break; }
    }
}
