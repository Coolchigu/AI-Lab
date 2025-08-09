#include <bits/stdc++.h>
using namespace std;

struct Node {
    vector<int> state;
    int blank; // position of 0
    vector<vector<int>> path; // for printing solution path
};

vector<vector<int>> moves = {
    {1,3},    // 0 -> right, down
    {0,2,4},  // 1 -> left, right, down
    {1,5},    // 2 -> left, down
    {0,4,6},  // 3 -> up, right, down
    {1,3,5,7},// 4 -> left, up, right, down
    {2,4,8},  // 5 -> up, left, down
    {3,7},    // 6 -> up, right
    {4,6,8},  // 7 -> left, up, right
    {5,7}     // 8 -> up, left
};

bool isGoal(vector<int> &state) {
    vector<int> goal = {1,2,3,4,5,6,7,8,0};
    return state == goal;
}

void printState(vector<int> &s) {
    for(int i=0;i<9;i++) {
        if(i%3==0) cout << "\n";
        cout << s[i] << " ";
    }
    cout << "\n";
}

void bfs(vector<int> start) {
    queue<Node> q;
    set<vector<int>> visited;
    Node root{start, (int)(find(start.begin(), start.end(), 0)-start.begin()), {start}};
    q.push(root);
    visited.insert(start);

    while(!q.empty()){
        Node cur = q.front(); q.pop();

        if(isGoal(cur.state)) {
            cout << "Solution found with BFS!\n";
            for(auto &step:cur.path) {
                printState(step);
                cout << "-----\n";
            }
            return;
        }

        for(int mv : moves[cur.blank]){
            vector<int> newState = cur.state;
            swap(newState[cur.blank], newState[mv]);
            if(!visited.count(newState)){
                visited.insert(newState);
                Node next{newState, mv, cur.path};
                next.path.push_back(newState);
                q.push(next);
            }
        }
    }
    cout << "No solution!\n";
}

int main(){
    vector<int> start = {1,2,3,4,0,6,7,5,8}; // initial state
    bfs(start);
}
