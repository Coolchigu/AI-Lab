#include <bits/stdc++.h>
using namespace std;

struct Node {
    vector<int> state;
    int blank;
    vector<vector<int>> path;
};

vector<vector<int>> moves = {
    {1,3}, {0,2,4}, {1,5},
    {0,4,6}, {1,3,5,7}, {2,4,8},
    {3,7}, {4,6,8}, {5,7}
};

bool isGoal(vector<int> &state) {
    return state == vector<int>{1,2,3,4,5,6,7,8,0};
}

void printState(vector<int> &s) {
    for(int i=0;i<9;i++) {
        if(i%3==0) cout << "\n";
        cout << s[i] << " ";
    }
    cout << "\n";
}

void dfs(vector<int> start, int depthLimit=20) {
    stack<Node> st;
    set<vector<int>> visited;
    st.push({start, (int)(find(start.begin(), start.end(), 0)-start.begin()), {start}});
    visited.insert(start);

    while(!st.empty()){
        Node cur = st.top(); st.pop();

        if(isGoal(cur.state)) {
            cout << "Solution found with DFS!\n";
            for(auto &step:cur.path){
                printState(step);
                cout << "-----\n";
            }
            return;
        }

        if((int)cur.path.size() > depthLimit) continue; // limit depth

        for(int mv : moves[cur.blank]){
            vector<int> newState = cur.state;
            swap(newState[cur.blank], newState[mv]);
            if(!visited.count(newState)){
                visited.insert(newState);
                Node next{newState, mv, cur.path};
                next.path.push_back(newState);
                st.push(next);
            }
        }
    }
    cout << "No solution (within depth limit)!\n";
}

int main(){
    vector<int> start = {1,2,3,4,0,6,7,5,8};
    dfs(start);
}
