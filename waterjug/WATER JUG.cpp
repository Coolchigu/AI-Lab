#include <bits/stdc++.h>
using namespace std;

struct State {
    int jug4, jug3; // amounts in 4L and 3L jugs
    vector<pair<int,int>> path;
};

void bfs(int target){
    queue<State> q;
    set<pair<int,int>> visited;

    q.push({0,0,{{0,0}}});
    visited.insert({0,0});

    while(!q.empty()){
        State cur = q.front(); q.pop();

        if(cur.jug4 == target || cur.jug3 == target){
            cout << "Solution found!\n";
            for(auto &step:cur.path){
                cout << "(" << step.first << "," << step.second << ")\n";
            }
            return;
        }

        int a = cur.jug4, b = cur.jug3;
        vector<pair<int,int>> nextStates = {
            {4,b}, {a,3},   // Fill
            {0,b}, {a,0},   // Empty
            {max(0,a-(3-b)), min(3,b+a)}, // Pour 4->3
            {min(4,a+b), max(0,b-(4-a))}  // Pour 3->4
        };

        for(auto &ns:nextStates){
            if(!visited.count(ns)){
                visited.insert(ns);
                auto newPath = cur.path;
                newPath.push_back(ns);
                q.push({ns.first, ns.second, newPath});
            }
        }
    }
    cout << "No solution!\n";
}

int main(){
    int target = 2;
    bfs(target);
}
