#include <bits/stdc++.h>
using namespace std;

struct Node {
    vector<int> state;
    int blank, h;
    vector<vector<int>> path;
};

struct Compare {
    bool operator()(Node const &a, Node const &b){
        return a.h > b.h; // min-heap (best-first)
    }
};

vector<vector<int>> moves = {
    {1,3}, {0,2,4}, {1,5},
    {0,4,6}, {1,3,5,7}, {2,4,8},
    {3,7}, {4,6,8}, {5,7}
};

vector<int> goal = {1,2,3,4,5,6,7,8,0};

bool isGoal(vector<int> &s){ return s==goal; }

int heuristic(vector<int> &s){
    int h=0;
    for(int i=0;i<9;i++) if(s[i]!=0 && s[i]!=goal[i]) h++;
    return h;
}

void printState(vector<int> &s){
    for(int i=0;i<9;i++){
        if(i%3==0) cout << "\n";
        cout << s[i] << " ";
    }
    cout << "\n";
}

void bestFirst(vector<int> start){
    priority_queue<Node, vector<Node>, Compare> pq;
    set<vector<int>> visited;
    pq.push({start, (int)(find(start.begin(), start.end(), 0)-start.begin()), heuristic(start), {start}});
    visited.insert(start);

    while(!pq.empty()){
        Node cur = pq.top(); pq.pop();

        if(isGoal(cur.state)){
            cout << "Solution found with Best-First Search!\n";
            for(auto &step:cur.path){
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
                Node next{newState, mv, heuristic(newState), cur.path};
                next.path.push_back(newState);
                pq.push(next);
            }
        }
    }
    cout << "No solution!\n";
}

int main(){
    vector<int> start = {1,2,3,4,0,6,7,5,8};
    bestFirst(start);
}
