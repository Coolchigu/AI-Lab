#include <bits/stdc++.h>
using namespace std;

struct Node {
    vector<int> state;
    int blank, g, h;
    vector<vector<int>> path;
};

struct Compare {
    bool operator()(Node const &a, Node const &b){
        return (a.g + a.h) > (b.g + b.h);
    }
};

vector<int> goal = {1,2,3,4,5,6,7,8,0};

vector<vector<int>> moves = {
    {1,3}, {0,2,4}, {1,5},
    {0,4,6}, {1,3,5,7}, {2,4,8},
    {3,7}, {4,6,8}, {5,7}
};

bool isGoal(vector<int> &s){ return s==goal; }

int manhattan(vector<int> &s){
    int dist=0;
    for(int i=0;i<9;i++){
        if(s[i]==0) continue;
        int targetX=(s[i]-1)/3, targetY=(s[i]-1)%3;
        int x=i/3, y=i%3;
        dist += abs(x-targetX)+abs(y-targetY);
    }
    return dist;
}

void printState(vector<int> &s){
    for(int i=0;i<9;i++){
        if(i%3==0) cout << "\n";
        cout << s[i] << " ";
    }
    cout << "\n";
}

void aStar(vector<int> start){
    priority_queue<Node, vector<Node>, Compare> pq;
    set<vector<int>> visited;
    pq.push({start, (int)(find(start.begin(), start.end(), 0)-start.begin()), 0, manhattan(start), {start}});
    visited.insert(start);

    while(!pq.empty()){
        Node cur=pq.top(); pq.pop();

        if(isGoal(cur.state)){
            cout << "Solution found with A*!\n";
            for(auto &step:cur.path){
                printState(step);
                cout << "-----\n";
            }
            return;
        }

        for(int mv:moves[cur.blank]){
            vector<int> newState=cur.state;
            swap(newState[cur.blank], newState[mv]);
            if(!visited.count(newState)){
                visited.insert(newState);
                Node next{newState, mv, cur.g+1, manhattan(newState), cur.path};
                next.path.push_back(newState);
                pq.push(next);
            }
        }
    }
    cout << "No solution!\n";
}

int main(){
    vector<int> start = {1,2,3,4,0,6,7,5,8};
    aStar(start);
}
