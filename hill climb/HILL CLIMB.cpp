#include <bits/stdc++.h>
using namespace std;

vector<int> goal = {1,2,3,4,5,6,7,8,0};

vector<vector<int>> moves = {
    {1,3}, {0,2,4}, {1,5},
    {0,4,6}, {1,3,5,7}, {2,4,8},
    {3,7}, {4,6,8}, {5,7}
};

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

void hillClimb(vector<int> start){
    int blank = find(start.begin(), start.end(), 0)-start.begin();
    vector<int> cur = start;
    int cur_h = heuristic(cur);

    cout << "Initial state (h=" << cur_h << "):\n";
    printState(cur);
    cout << "-----\n";

    while(true){
        vector<int> best = cur;
        int best_h = cur_h;

        for(int mv:moves[blank]){
            vector<int> neighbor = cur;
            swap(neighbor[blank], neighbor[mv]);
            int h = heuristic(neighbor);
            if(h < best_h){
                best = neighbor;
                best_h = h;
                blank = mv;
            }
        }

        if(best_h == cur_h){ // no better neighbor
            cout << "Local optimum reached (h=" << cur_h << ")\n";
            if(best == goal) cout << "Goal Reached!\n";
            else cout << "Stuck in local minima!\n";
            return;
        }

        cur = best;
        cur_h = best_h;
        cout << "Next state (h=" << cur_h << "):\n";
        printState(cur);
        cout << "-----\n";
        if(cur==goal) {
            cout << "Goal Reached!\n";
            return;
        }
    }
}

int main(){
    vector<int> start = {1,2,3,4,0,6,7,5,8};
    hillClimb(start);
}
