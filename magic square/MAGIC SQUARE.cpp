#include <bits/stdc++.h>
using namespace std;

// A function to generate odd sized magic squares
vector<vector<int>> generateSquare(int n) {

    // initialize magic square
    vector<vector<int>> mat(n, vector<int>(n, 0)); 

    // Initialize position for 1
    int i = n / 2;
    int j = n - 1;

    // One by one put all values in magic square
    for (int num = 1; num <= n * n;) {

        // if row is -1 and column becomes n, 
        // set row = 0, col = n -2
        if (i == -1 && j == n)  {
            j = n - 2;
            i = 0;
        }
        else {

            // If next number goes to out of 
            // square's right side
            if (j == n)
                j = 0;

            // If next number goes to out of
            // square's upper side
            if (i < 0)
                i = n - 1;
        }

        // If number is already present decrement 
        // column by 2, and increment row by 1
        if (mat[i][j]) {
            j -= 2;
            i++;
            continue;
        }
        else {

            // set number
            mat[i][j] = num++; 
        }

        // increment and decrement
        // column and row by 1 respectively
        j++;
        i--; 
    }

    return mat;
}

int main() {
    int n = 5;
    vector<vector<int>> magicSquare = generateSquare(n);
    for(int i = 0; i < n; i++) {
        for(int j = 0; j < n; j++) {
            cout << magicSquare[i][j] << " ";
        }
        cout << endl;
    }
    return 0;
}
