/*
 * Implementation file gameoflife.cpp: This is the IMPLEMENTATION of the ADT gameoflife.
 * The interface for the class gameoflife is in the header file gameoflife.h.
 */

#include <iostream>
#include <cstdlib>
#include "gameoflife.h"
using namespace std;

//constructor
gameoflife::gameoflife(){
	//set every cell to be dead
	for (int i = 0; i < ROW + 2; i++){
		for (int j = 0; j < COL + 2; j++){
			world[i][j] = ' ';
		}
	}
}

//Specify the initial configuration
void gameoflife::getposition(){

	cout << "This is the \"Game of Life\"." << endl
	        << "Please enter the positions (in row and column numbers) you would like to be alive. " <<
			endl << "When you finish entering all the positions, enter any negative number to terminate." << endl;

	int row, col;

	while(true){
		cout << "Please enter the row number (from 1 to 22): ";
		cin >> row;
		if (row < 0) break;
		cout << "Please enter the column number (from 1 to 80): ";
		cin >> col;
		if (col < 0) break;

		if (row < 1 || row > 22 || col < 1 || col > 80){
			cout << "Enter error.\n";
			exit(1);
		}

		world[row][col] = '*';
	}
	cout << "The initial configuration is: " << endl;
	display();

}

//scans the array world and modifies the cells,
//marking the cells with births and deaths in accord with the rules
void gameoflife::generation(){
	//make a copy of the world array
	char world2[ROW+2][COL+2];
	for(int i = 0; i < ROW + 2; i++){
		for(int j = 0; j < COL + 2; j++){
			world2[i][j] = world[i][j];
		}
	}

	//scans the array
	for(int i = 1; i < ROW - 1; i++){
		for(int j = 1; j < COL - 1; j++){
			//determine the number of neighbors
			unsigned int neighbors = 0;
			if (world[i-1][j-1]=='*') neighbors++;
			if (world[i-1][j]=='*') neighbors++;
			if (world[i-1][j+1]=='*') neighbors++;
			if (world[i][j-1]=='*') neighbors++;
			if (world[i][j+1]=='*') neighbors++;
			if (world[i+1][j-1]=='*') neighbors++;
			if (world[i+1][j]=='*') neighbors++;
			if (world[i+1][j+1]=='*') neighbors++;

			//Apply the rules to check births and deaths
			if (neighbors == 0 || neighbors == 1){
				world2[i][j] = ' ';
			}
			if (neighbors > 3){
				world2[i][j] = ' ';
			}
			if (neighbors == 3){
				world2[i][j] = '*';
			}
		}

	}

	//determine the next generation of the world array
	for(int i = 0; i < ROW + 2; i++){
		for(int j = 0; j < COL + 2; j++){
			world[i][j] = world2[i][j];
		}
	}
}

//displays the array world on the screen
void gameoflife::display(){
	for (int i = 1; i < ROW - 1; i++){
		for (int j = 1; j < COL - 1; j++){
			cout << world[i][j];
		}
		cout << endl;
	}
}


