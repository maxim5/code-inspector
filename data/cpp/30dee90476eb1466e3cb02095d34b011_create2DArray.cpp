/*
 * Implementation file create2DArray.cpp: This is the IMPLEMENTATION of the ADT
 * create2DArray. The interface is in the header file create2DArray.h
 */

#include "create2DArray.h"

using namespace std;

// Default constructor
create2DArray::create2DArray() {
	arr = NULL;
	row = 0;
	col = 0;
}

// Precondition: rows and columns are the desired number of rows and columns in
// the two-dimensional array
// Postcondition: initializes the pointer to a one-dimensional dynamic array with
// size rows * columns.
create2DArray::create2DArray(int rows, int columns) {
	arr = new int[rows * columns];
	row = rows;
	col = columns;
}

// Destructor
create2DArray::~create2DArray() {
	if (arr != NULL) {
		delete arr;
	}
}

// Precondition: desired_row and desired_column are the zero-based index of the row
// and column of the caller would like to access. val is the value to store at
// desired_row and desired_col.
// Postcondition: val has been stored at desited_row and desired_col of the 2D array.
void create2DArray::set(int desired_row, int desired_column, int val) {
	//check input validity
	if (desired_row < 0 || desired_row > row - 1 || desired_column < 0
			|| desired_column > col - 1) {
		cout << "Desired indices are invalid.\n";
		exit(1);
	}

	arr[desired_row * col + desired_row] = val;
}

// Precondition: desired_row and desired_column are the zero-based index of the row
// and column of the caller would like to access.
// Postcondition: return the value at desired_row and desired_column of the 2D array.
int create2DArray::get(int desired_row, int desired_column) {
	//check input validity
	if (desired_row < 0 || desired_row > row - 1 || desired_column < 0
			|| desired_column > col - 1) {
		cout << "Desired indices are invalid.\n";
		exit(1);
	}

	return arr[desired_row * col + desired_column];
}
