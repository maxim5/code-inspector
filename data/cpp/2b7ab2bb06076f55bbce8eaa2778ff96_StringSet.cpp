/*
 * Implementation file StringSet.cpp: This is the IMPLEMENTATION for the ADT StringSet.
 * The interface for the StringSet class is in the header file StringSet.h
 */

#include <iostream>
#include "StringSet.h"

using namespace std;

// Default constructor
StringSet::StringSet() {
	value.clear();
}

// Initializes the value set with the given vector of string
StringSet::StringSet(vector<string> a) {
	value.clear();
	for (unsigned int i = 0; i < a.size(); i++) {
		value.push_back(a[i]);
	}
}

// Destructor
StringSet::~StringSet() {
	value.clear();
}

// Add the given string to the set
void StringSet::add(string a) {
	value.push_back(a);
}

// Remove the last string from the set
void StringSet::remove() {
	value.pop_back();
}

// Clear the entire set
void StringSet::clear() {
	value.clear();
}

// Return the number of strings in the set
int StringSet::getsize() {
	return value.size();
}

// Output all strings in the set in the form of
// {string1, string2, string3,..., stringn}
void StringSet::output() {
	cout << "{";
	// store the number of strings in the set
	int size = getsize();
	// iterate through the set and output each string
	for (int i = 0; i < size - 1; i++) {
		cout << value[i] << ", ";
	}
	cout << value[size - 1];
	cout << "}" << endl;
}

// compute the similarity (measured by the binary cosine coefficient) between
// the current StringSet and an input parameter of type StringSet
double StringSet::checksimilarity(StringSet a) {
	// declare a copy of the current StringSet object
	StringSet c(value);

	// compute the binary cosine coefficient
	double sqr1 = sqrt(static_cast<double> (c.getsize()));
	double sqr2 = sqrt(static_cast<double> (a.getsize()));
	double sim = ((c * a).getsize()) / (sqr1 * sqr2);
	return sim;
}

// Overload the + operator which returns the union of two StringSet objects
StringSet operator +(StringSet a, StringSet b) {
	// declare c to store the union of a and b
	StringSet c;

	// include each string in a in the union of a and b
	for (int i = 0; i < a.getsize(); i++) {
		c.value.push_back(a.value[i]);
	}

	// iterate through b and adds new string to the union
	for (int i = 0; i < b.getsize(); i++) {
		// tracks whether the string in b is repeated in a
		bool rep = false;
		for (int j = 0; j < a.getsize(); j++) {
			if (b.value[i] == a.value[j])
				rep = true;
		}
		// add new string to the union
		if (rep == false) {
			c.value.push_back(b.value[i]);
		}
	}

	return c;
}

// Overload the * operator which returns the intersection of two StringSet objects
StringSet operator *(StringSet a, StringSet b) {
	// declare c to store the intersection of a and b
	StringSet c;

	// iterate through a and b to include the intersection in c
	for (int i = 0; i < b.getsize(); i++) {
		// tracks whether the string in b is repeated in a
		bool rep = false;
		for (int j = 0; j < a.getsize(); j++) {
			if (b.value[i] == a.value[j])
				rep = true;
		}
		// adds repeated string into c
		if (rep == true) {
			c.value.push_back(b.value[i]);
		}
	}

	return c;
}
