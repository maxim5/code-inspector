// Implementation file Voter.cpp: This is the IMPLEMENTATION of the ADT Voter.
// The interface of the class Voter is in the header file "Voter.h".

#include "Voter.h"

using namespace std;

//Default constructor
Voter::Voter() {
	votes = "";
	ID = 0;
}

//Initializes the object so that the voter ID is set to n, and the string of
//votes is set to a
Voter::Voter(string a, int n) {
	votes = a;
	ID = n;
}

//Returns the voter ID
int Voter::getID() {
	return ID;
}

//Returns the vote for the mayor
char Voter::getVoteForMayor() {
	return votes[0];
}

//Returns the vote for proposition
char Voter::getProposition17() {
	return votes[1];
}

//Returns the vote for measure 1
char Voter::getMeasure1() {
	return votes[2];
}

//Returns the vote for measure 2
char Voter::getMeasure2() {
	return votes[3];
}
