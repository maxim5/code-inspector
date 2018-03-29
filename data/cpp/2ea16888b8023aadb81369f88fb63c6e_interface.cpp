/*
 * interface.cpp
 * 
 * Copyright 2012 Mads KJeldsen <madswk@mads-tux>
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301, USA.
 * 
 * 
 */


#include "Interface.h"
#include <iostream>
#include <stdlib.h>
#include <string>
#include <stdio.h>

using std::string;
using std::cout;
using std::cin;
using std::endl;

Interface::Interface()
{
	
}

void Interface::title()
{
	system("clear");
	cout << "" <<endl; cout << "" <<endl;
	cout << "\t\t\033[1;31mB\033[0m ";cout << "A ";cout << "\033[1;31mT\033[0m ";cout << "T ";cout << "\033[1;31mL\033[0m ";
	cout << "E ";cout << "\033[1;31mS\033[0m ";cout << "H ";cout << "\033[1;31mI\033[0m ";cout << "P " << endl << endl;
}

string Interface::setPlayer()
{
	string player;
	cout << "Choose you name: ";
	cin >> player;
	return player;
}

void Interface::printSeachart(int x, int y, Seachart *player, Shoot *shots, bool showships, string playername)
{	
	char row[] = {'A','B','C','D','E','F','G','H','I','J'};
	int colum[] = {0,1,2,3,4,5,6,7,8,9};
	int b = 0;
	cout << "\t   ";
	for(int i = 0; i < 10; i++)
	{
		printf("\033[1;31m%c\033[0m ", row[i]);
	}
	cout << endl;
	for(int i=0; i<10; i++)
	{
		printf("\t\033[1;31m%d\033[0m\033[1;37m|\033[0m\033[104;37m.\033[0m", colum[i]);
		
		for(int j=0; j<10; j++)
		{
			if(shots->shots[j][i] == 1)
			{
				if(j < 10) printf("\033[104;31mXX\033[0m");
				if(j == 9) printf("\033[1;37m|\033[0m");
			}
			else if(player->getLocation(j,i) != NULL && showships)
			{
				cout << "\033[100;37m" << player->getLocation(j,i)->getShiptype() << "\033[0m"; if(j < 10) cout << "\033[100;37m \033[0m";
				if(j == 9) printf("\033[1;37m|\033[0m");
			}
			else if(shots->shots[j][i] == 2)
			{
				if(j < 10)printf("\033[104;37m*.\033[0m");
				if(j == 9) printf("\033[1;37m|\033[0m");
			}
			else if(shots->shots[j][i] == 0)
			{	
				if(j < 10) printf("\033[104;37m~.\033[0m");
				if(j == 9) printf("\033[1;37m|\033[0m");
			}
		}
		cout << endl;
	}
	printf("\t\033[1;32mPLAYER\033[0m "); cout << playername << endl;
	if(showships)
	{
		if(player->getLocation(x,y) != NULL && shots->shots[x][y] == 1) {
			printf("\t\033[1;31mOuch!, you have been hit\033[0m\n");
			if(player->getLocation(x,y)->shipDestroyed()) {
				printf("\t\033[1;35mThe enemy have destroyed your\033[0m "); cout << player->getLocation(x,y)->getShipname() << endl;;
			}
		}
		cout <<  "\tYou have " << player->getShips() << " ships on the seachart" << endl;
	
	}else {
		if(player->getLocation(x,y) != NULL && shots->shots[x][y] == 1) {
			printf("\t\033[1;31mYeah!, target hit\033[0m\n");
			if(player->getLocation(x,y)->shipDestroyed()) {
				printf("\t\033[1;35mYou have destroyed the enemy's'\033[0m "); cout << player->getLocation(x,y)->getShipname() << endl;
			}
		}
		cout << "\tThe enemy have " << player->getShips() << " ships on the seachart" << endl;
	}
	cout << endl << endl;
}

void Interface::gameOver(Seachart *player1, Seachart *player2)
{
	if(player1->getShips() > player2->getShips()) {
		cout << "CONGRATIULATIONS, YOU HAVE WON!";
	}else
	cout << "GAME OVER, YOU HAVE LOST!";
}

SHIPTYPE Interface::chkCommandType(char type)
{
	type = convertCharecter(type);
	switch(type)
	{
		case 'h': return HANGARSKIB;
		break;
		case 'u': return SUBMARINE;
		break;
		case 'f': return FRIGAT;
		break;
		case 's': return SLAGSKIB;
		break;
	}
}

ANGLE Interface::chkCommandAngle(char angle)
{
	angle = convertCharecter(angle);
	switch(angle)
	{
		case 'h': return HORIZONTAL;
		break;
		case 'v': return VERTICAL;
		break;
	}	
}

int Interface::chkCommandxCoordinate(char xPosition)
{
	xPosition = convertCharecter(xPosition);
	switch(xPosition)
	{
		case 'a': return 0;
		break;
		case 'b': return 1;
		break;
		case 'c': return 2;
		break;
		case 'd': return 3;
		break;
		case 'e': return 4;
		break;
		case 'f': return 5;
		break;
		case 'g': return 6;
		break;
		case 'h': return 7;
		break;
		case 'i': return 8;
		break;
		case 'j': return 9;
		break;
	}
}

void Interface::shipInputCoordinates(int *inputShipXY)
{
	char temp[4];
	cout << "Choose Ship [H,U,F,S]: ";
	cin >> temp[0];
	cout << "Choose X position [A-J]: ";
	cin >> temp[1];
	cout << "Choose Y position [0-10]: ";
	cin >> temp[2];
	cout << "Choose Angle [V / H]: ";
	cin >> temp[3];	
	
	inputShipXY[0] = chkCommandType(temp[0]);
	inputShipXY[1] = chkCommandxCoordinate(temp[1]);
	inputShipXY[2] = atoi(&temp[2]);
	inputShipXY[3] = chkCommandAngle(temp[3]);
	cout << endl << endl;
}

int* Interface::shootInputCoordinates(int *inputShootXY)
{
	char temp[2];
	cout << "SHOOT" << endl;
	cout << "[A-J]: ";
	cin >> temp[0];
	cout << "[0-9]: ";
	cin >> temp[1];
	
	inputShootXY[0] = chkCommandxCoordinate(temp[0]); 
	inputShootXY[1] = atoi(&temp[1]);
	
	return inputShootXY;
}

char Interface::convertCharecter(char input)
{
	if((int)input < 91) {
		input = input+32;
		return input;
	}
}
