/*	Implementation of the class for Mountain-Car task, based on the 
	formulation from Sutton & Borto book "Reinforcement Learning: An Introdunction". 
	File:		mountain_car.cpp
	Author:		Bohdana Ratitch
	Version:	February 2001
*/

#include <stdlib.h>
#include <string.h>
#include<stdio.h>
#include<math.h>
#include<sys/types.h>		
#include<iostream.h>

#ifndef INTERFACE_CLASSES	
	#define INTERFACE_CLASSES
	#include "interface_classes.h"
#endif


#ifndef ENV_MC	
	#define ENV_MC 1
	#include "mountain_car.h"
#endif


//	IMPLEMENTATION OF Environment class for mountain car task

MountainCar::MountainCar(){
			/*	Default constructor.
				Initializes state according to start state distribution
			*/
		bool t;
		startState(CurrentState, t);
		reward=-1;
	}

void MountainCar::uniformStateSample(State& s)
{
  s.x[0]=-1.2+(double)rand()/((double)RAND_MAX)*1.7;	//random number in [-1.2, 0.5] - car's position
  s.x[1]=-0.07+(double)rand()/((double)RAND_MAX)*0.14;//random number in [-0.07,0.07]- car's velocity
}

void MountainCar::startState(State& start, bool& terminal){
			/*	Selects start state
			*/
	CurrentState.x[0]=-1.2+(double)rand()/((double)RAND_MAX)*1.7;	//random number in [-1.2, 0.5] - car's position
	CurrentState.x[1]=-0.07+(double)rand()/((double)RAND_MAX)*0.14;//random number in [-0.07,0.07]- car's velocity
	start=CurrentState;
	if (CurrentState.x[0]>=0.5) terminal=true;
	else terminal=false;
	Stages=1;
}

void MountainCar::setState(const State& s, bool& terminal){
	CurrentState=s;
	if ((CurrentState.x[0]<-1.2)||(CurrentState.x[0]>0.5)){
		cout << "State set to invalid value" << endl;
		cout << s << endl;
		exit(EXIT_FAILURE);
	}

	if ((CurrentState.x[1]<-0.07)||(CurrentState.x[1]>0.07)){
		cout << "State set to invalid value" << endl;
		cout << s << endl;
		exit(EXIT_FAILURE);
	}

	if (CurrentState.x[0]>=0.5) terminal=true;
	else terminal=false;
	Stages=1;
}

void MountainCar::transition(const Action& action, State& s_new, double& r_new, bool& terminal){
			/*	Implements a transtion in responce to the action 
				performed by the agent. Updates its internal variables 
				and delivers values to the agent.
				Parameters:
					action: action performed by the agent
					s_new : return value - new state
					r_new : return value - new reward
					terminal: indication of whether s_new is a terminal state
			*/

		//new state and reward:

		State s_last=CurrentState;
		double temp;
		
		
		//check action applicability
		if (applicable(CurrentState, action)==false)
		  {	cout << "Error (env_mc): unapplicable action performed in state: " << CurrentState << endl;
			
			exit(EXIT_FAILURE);
		}
		//calculate new velocity
		temp=s_last.x[1]+0.001*action.value-0.0025*cos(3*s_last.x[0]);
		if (temp<-0.07) 
			CurrentState.x[1]=-0.07;
		else if (temp<=0.07) 
				CurrentState.x[1]=temp;
			else CurrentState.x[1]=0.07;
		
		//calculate new position
		reward=-1.0; terminal=false;	//until goal is reached
		
		temp=s_last.x[0]+CurrentState.x[1];
		if (temp<-1.2) 
			{CurrentState.x[0]=-1.2; CurrentState.x[1]=0.0;}
		else if (temp<0.5) 
				CurrentState.x[0]=temp;
			else {CurrentState.x[0]=0.5;terminal=true;reward=0.0;}
		

	CurrentAction=action;	//remember last action
	s_new=CurrentState;	//return new state
	r_new=reward;	//return new reward;
	Stages++;
	

}


bool MountainCar::applicable(const State& s, const Action& a)
		/*	Checks if action a is applicable in state s.
		*/
	{if ((a.value==-1.0) || (a.value==0.0) || (a.value == 1.0) )
		return true;
	else return false;
}

void MountainCar::bound(int i, bool& bounded, double& left, double& right)
		/*	Gives bounds on state variables' values
			This implementation is for the case where all variables are unbounded.
			This method should be redefined by derived classes in case of bounded variables
			Parameters:
				i : index of state variable
				bounded: indicates if i^th variable is bounded
				left : left bound
				right: right bound
			*/
{	
	if ((i<0) || (i>= State::dimensionality)) 
	{	cout << "Error (env_mc:bound()): variable index out of limit" << endl;
		exit(EXIT_FAILURE);
	}
	
	bounded=true;
	if (i==0) {	
		left = -1.2;
		right= 0.5;
		return;
	}
	if (i==1){
		left = -0.07;
		right = 0.07;
	}
		
}

MountainCar::~MountainCar(){}



