/*
 * algorithm.cpp
 *
 *  Created on: Sep 20, 2010
 *      Author: Cuong Dong (cuongdon@usc.edu)
 */
#include <iostream>
#include <set>
#include "algorithm.h"

namespace csci561{
namespace homework1{
namespace search{
	ResultType Solve( Node & starting_node,  GoalTestFunction & goal_test,  EvaluationFunction & f, ProblemMap & state_space){
		using namespace std;

		//Initial setup
		vector<Node> explored_set;
		set<string> explored_state_set;
		list<Node> frontier_set;//cannot use the priority queue in STL because you need to remove random node in loop checking
		set<string> frontier_state_set;

		//Insert the starting node into the frontier queue
		frontier_set.push_back(starting_node);
		frontier_state_set.insert(starting_node.get_state());

		ResultType result;
		NodeComparison comparator(f);

		//Statistics
		int count = 0;
		int max_frontier_size = 0;

		//While there is node in frontier queue, keep selecting and expanding
		while(frontier_set.size() > 0){
			frontier_set.sort(comparator); // maintains the priority queue
			const Node current = frontier_set.front(); // get the node at the front

			//Update statistics
			count++;
			if (frontier_set.size() > max_frontier_size) max_frontier_size = frontier_set.size();

			if (goal_test(current)){
				//Build and return the solution from the goal node by traversing up the parents
				result.push(current);
				int parent_id = current.get_parent_id();

				while (parent_id != kNoParent){ //keep going until getting to the starting node
					NodePredicate node_predicate(parent_id);
					Node parent = *(find_if(explored_set.begin(), explored_set.end(), node_predicate));
					result.push(parent);
					parent_id = parent.get_parent_id();
				}

				cout << "Number of steps : " << count << endl;
				cout << "Max frontier set size: " << max_frontier_size << endl;
				cout << "Explored set size: " << explored_set.size() << endl;
				cout << "Frontier set size: " << frontier_set.size() << endl;
				return result;
			}

			//If not goal node, move from frontier set to explored set
			string current_state = current.get_state();
			frontier_set.pop_front();
			frontier_state_set.erase(current_state);
			explored_set.push_back(current);
			explored_state_set.insert(current_state);

			//Getting all possible operations. The result is a map from destination state -> the step cost to that state.
			DistanceMap operations = state_space[current_state];
			//For each operation, generate a new node and add to frontier set (with loop checking)
			for (DistanceMap::iterator it = operations.begin(); it != operations.end(); ++it){
				string next_state = it->first;
				int step_cost = it->second;

				if (!(step_cost > 0)) continue; //need this check for uniform cost search (it is complete if step cost > 0)

				//Create new node
				Node new_node(next_state, current.get_id(), current.get_path_cost() + step_cost, current.get_depth() + 1);

				//Loop checking algorithm from lecture slides
				if (frontier_state_set.count(next_state) > 0){
					//compare and replace if path cost of frontier node is > new node's
					for (list<Node>::iterator i=frontier_set.begin(); i != frontier_set.end(); i++){
						if (i->get_state() == next_state){
							if (i->get_path_cost() > new_node.get_path_cost()){
								frontier_set.erase(i);//erase the node in frontier set
								frontier_set.push_back(new_node);
								frontier_state_set.insert(new_node.get_state());
							}
							break;
						}
					}
				}
				else if (explored_state_set.count(next_state) > 0){
					//compare and replace if path cost of explored node is > new node's
					for (vector<Node>::iterator i=explored_set.begin(); i != explored_set.end(); i++){
						if (i->get_state() == next_state){
							if (i->get_path_cost() > new_node.get_path_cost()){
								explored_set.erase(i);//erase the node in explored set
								frontier_set.push_back(new_node);
								frontier_state_set.insert(new_node.get_state());
							}
							break;
						}
					}
				}
				else {
					frontier_set.push_back(new_node);
					frontier_state_set.insert(new_node.get_state());
				}
			}
		}

		return result;
	}
}
}
}
