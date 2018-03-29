//Algorithm Implemented: Prim's algorithm
//Complexity: O(ElogV)

#include<bits/stdc++.h>
using namespace std;

const double eps = 1e9;


Prim()
{
	//initialising 
	for(int node_u =0; node_u < no_of_vertices; node_u++)
	{
		parent_array[node_u]=0;
		key[node_u]= eps;
	}

	key[start_node]=0;

	priority_queue<int,int> q;

	//insert all the vertices into the graph along with their key values.
	for (int i = 0; i < no_of_vertices; ++i)
	{
		q.push(key[i],i);
	}

	while(q!=empty())
	{
		node_u = q.top(); //takes the node with the minimum key value as the next key.
		q.pop();
		vis[node_u]=1; //indicating it is not in the queue.
		for(node_v=0; node_v < no_of_vertices; node_v++)
		{
			if(graph[node_u][node_v]!=0)// all those nodes that are adjacent to the current key node.
			{
				if(vis[node_v]==0 and graph[node_u][node_v]<key[node_v]) // if the node is still in graph and the weight difference between node_u and node_v is less than the key value of node_v.
				{
					key[node_v]= graph[node_u][node_v];
					parent[node_v]=node_u;
				}
			}
		}

	}

}

void main()
{
	//take graph as input
	Prim();
	return 0;
}
