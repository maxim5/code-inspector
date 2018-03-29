/*algorithm used: stripped down version of Ford Fulkerson, using dfs to find augmenting path.
to find the maximum bipartite matching, 
or the maximum number of pigeons that can find holes (one per pigeon), 
or maximum number of independent edges in the graph*/



#include<bits/stdc++.h>
#define MAX 500 // the maximum number of vertices in the bipartite graph.
using namespace std;


int graph[MAX][MAX];// contains the graph.
// if there is an edge from pigeon i to hole j then graph[i][j] is true;
int vis[MAX]; // visited array to keep track of the visited nodes in a path.
int pigeon_number,hole_number; // the number of nodes in either sets of the bipartite graph.
int matchL[MAX]; //matchL[i] contains the hole in which the pigeon i lies;
int matchR[MAX]; //matchR[i] contains the pigeon present in the ith hole;


//function returns true if there is a matching.
bool bpm(int node_u)
{
	for(int node_v=0 ; node_v < pigeon_number ; node_v++)
	{
		if(graph[node_u][node_v] and !vis[node_v])
		{
			vis[node_v]=1;
			if(matchR[node_v]<0 or bpm(matchR[node_v]))
			{
				matchL[node_u]=node_v;
				matchR[node_v]=node_u;
				return true;
			}
		}
	}
	return false;
}


int main()
{
	memset(matchL,-1,sizeof(matchL));// contains the holes for each pigeon or -1.
	memset(matchR,-1,sizeof(matchR));// contains the pigeon for each hole or  -1.
	int answer=0; // contains the maximum bipartite matching.

	for(int node_u=0; node_u < hole_number; node_u++)
	{
		memset(vis,0,sizeof(vis));
		if(bpm(node_u))
			answer++;
	}
	
	cout<<answer<<"\n";//contains the happy pigeons with holes.

	return 0;
}
