//Algorithm Implemented: Kruskal's algorithm
//Complexity: O(ElogE)

#include<bits/stdc++.h>
using namespace std;



int n,cost,v,p;
vector<pair<int,pair<int,int> > >graph;
string name;
long long int total=0;
int parent[10005];
void reset()
{
	graph.clear();
	for(int i=1;i<=n;i++)
	{
		parent[i]=i;
	}
	total=0;
}
int find(int x,int *parent)
{
	if(x!=parent[x])
		parent[x]=find(parent[x],parent);
	return parent[x];
}
void kruskal()
{
	sort(graph.begin(),graph.end());
	for(int i=0;i<graph.size();i++)
	{
		int pu=find(graph[i].second.first,parent);
		int pv=find(graph[i].second.second,parent);
		if(pu!=pv)
		{
			parent[pu]=parent[pv];
			total+=graph[i].first;
		}
	}
	printf("%lld\n",total );
}
int main()
{
	//taking graph as input
	int t;
	scanf("%d",&t);
	while(t--)
	{
		scanf("%d",&n);
		reset();
		for(int i=1;i<=n;i++)
		{
			cin>>name;
			scanf("%d",&p);
			for(int j=1;j<=p;j++)
			{
				scanf("%d %d",&v,&cost);
				graph.push_back(make_pair(cost,make_pair(i,v)));
			}
		}
		kruskal();
	}
	return 0;
}
