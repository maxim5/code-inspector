%  data{1} = loadfile('t1.pcd');
%  data{2} = loadfile('t2.pcd');
%  
%  [data,pc,latent] =fixCoordSys(data);
i =1;
j= 5;
options = statset('Display','final','MaxIter', 250);
obj1 = gmdistribution.fit(data{i},32,'Options',options,'CovType','diagonal');
obj2 = gmdistribution.fit(data{j},32,'Options',options,'CovType','diagonal');

[d nlog1]= posterior(obj1,data{j});
[d nlog2]= posterior(obj2,data{j});

[nlog1 nlog2]


visual(data{i}); 
hold on

plot3(obj1.mu(:,1),obj1.mu(:,2),obj1.mu(:,3),'mx');
plot3(obj1.mu(:,1),obj1.mu(:,2),obj1.mu(:,3),'mo');

figure(2);
visual(data{j})
hold on

plot3(obj2.mu(:,1),obj2.mu(:,2),obj2.mu(:,3),'mx');
plot3(obj2.mu(:,1),obj2.mu(:,2),obj2.mu(:,3),'mo');

kld=KLD_gmm(obj1,obj2)