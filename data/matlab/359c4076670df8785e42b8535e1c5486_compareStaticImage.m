
[data, pathname] = uigetfile('*','Select the data to compare','MultiSelect','on');
if isequal(data,0)
   disp('User selected Cancel');
   return;
else
   disp(['User selected background ', pathname]);
end

for j = 1:(size(data,2)-1), 
    [d1,m1,x,y,z] = getDataFromFile(fullfile(pathname, data{j}));
    [d2,m2,x1,y1,z1] = getDataFromFile(fullfile(pathname, data{j+1}));
    
    dist = distEukledienne(m1,m2);
    
    v = var(dist(:));
    t = mean(dist(:));
    
    distXYZ = averegeDistEuklid(x,x1,y,y1,z,z1);
   
    disp(['The average of difference betwen row data are : ',num2str(t)]);
    
    disp(['The average of Euclidean distance is [m] : ',num2str(mean(distXYZ))]);
    
    disp(['The variance of difference betwen row data are : ',num2str(v)]);
    
    disp(['The variance of distance betwen the pictures is [m] : ',num2str(var(distXYZ))]);
    
    disp(['The standard deviation of difference betwen row data are : ',num2str(std(dist(:)))]);
    
    disp(['The standard deviation  of distance betwen the pictures is [m] : ',num2str(std(distXYZ))]);
    
    
    
   
   subplot(1,2,1)
   x = 0:20:2048;
   hist(d1(3,:),x);
   title('Row data');
 
    
   subplot(1,2,2);

   x = 0:0.1:8;
   hist(z,x);
   title('Data after ijd->xyz');

end

