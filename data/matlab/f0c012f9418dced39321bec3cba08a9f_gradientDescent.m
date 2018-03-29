# Implementation of gradient descent algorithm, based on 
# Exercise 2: linear regression
# http://openclassroom.stanford.edu/MainFolder/DocumentPage.php?
# course=MachineLearning&doc=exercises/ex2/ex2.html

x = load('ex2x.dat');
y = load('ex2y.dat');

# open a new figure window and plot x and y
figure;
plot(x, y, 'o');
ylabel('Heights in meters');
xlabel('Age in years');

# store the number of training examples
m = length(y); 

# add a column of ones to x and initialize alpha
x = [ones(m, 1), x] ;
alpha = 0.07;

# gradient descent implementation
theta = zeros(2, 1);
temp1 = theta(1);
temp2 = theta(2);
while true

	prevTemp1 = temp1;
	prevTemp2 = temp2;
	temp1 = theta(1) - alpha * (1/m) * ((x * theta) - y)' * x(:, 1);
	temp2 = theta(2) - alpha * (1/m) * ((x * theta) - y)' * x(:, 2);
	theta(1) = temp1;
	theta(2) = temp2;

	# check for convergence
	# printf('counter: %i, temp1: %f, temp2: %f\n', counter, temp1, temp2);
	if abs(prevTemp1 - temp1) < 0.0001 && abs(prevTemp2 - temp2) < 0.0001 break;
	endif

endwhile
printf('After gradient descent, theta is \n');
disp(theta);

# plot the straight line fit
hold on;
plot(x(:, 2), x*theta, '-');
legend('Training data', 'Linear regression');

# make predictions about the heights of two boys age 3.5 and 7
printf('\nMake prediction about the height of a boy age 3.5\n');
predictedX = [1, 3.5]
predictedY = predictedX * theta

printf('\nMake prediction about the height of a boy age 3.5\n');
predictedX = [1, 7]
predictedY = predictedX * theta