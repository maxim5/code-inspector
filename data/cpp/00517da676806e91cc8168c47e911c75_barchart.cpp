/*
 * Implementation file barchart.cpp: This is the IMPLEMENTATION of the
 * ADT barchart. The interface for the class barchart is in the header
 * file barchart.h
 */

#include <iostream>
#include <string>
#include <fstream>
#include <cstdlib>
#include "barchart.h"

using namespace std;

barchart::barchart(){
    for (int i = 0; i < 4; i++){
    	heights[i] = 0;
    }
}

void barchart::getvalue(){
	cout << "Please enter the first nonnegative integer value: ";
	cin >> heights[0];
	cout << "Please enter the second nonnegative integer value: ";
	cin >> heights[1];
	cout << "Please enter the third nonnegative integer value: ";
	cin >> heights[2];
	cout << "Please enter the fourth nonnegative integer value: ";
	cin >> heights[3];
}

void barchart::scale(){
	//store the maximum of the four integer values in max
	unsigned int max = heights[0];
	for (int i = 1; i < 4; i++){
		if (max < heights[i]){
			max = heights[i];
		}
	}

	//compute the scaled values of the four integers and store them in the array height
	double scale_index = 400.0 / max;
	for (int i = 0; i < 4; i++){
		heights[i] = static_cast<int>(heights[i] * scale_index);
	}
}

void barchart::output(){
	//create the output stream and open the file "barchart.svg"
	ofstream out_stream;
	out_stream.open("barchart.svg");

	if(out_stream.fail()){
		cout << "Output file opening failed.\n";
		exit(1);
	}

	out_stream << "<?xml version=\"1.0\" standalone=\"no\"?>" << endl
			<< "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"" << endl
			<< "\"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">" << endl
			<< "<svg width=\"500\" height=\"500\"" << endl
			<< "xmlns=\"http://www.w3.org/2000/svg\">" << endl;

	//draw the axis of the bar chart
	out_stream << "<line x1=\"0\" y1=\"0\" x2=\"0\" y2=\"399\" style=\"stroke:purple;stroke-width:2\"/>" << endl
			<< "<line x1=\"0\" y1=\"399\" x2=\"399\" y2=\"399\" style=\"stroke:purple;stroke-width:2\"/>" << endl;

	//draw bars to represent the four nonnegative integer values
	for (int i = 0; i < 4; i++){
		out_stream << "<rect x=\"" << 20 + 100 * i << "\" y=\"" << 400 - heights[i]
		             << "\" width=\"50\" height=\"" << heights[i] << "\" style=\"fill:blue;\"/>" << endl;
	}

	out_stream << "</svg>" << endl;
	//close the output stream
	out_stream.close();

}
