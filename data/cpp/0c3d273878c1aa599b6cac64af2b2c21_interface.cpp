// Interface position finder, uses DAT_x_aread.dat as input.
// JJ Williamson, Oct 2011.
// Takes an averaging integer (number of datapoints to combine together) and a threshold
// concentration.
// Interface position output is the start of the averaging period in which the concentration falls
// below threshold.

// Modified to accept commandline arguments: input file, number of points to average, threshold concentration

#include <stdio.h>
#include <stdlib.h>
#include <iostream>
#include <string>
#include <fstream>
//#include <math.h>


using namespace std;

int main (int argc, char **argv) {
	FILE *fout, *fin;
    string ifilename;
	string ofilename;
	
    // The arrays defining the 'averaging window' in question:
    // This contains the area density readings which will be averaged:
	double *averaging;
    // This contains the x-coordinates of the area density readings:
	double *xposns;
	
	double phi_threshold, t, tcurrent, x, aread, navg_times_phi;	
	int navg, n, i, interface_found;	
	double total;
	
	double maxval;
	double val;
	double dummy1, dummy2;
	
	if (argc != 4) {
		printf("\nPlease enter correct cmd line parameters: input file, no of averaging points, threshold concentration.");
		return -1;
	}
	
	if (fin = fopen(argv[1], "r")) {
		cout << "\nFile opened.";
	} else {
		cout << "\nCouldn't open file. ";
		return -1;
	}
	
	
	ofilename = "DAT_x_interface_posn.dat";
	fout = fopen(ofilename.c_str(), "w");
		
	navg = atoi(argv[2]);		
	
	phi_threshold = atof(argv[3]);
	
	// Save this variable for efficiency:
	navg_times_phi = phi_threshold * navg;
	
	// Make an array to store the area densities.
	averaging = (double*)malloc(navg*sizeof(double));
	xposns = (double*)malloc(navg*sizeof(double));
	
	tcurrent = -1;
	interface_found = 0;
	
	// Scan through input files area density points:
    while (fscanf(fin, "\n%lf", &t) == 1) {
        fscanf(fin, "\t%lf\t%lf", &x, &aread);
        // If we've moved into the next timestep:
        if (t > tcurrent) {
            printf("\nTime = %lf", t);
            tcurrent = t;
            n=0;
            interface_found = 0;
        }
        // Just keep going through the file if we've found the interface already
        // in order to get through to the next timestep:
        if (interface_found)
            continue;
        
        // Populate the arrays of xposns and their associated area densities.
        // If the arrays are unfilled (i.e. n < navg) just plonk the values right in.
        // Else, shift the values along (oldest one falls off the end).
        if (n < navg) {
            averaging[n] = aread;
            xposns[n] = x;
        } else {
            for (i = 0; i < (navg-1); i++) {
                averaging[i] = averaging[i+1];
                xposns[i] = xposns[i+1];
            }
            averaging[navg-1] = aread;
            xposns[navg-1] = x;
        }
        // Increment the counter here:
        n++;
		
        // If the array is now full, calculate the average area density for this avging window:
        // NB -- this is inefficient, but easier to understand, leave it like this.
        if (n >= navg) {
            total = 0;
            // Find average area density:
            for (i = 0; i < navg; i++) {
                total += averaging[i];
            }
            // If the averaged concentration in the window falls below the threshold, mark the midpoint of it as the xposition of the interface.
            if (total < navg_times_phi) {
                interface_found = 1;
                printf("\nFound Interface..");
                fprintf(fout, "\n%lf\t%lf", t, 0.5*(xposns[navg-1] + xposns[0]));
            }
        }
    }
	printf("\nDone.\n");
	return 0;
}