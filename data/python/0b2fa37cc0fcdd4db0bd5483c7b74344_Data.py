"""
Data.py

This module contains definitions for data and nothing else. These will be used for unit testing and comparing against other statistics programs.

Integer vectors
Integer matrices
Integer missing vector
Integer missing arrays
Float vectors I
Float vectors II
Float matrices
Float missing vector
Float missing arrays

"""

import numpy
import numpy.ma

intvector = numpy.array(([0,19,2,3,16,19,12,2]), numpy.int)
intmatrix = numpy.array(([[0,19,2,3,16,19,12,2],
                        [19,1,5,4,4,12,18,16],
                        [0,4,14,13,19,17,6,12]]), numpy.int)
intmisvec = numpy.ma.array(([0,19,2,3,16,19,12,2]),mask=[0,0,0,1,0,0,0,1.0])
intmismat = numpy.ma.array(([[0,19,2,3,16,19,12,2],
                        [19,1,5,4,4,12,18,16],
                        [0,4,14,13,19,17,6,12]]), 
                mask=  [0,0,0,1,0,0,0,1,0,1,0,0,0,0,0,0,0,0,1,0,0,1,0,0])

fltvector = numpy.array(([0.460, 18.509, 1.627, 2.795, 16.258, 19.021, 11.699, 1.912]),numpy.float)
fltmatrix = numpy.array(([[0.46, 18.50, 1.62, 2.79, 16.25, 19.02, 11.69, 1.91],
                        [18.96, 0.66, 4.82, 4.12, 13.97, 12.00, 17.74, 16.06],
                        [0.37, 4.32, 13.74, 13.49, 19.28, 16.88, 6.29, 12.49]]), numpy.float)
fltmisvec = numpy.ma.array(([0.460, 18.509, 1.627, 2.795, 16.258, 19.021, 11.699, 1.912]),mask=[0,0,0,1,0,0,0,1.0])
fltmismat = numpy.ma.array(([[0.46, 18.50, 1.62, 2.79, 16.25, 19.02, 11.69, 1.91],
                        [18.96, 0.66, 4.82, 4.12, 13.97, 12.00, 17.74, 16.06],
                        [0.37, 4.32, 13.74, 13.49, 19.28, 16.88, 6.29, 12.49]]),
                mask=  [0,0,0,1,0,0,0,1,0,1,0,0,0,0,0,0,0,0,1,0,0,1,0,0])
longvec = numpy.array(([5.233, 8.012, 7.966, 2.608, 4.67, 8.924, 3.897, 5.537, 4.351, 9.147, 2.889, 3.418, 3.393, 8.202, 1.046, 2.684, 1.504, 6.814, 3.346, 6.311]),numpy.float)
uniques = numpy.array(([1,2,3,4,3,2,1,2,3,4,3,2,3,2,2,2]),numpy.int)
uniqmis = numpy.ma.array(([1,2,3,4,3,2,1,2,3,4,3,2,3,2,2,2]), numpy.int, mask=[0,0,0,1,1,0,0,0,0,0,0,0,1,0,0,0])
