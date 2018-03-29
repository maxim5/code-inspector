'''Yppy algorithm facilities.
'''

# ....................{ IMPORTS                            }....................
from yppy import ui

# ....................{ CLASSES                            }....................
class YppyAlgorithmBase(object):
    '''Base class for all Yppy algorithms.
    
    Subclasses of this class are algorithms solving Yppy-related problems:
    typically, clustering graphs, aggregating graph clusterings, and so on.
    
    Instantiations of these classes are distinct algorithm runs. By design,
    subclasses of this class expose a Python iterator ui. When iterated,
    subclass objects perform the algorithm until completion and, for each such
    iteration, return a value of interest to the caller iterating that object.
    Designing algorithms as Python iterators provides a consistent use interface
    to CLI and GUI users. Which is nice.
    
    Instantiations are only valid until algorithm completion (i.e., until the
    iterator has been exhausted). After algorithm completion, the corresponding
    algorithm object should be discarded. If a new run is needed, a new
    algorithm object should be created. This assists your sanity, and ours.

    Technically speaking, Python permits iterators to have infinite length.
    Technically speaking, therefore, Yppy algorithms need never reach
    completion. But this is probably bad form. 
           
    Examples
    ----------
    # Shows all clusters identified by all algorithm iterations for the
    # "Zachary's Karate Club" graph in Graphviz DOT file format. The clustering
    # algorithm, here, is synchronous LPA.
    >>> graph = YppyAlgorithmFileGraphReader().read('zachary.dot')
    >>> iteration = 1
    >>> for clusters in YppyGraphClusterLPASync(graph):
    ...     print 'iteration %s identified clusters: %s' % (iteration, clusters)
    ...     iteration++
    
    # Sometimes, all we want is the set of clusters identified by the final
    # algorithm iteration: that is, the "definitive solution." Do this, then.
    #
    # This brute-force approach is currently both the cleanest and fastest way
    # to iterate the last item of a Python iterator.
    >>> graph = YppyAlgorithmFileGraphReader().read('zachary.dot')
    >>> for clusters in YppyGraphClusterLPASync(graph):
    ...     pass
    >>> print 'algorithm identified clusters: %s' % clusters

    Attributes
    ----------
    _progress_bar : ProgressBar
        Progress bar object to which this class emits progress updates.
    _iteration : int
        Current algorithm iteration as a 0-based integer. This is as follows:
        * 0, after an __iter__() call but before the first next() call.
        * 1, after the first next() call.
        * 2, after the second next() call.
        * ...
        * N, after the Nth and final next() call raises "StopIteration". 
    '''
    def __init__(self):
        '''Initialize this algorithm.
        '''
        # Set user interface attributes.
        self._progress_bar = UI.get_progress_bar()
    
    def __iter__(self):
        '''Get an iterator suitable for running this algorithm.
        
        For simplicity's sake, this is typically the current object.
        Subclasses should probably override this method, but do not have to.
        '''
        self._iteration = 0
        return self

    def next(self):
        '''Iterate the current algorithm run.
        
        Subclasses must override this method. On completion, this method should
        raise a "StopException"; in all other circumstances, this method should
        return a value of interest to the caller iterating this algorithm.
        '''
        self._iteration += 1

#FIXME: Obsolete.
#    _I : YppyInterfaceBase
#        User interface to which this class emits messages.

# --------------------( COPYRIGHT AND LICENSE              )--------------------
# The information below applies to everything in this distribution,
# except where noted.
#
# Copyright 2010-2011, Cecil Curry <leycec@gmail.com {*} http://raiazome.com>.
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# 1. Redistributions of source code must retain the above copyright notice,
#    this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in the
#    documentation and/or other materials provided with the distribution.
# 3. Neither the name of the <ORGANIZATION> nor the names of its
#    contributors may be used to endorse or promote products derived from
#    this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE 
#
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN 
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
