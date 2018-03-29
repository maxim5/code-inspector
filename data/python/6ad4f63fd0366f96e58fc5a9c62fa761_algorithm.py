#!/usr/bin/env python
# coding:utf-8
"""
    Algorithm
    ~~~~~~~~~

    A controller for algorithms
"""
import StringIO
import copy
import logging
import random
import re
import types

import tkMessageBox

from gato.anim.history import AnimationHistory
from gato.common import gInteractive, gProperty
from gato.util import stripPath, MethodLogger
from gato.graphs.util import WeightedGraphInformer, OpenCATBoxGraph
from gato.algo_debugger import AlgorithmDebugger


log = logging


class AbortProlog(Exception):
    """Phony exception to about execution of prolog."""
    def __init__(self, message):
        """ TODO: Add docstring """
        Exception.__init__(self)
        self.message = message
    def __str__(self):
        """ TODO: Add docstring """
        return repr(self.message)


class Algorithm:
    """ Provides all services necessary to load an algorithm, run it
        and provide facilities for visualization """
    
    def __init__(self):
        """ TODO: Add docstring """
        self.DB = AlgorithmDebugger(self)
        self.source = ""            # Source as a big string
        self.interactive = []  
        self.breakpoints = []       # Doesnt debugger take care of it ?
        self.algoFileName = ""
        self.prologFileName = ""
        self.graphFileName = ""
        self.mode = 0
        # mode = 0  Stop
        # mode = 1  Running
        # mode = 2  Stepping
        self.graph = None           # graph for the algorithm
        self.cleanGraphCopy = None  # this is the backup of the graph
        self.graphIsDirty = 0       # If graph was changed by running
        self.algoGlobals = {}       # Sandbox for Algorithm
        self.logAnimator = 1
        self.about = None
        
        self.commentPattern = re.compile('[ \t]*#')
        self.blankLinePattern = re.compile('[ \t]*\n')
        
        
    def SetGUI(self, itsGUI):
        """ Set the connection to its GUI """
        self.GUI = itsGUI
        
        
    def Open(self, filename):
        """ Read in an algorithm from file. """
        self.ClearBreakpoints()
        self.algoFileName = filename
        input_file = open(filename, 'r')
        self.source = input_file.read()
        input_file.close()

        # Create prolog file name by removing .alg.py and adding .pro.py to file name
        self.prologFileName = self.algoFileName.strip()[0:-len(".alg.py")] + ".pro.py"
        
        # Now read in the prolog as a module to get access to the following data
        # Maybe should obfuscate the names ala xxx_<bla>, have one dict ?
        try:
            input_file = open(self.prologFileName, 'r')
            options = self.ReadPrologOptions(input_file)
            input_file.close()
        except (EOFError, IOError),(errno, strerror):
            self.GUI.HandleFileIOError("prolog", self.prologFileName, errno, strerror)
            return
            
        try:
            self.breakpoints   = options['breakpoints']
        except:
            self.breakpoints   = []
        try:
            self.interactive   = options['interactive']
        except:
            self.interactive   = []
        try:
            self.graphDisplays = options['graphDisplays']
        except:
            self.graphDisplays = None
        try:
            self.about         = options['about']
        except:
            self.about         = None
            
            
        if self.graphDisplays != None:
            if self.graphDisplays == 1 and hasattr(self,"GUI"):
                self.GUI.WithdrawSecondaryGraphDisplay()
                
                
    def ReadPrologOptions(self, file_handle):
        """ Prolog files should contain the following variables:
        
            - breakpoints = [] a list of line numbers which are choosen as default
              breakpoints
            - interactive = [] a list of line numbers which contain interactive commands
              (e.g., PickVertex)
            - graphDisplays = 1 | 2 the number of graphDisplays needed by the algorithm
            - about = \"\"\"<HTML-code>\"\"\" information about the algorithm
        
            Parameter: filelike object
        """
        
        text = file_handle.read()
        options = {}
        optionPattern = {'breakpoints':'breakpoints[ \t]*=[ \t]*(\[[^\]]+\])',
                         'interactive':'interactive[ \t]*=[ \t]*(\[[^\]]+\])',
                         'graphDisplays':'graphDisplays[ \t]*=[ \t]*([1-2])'}
        # about is more complicated
        
        for patternName in optionPattern.keys():
            compPattern = re.compile(optionPattern[patternName])
            match = compPattern.search(text) 
            
            if match != None:
                options[patternName] = eval(match.group(1))	
                
                # Special case with about (XXX: assuming about = """ ... """)
                
        try:
            aboutStartPat = re.compile('about[ \t]*=[ \t]*"""')
            aboutEndPat   = re.compile('"""')
            left = aboutStartPat.search(text).end() 
            right = aboutEndPat.search(text, left).start()
            
            options['about'] = text[left:right]
        except:
            pass
            
        return options
        
        
    def About(self):
        """ Return a HTML-page giving information about the algorithm """
        if self.about != None:
            return self.about
        else:
            return "<HTML><BODY> <H3>No information available</H3></BODY></HTML>"
            
    def OpenGraph(self, file_obj, fileName=None):
        """ Read in a graph from file and open the display """
        if type(file_obj) in types.StringTypes:
            self.graphFileName = file_obj
        elif type(file_obj)==types.FileType or issubclass(file_obj.__class__,StringIO.StringIO):
            self.graphFileName = fileName
        else:
            raise Exception("wrong types in argument list: expected string or file like object")
        self.cleanGraphCopy = OpenCATBoxGraph(file_obj)
        self.restoreGraph()
        self.GUI.graphDisplay.Show() # In case we are hidden
        self.GUI.graphDisplay.ShowGraph(self.graph, stripPath(self.graphFileName))
        self.GUI.graphDisplay.RegisterGraphInformer(WeightedGraphInformer(self.graph))
        self.GUI.graphDisplay.UpdateScrollRegion(auto=1)
        
    def restoreGraph(self):
        """ TODO: Add docstring """
        self.graph = copy.deepcopy(self.cleanGraphCopy)
        self.graphIsDirty = 0
        
    def OpenSecondaryGraph(self, G, title, informer=None):
        """ Read in graph from file and open the the second display """
        self.GUI.OpenSecondaryGraphDisplay()
        self.GUI.secondaryGraphDisplay.ShowGraph(G, title)
        self.GUI.secondaryGraphDisplay.UpdateScrollRegion(auto=1)
        if informer is not None:
            self.GUI.secondaryGraphDisplay.RegisterGraphInformer(informer)
            
            
    def ReadyToStart(self):
        """ Return 1 if we are ready to run. That is when we user
            has opened both an algorithm and a graph.  """
        if self.graphFileName != "" and self.algoFileName != "":
            return 1
        else:
            return 0
            
    def Start(self):
        """ Start an loaded algorithm. It firsts execs the prolog and
            then starts the algorithm in the debugger. The algorithms
            globals (i.e., the top-level locals are in a dict we supply
            and for which we preload the packages we want to make available)"""
        if self.graphIsDirty == 1:
            self.restoreGraph()
            # Does show 
            self.GUI.graphDisplay.Show() # In case we are hidden
            self.GUI.graphDisplay.ShowGraph(self.graph, stripPath(self.graphFileName))
            self.GUI.graphDisplay.RegisterGraphInformer(WeightedGraphInformer(self.graph))
        else:
            self.GUI.graphDisplay.Show() # In case we are hidden
        self.graphIsDirty = 1
        self.mode = 1
        
        # Set global vars ...
        self.algoGlobals = {}
        self.algoGlobals['algo'] = self
        self.algoGlobals['graph'] = self.graph
        
        self.animation_history = None
        
        if self.logAnimator == 1:
            self.animation_history = AnimationHistory(self.GUI.graphDisplay)
            self.algoGlobals['disp'] = self.animation_history
        elif self.logAnimator == 2:
            self.algoGlobals['disp'] = MethodLogger(self.GUI.graphDisplay)
        else:
            self.algoGlobals['disp'] = self.GUI.graphDisplay
            
        ## explictly loading packages we want to make available to the algorithm
        #modules = ['data_structs', 
                   #'gato.data_structs', 
                   #'gato.anim.algorithms',
                   #'gato.graphs.util',
                   #'gato.util']
        
        #for m in modules:
            #self.algoGlobals[m] = __import__(m)
            
        # transfer required globals
        self.algoGlobals['gInteractive'] = gInteractive
        
        # Read in prolog and execute it
        try:
            execfile(self.prologFileName, self.algoGlobals, self.algoGlobals)
        except AbortProlog:
            # Only get here because NeededProperties was canceled by user
            self.GUI.CmdStop()
        except (EOFError, IOError), (errno, strerror):
            self.GUI.HandleFileIOError("prolog",
                                       self.prologFileName,
                                       errno,strerror)
        except:
            log.exception("Bug in %s" % self.prologFileName)
            #traceback.print_exc()
        
        # Filename must be handed over in a very safe way
        # because of \ and ~1 under windows
        self.algoGlobals['_tmp_file'] = self.algoFileName
        
        # Switch on all shown breakpoints
        for line in self.breakpoints:
            self.DB.set_break(self.algoFileName, line)
        try:
            command = "execfile(_tmp_file)"
            self.DB.run(command, self.algoGlobals, self.algoGlobals)
        except:
            log.exception("Bug in %s" % self.algoFileName)
            #traceback.print_exc()
            
        self.GUI.CommitStop()
        
    def Stop(self):
        """ TODO: Add docstring """
        self.mode = 0
        
    def Step(self):
        """ TODO: Add docstring """
        if self.animation_history is not None:
            self.animation_history.DoAll()        
        self.DB.doTrace = 0
        self.mode = 2 
        
    def Continue(self):
        """ TODO: Add docstring """
        if self.animation_history is not None:
            self.animation_history.DoAll()
        self.DB.doTrace = 0
        self.mode = 1
        
    def Trace(self):
        """ TODO: Add docstring """
        if self.animation_history is not None:
            self.animation_history.DoAll()
        self.mode = 2 
        self.DB.doTrace = 1
        
    def Replay(self):
        """ TODO: Add docstring """
        #self.GUI.CmdStep()
        if self.animation_history is not None:
            self.animation_history.DoAll()
            self.animation_history.Replay()
            
    def Undo(self):
        """ TODO: Add docstring """
        #self.GUI.CmdStep()
        if self.animation_history is not None:
            self.animation_history.Undo()
            
    def Do(self):
        """ TODO: Add docstring """
        #self.GUI.CmdStep()
        if self.animation_history is not None:
            self.animation_history.Do()    
            
    def ClearBreakpoints(self):
        """ Clear all breakpoints """
        for line in self.breakpoints:
            self.GUI.HideBreakpoint(line)
            self.DB.clear_break(self.algoFileName, line)
        self.breakpoints = []
        
    def SetBreakpoints(self, bp_list):
        """ SetBreakpoints is depreciated 
            NOTE: Use 'breakpoint' var in prolog instead. 
        
            Set all breakpoints in list: So an algorithm prolog
            can set a bunch of pre-assigned breakpoints at once """
        log.info("SetBreakpoints() is depreciated. Use 'breakpoint' var in prolog instead. ")
        for line in bp_list:
            self.GUI.ShowBreakpoint(line)
            self.breakpoints.append(line)
            self.DB.set_break(self.algoFileName, line)
            
            
    def ToggleBreakpoint(self, line=None):
        """ If we have a breakpoint on line, delete it, else add it. 
            If no line is passed we ask the DB for it"""
        
        if line == None:
            line = self.DB.lastLine
            
        if line in self.breakpoints:
            self.GUI.HideBreakpoint(line)
            self.breakpoints.remove(line)
            self.DB.clear_break(self.algoFileName, line)
        else: # New Breakpoint
        
            # check for not breaking in comments nor on empty lines. 
            import linecache
            codeline = linecache.getline(self.algoFileName, line)
            if codeline != '' and self.commentPattern.match(codeline) == None and \
                   self.blankLinePattern.match(codeline) == None:
                self.GUI.ShowBreakpoint(line)
                self.breakpoints.append(line)
                self.DB.set_break(self.algoFileName, line)
                
                
    def GetInteractiveLines(self):
        """ Return lines on which user interaction (e.g., choosing a 
            vertex occurrs. """
        return self.interactive
        
    def GetBreakpointLines(self):
        """ Return lines on which user interaction (e.g., choosing a 
            vertex occurrs. """
        return self.breakpoints
        
    def GetSource(self):
        """ Return the algorithms source """  
        return self.source
        
    def NeededProperties(self, propertyValueDict):
        """ Check that graph has that value for each property
            specified in the dictionary 'propertyValueDict' 
        
            If check fails algorithm is stopped 
        
            Proper names for properties are defined in gProperty
        """
        for prop, requiredValue in propertyValueDict.iteritems():
            failed = 0
            value = self.graph.get_property(prop)
            if value != 'Unknown':
                try:
                    c = cmp(value, requiredValue)
                    if gProperty[prop][0] < 0 and c > 0:
                        failed = 1
                    elif gProperty[prop][0] == 0 and c != 0:
                        failed = 1
                    if gProperty[prop][0] > 0 and c < 0:
                        failed = 1                            
                except ValueError:
                    failed = 1
            
            if failed or value == 'Unknown':
                errMsg = "The algorithm %s requires that the graph %s has %s" % \
                         (stripPath(self.algoFileName),
                          stripPath(self.graphFileName),
                          gProperty[prop][1])
                if gProperty[prop][0] < 0:
                    errMsg += " of %s or less" % str(requiredValue)
                elif gProperty[prop][0] > 0:
                    errMsg += " of %s or more" % str(requiredValue)
                errMsg += ". This is not known"
                errMsg += ".\nDo you still want to proceed ?"                          
                r = tkMessageBox.askokcancel("Gato - Error", errMsg)
                if not r:
                    raise AbortProlog, "User aborted at check for property %s" % prop
                    
    def PickVertex(self, default=None, filter_fun=None, visual=None):
        """ Pick a vertex interactively. 
        
            - default: specifies the vertex returned when user does not
              want to select one. If default==None, a random
              vertex not subject to filter will be returned.
        
            - filter: a function which should return a non-None value
              if the passed vertex is acceptable
        
            - visual is a function which takes the vertex as its 
              only argument and cause e.g. some visual feedback """
        v = None
        
        #log.debug("pickVertex %s" %s globals()['gInteractive'])
        if gInteractive == 1:
            v = self.GUI.PickInteractive('vertex', filter_fun, default)
            
        if v == None:
            if default == None:
                v = random.choice(self.graph.vertices)
            else:
                v = default
        if visual is not None:
            visual(v)
        return v
        
    def PickEdge(self, default=None, filter_fun=None, visual=None):
        """ Pick an edge interactively
        
            - default: specifies the edge returned when user does not
              want to select one. If default==None, a random
              edge not subject to filter will be returned
        
            - filter: a function which should return a non-None value
              if the passed edge is acceptable
        
            - visual is a function which takes the edge as its 
              only argument and cause e.g. some visual feedback """ 
        e = None
        
        if gInteractive == 1:
            e = self.GUI.PickInteractive('edge', filter_fun, default)
            
        if e == None:
            if default == None:
                e = random.choice(self.graph.get_all_edges())
            else:
                e = default
                
        if visual is not None:
            visual(e)
        return e
