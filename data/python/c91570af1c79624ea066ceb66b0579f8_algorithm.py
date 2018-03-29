'''
Created on Aug 1, 2010

@author: Jake
'''
from enthought.traits.api import HasTraits
from enthought.traits.api import (Any, Instance, Enum, 
                                  Bool, Float, Property, List)

from enthought.traits.ui.api import View, Group, VSplit, HSplit, Item, ListEditor, VGroup, HGroup, Handler

from pyperoxidase.ec.i_ec_algorithm import IAlgorithmComponent, IAlgorithmFunction    

class ECAlgorithm(HasTraits):
    # An Evolutionary Algorithm Tree Node
    # for building a visual representation of
    # the EC algorithm
    
    name  = 'EC Algorithm'
    
    algorithm = Enum("Custom", "ES", "DEA", "EDA", "SA")
    
    #-------- EC Algorithm Components ---------------
    evaluators = Property(Instance(IAlgorithmFunction))
    generators = Property(Instance(IAlgorithmFunction))
    observers = Property(List(IAlgorithmFunction))
    replacers = Property(Instance(IAlgorithmFunction))
    selectors = Property(Instance(IAlgorithmFunction))
    terminators = Property(List(IAlgorithmFunction))
    variators = Property(List(IAlgorithmFunction))
    
    components = List(IAlgorithmComponent)
    
    #-------------------------
    # 'object' Interface
    #-------------------------
    def __init__(self, **traits):
        super(ECAlgorithm, self).__init__(**traits)
        return
    
    #--------------------------
    # 'HasTraits' Interface
    #--------------------------
    def _algorithm_changed(self):
        if self.algorithm == "DEA":
            self.update_functions_dea()
        elif self.algorithm == "EDA":
            self.update_functions_eda()
        elif self.algorithm == "ES":
            self.update_functions_es()
        elif self.algorithm == "SA":
            self.update_functions_sa()
        elif self.algorithm == "Custom":
            pass
        return
    
    #--------------------------
    # 'Property' Interface
    #--------------------------
    def _get_evaluators(self):
        return [obj for obj in self.components if
                obj.name == 'Evaluator'][0]
                      
    def _get_generators(self):
        return [obj for obj in self.components if 
                obj.name == 'Generator'][0]
        
    def _get_observers(self):
        return [obj for obj in self.components if
                obj.name == 'Observers'][0]
                
    def _get_replacers(self):
        return [obj for obj in self.components if
                obj.name == 'Replacer'][0]
                    
    def _get_selectors(self):
        return [obj for obj in self.components if
                obj.name == 'Selector'][0]
        
    
    def _get_terminators(self):
        return [obj for obj in self.components if 
                obj.name == 'Terminators'][0]
        
    def _get_variators(self):
        return [obj for obj in self.components if
                obj.name == 'Variators'][0]
    
    #------------------
    # Methods
    #-------------------
    def update_functions_sa(self):
        self.variators.clear_functions()
        self.variators.load_function('Gaussian Mutation')
        
        self.selectors.clear_functions()
        
        self.replacers.clear_functions()
        self.replacers.load_function('SA Replacement')
        return
        
    def update_functions_es(self):
        self.selectors.clear_functions()
        
        self.variators.clear_functions()
        self.variators.load_function('Gaussian Mutation')
        
        self.replacers.clear_functions()
        self.replacers.load_function('Plus Replacement')    
        return
    
    def update_functions_eda(self):
        self.selectors.clear_functions()
        self.selectors.load_function('Truncation Selection')
        
        self.variators.clear_functions()
        self.variators.load_function('Est. Distribution of Variation')
        
        self.replacers.clear_functions()
        self.replacers.load_function('Generational Replacement')
        return
    
    def update_functions_dea(self):
        self.selectors.clear_functions()
        self.selectors.load_function('Tournament Selection')
        
        self.replacers.clear_functions()
        self.replacers.load_function('Steady State Replacement')
        
        self.variators.clear_functions()
        self.variators.load_function('Differential Crossover')
        self.variators.load_function('Gaussian Mutation')
    
        return
    
