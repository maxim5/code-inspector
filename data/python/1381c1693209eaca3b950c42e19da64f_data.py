"""
data.py

by Charles Fracchia, Copyright (c) 2013

Data class module

This class defines the data object in a packet
"""
import warnings

class Data(object):
  """docstring for Data"""
  
  def __init__(self, name, vals):
    #super(Data, self).__init__()
    self.name = "%s" % name                   #Set the name
    
    isValid = self._validateVals(vals)        #Validate the passed vals array
    if (isValid != False) :
      self.vals = isValid                     #Store the values
    
  def _validateVals(self,vals):
    """
    Verifies that vals is a list and passes every dictionnary to be validated by _validateVal (singular)
    Takes the values array
    Returns the array with all the validated values if it's an array, False if it isn't (and warns user)
    """
    pass
    validatedVals = []
    if type(vals) == list:                          #Check that vals is an array
      for i,valDict in enumerate(vals):             #For each val object
        if self._validateVal(valDict) == True:      #Validate each val object
          validatedVals.append(valDict)             #Add val to the vals array, subroutine warns user in negative case
      return validatedVals
    else:
      warnings.warn("The values should be an array of dictionnaries with the following structure: [ {'time':433,'val':45}, {'time':655,'val':77} ]. \
      This data packet will be malformed.")
      return False
    
  def _validateVal(self,valDict):
    """
    Validates value dictionnaries one at a time
    Takes a value dictionnary: {'time':timeval, 'val',value}
    Returns the True if correctly formed, False false otherwise.
    """
    pass
    #print "Validating the following vals dictionnary: %s" % valDict      #DEBUG
    if type(valDict) == dict:                       #Check the val array type
      #TIME Checking
      try:                                          #Need to do a try to make sure the error is handled correctly in case of missing key
        if (type(valDict['time']) in (int,float)):
          timeOK = True                             #Set flag that TIME is ok
        else:
          timeOK = False                            #Set flag that TIME is not ok
          warnings.warn("The TIME attribute in the value packet needs to be either an integer or a float. Ignoring this timepoint")
      except KeyError:
        timeOK = False                              #Set flag that TIME is not ok
        warnings.warn("The value packet is missing the TIME attribute. Ignoring this timepoint.")
      
      #VAL Checking
      try:                                          #Need to do a try to make sure the error is handled correctly in case of missing key
        if (type(valDict['val']) in (int,float)):
          valOK = True                              #Set flag that VAL is ok
        else:
          valOK = False                             #Set flag that VAL is not ok
          #warnings.warn("The VAL attribute in the value packet needs to be either an integer or a float. Ignoring this packet")
      except KeyError:
        valOK = False                               #Set flag that VAL is not ok
        warnings.warn("The value packet is missing the VAL attribute. Ignoring this timepoint.")
        
      if timeOK and valOK:
        return True
      else:
        #print "The value packet is malformed. Ignoring this timepoint."        #DEBUG
        warnings.warn("The value packet is malformed. Ignoring this timepoint.")
        return False
        
    else:
      warnings.warn("A value packet was not in the correct format. The value packet needs to be a dictionnary in the following form: {'time':timeval, 'val':integer or float}. Ignoring this timepoint.")     #Warn the user
      return False