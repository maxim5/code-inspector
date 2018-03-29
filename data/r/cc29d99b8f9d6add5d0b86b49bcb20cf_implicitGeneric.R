### Name: implicitGeneric
### Title: Manage Implicit Versions of Generic Functions
### Aliases: implicitGeneric setGenericImplicit prohibitGeneric
###   registerImplicitGenerics 'implicit generic'
### Keywords: programming methods

### ** Examples


### How we would make the function with() into a generic:

## Since the second argument, 'expr' is used literally, we want
## with() to only have "data" in the signature.

## Note that 'methods'-internal code now has already extended  with()
## to do the equivalent of the following
## Not run: 
##D setGeneric("with", signature = "data")
##D ## Now we could predefine methods for "with" if we wanted to.
##D 
##D ## When ready, we store the generic as implicit, and restore the original
##D setGenericImplicit("with")
##D 
##D ## (This example would only work if we "owned" function with(),
##D ##  but it is in base.)
## End(Not run)

implicitGeneric("with")



