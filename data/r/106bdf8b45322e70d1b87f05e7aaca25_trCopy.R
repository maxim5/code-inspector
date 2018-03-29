#=======================================================================
# Orinal author: Philippe Grosjean
# Original code: [svIO] copy function
# Adapted by   : Jose Claudio Faria
# Objective    : To supply the current necessity of the Tinn-R project
# Date         : 09/03/2008 1:25 PM
#=======================================================================

trCopy <- function(x,
                   type="raw",
                   objname=deparse(substitute(x)), ...)
{
  objname <- objname
  # Compute the expression
  xexp <- try(ifelse(inherits(x,
                              "expression"),
                     x,
                     NULL),
              silent=TRUE)
  if(inherits(xexp,
              "try-error") || is.null(xexp)) {
    xexp <- substitute(x)
    # To make sure that non conventional names will be correctly evaluated,
    # we use backticks!
    if(is.character(xexp))
      xexp <- parse(text=paste("`",
                               xexp,
                               "`",
                               sep=""))
    xexp <- as.expression(xexp)
  }
  # The way to copy to the clipboard is platform-dependent
  # This is Windows
  if(.Platform$OS == "windows")
    trExport(x=xexp,
             type=type,
             file="clipboard",
             append=FALSE,
             objname=objname, ...)
  else { ### Rem: according to a suggestion by Ted Harding... not tested yet!
    File <- tempfile()
    trExport(x=xexp,
             type=type,
             file=File,
             append=FALSE,
             objname=objname, ...)
    # TO DO: manage errors!
    system(paste("wxcopy <",
                 File),
           wait=TRUE,
           invisible=TRUE)
    unlink(File)
  }
}
