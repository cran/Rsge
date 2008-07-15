# $Id: sge.submit.R,v 1.3 2007/04/01 22:27:22 coultn Exp $

"sge.parPrep" <-
  function(func, ..., 
           savelist=c(), packages=NULL, 
           index=1, debug=FALSE, prefix=getOption("Rsge_data")
  )
  # savelist is a character vector of *names* of objects to be
  # copied to the remote R session
  {
# Extend the filenames to have the task array index 
    fname <- paste(prefix, "-", index,  sep="")
    sge.call <- as.call(list(func, ...) )
    savelist <- c(savelist, "sge.call", "packages")
    save(list=savelist, file=fname)
    sge.ret.name = getOption("sge.ret.ext")
    sge.fname <- paste(fname,".", sge.ret.name, sep="")
    sge.fname 
  }


