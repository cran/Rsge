# $Id: sge.get.result.R,v 1.2 2006/12/15 15:21:23 kuhna03 Exp $

sge.list.get.result <- function(list, ...) {
#  I dont like having this here, but its the best place, the end user should not have to make these calls
  system(paste("for i in `ls *.e",list$jobid,"*`; do cat $i; done", sep=""))
  sge.get.result(sge.fname=list$filename, jobid=list$jobid, ...)
}

"sge.get.result" <-
  function(sge.fname, jobid,  remove = FALSE)
  {
#   I changed the filename to be the one to retrieve
#    sge.ret.name = getOption("sge.ret.ext")
#    sge.fname <- paste(filename,".", sge.ret.name, sep="")
    sge.ret.name <- getOption("sge.ret.ext") 
    sge.ret.ext <- paste(".", sge.ret.name, sep="")
    filename <- strsplit(sge.fname, sge.ret.ext)[[1]]
#cat("|",sge.fname,"|", filename ,"|\n")
    if (!file.exists(sge.fname)) {
      warning(paste("Expected file", sge.fname, "Does not exist at:", getwd()))
      return(NULL)
    }
# print out stderr from all of the jobs (this assumes that 
    
    load(sge.fname)
    if (remove == TRUE) {
      file.remove(filename)
      file.remove(sge.fname)
    }
    ret <- get(sge.ret.name)
    if (class(ret) == "try-error")
      warning("Error(s) encountered in the remote R session")
    ret
  }
