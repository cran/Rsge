# $Id: sge.submit.R,v 1.3 2007/04/01 22:27:22 coultn Exp $

"sge.submit" <- function(func, ..., 
                         savelist=NULL, 
                         packages=NULL, 
                         debug=FALSE,
                         file.prefix=getOption('sge.file.prefix')
                         )
  # savelist is a character vector of *names* of objects to be
  # copied to the remote R session
  {
    fname <- tempfile(pattern = file.prefix, tmpdir = getwd())

    sge.call <- as.call(list(func, ...) )

    savelist <- c(savelist, "sge.call", "packages")

    save(list=savelist, file=fname)

    qsub          <- getOption("sge.qsub")
    qsub.user.opt <- getOption("sge.user.options")
    qsub.options  <- getOption("sge.qsub.options")
    qsub.script   <- getOption("sge.script")
    script <- paste(file.path(.path.package("Rsge"), qsub.script), fname)
    result <- system(paste(qsub, qsub.user.opt, qsub.options, script), intern = TRUE)
    sge.ret.name = getOption("sge.ret.ext")
    sge.fname <- paste(fname,".", sge.ret.name, sep="")
    list(jobid=sge.get.jobid(result), filename=sge.fname)
  }
