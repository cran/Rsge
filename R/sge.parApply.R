
                                        # $Id: sge.parRapply.R,v 1.2 2006/12/15 15:21:23 kuhna03 Exp $

sge.apply <- function(X, MARGIN, FUN, ..., 
                          join.method=cbind,
                          njobs,
                          batch.size=getOption('sge.block.size'),
                          packages=NULL,
                          savelist=NULL,
                          cluster=getOption('sge.use.cluster'),
                          trace=TRUE,
                          debug=FALSE,
                          file.prefix=getOption('sge.file.prefix')
                         ) {
    if(MARGIN == 1) {
      sge.parRapply(X, FUN, ...,
                   join.method=join.method,cluster=cluster,
                   njobs=njobs, batch.size=batch.size,
                   packages=packages, savelist=savelist,
                   trace=trace, debug=debug, file.prefix=file.prefix)
    } else {
      sge.parCapply(X, FUN, ...,
                   join.method=join.method,cluster=cluster,
                   njobs=njobs, batch.size=batch.size,
                   packages=packages, savelist=savelist,
                   trace=trace, debug=debug, file.prefix=file.prefix)
    } 
} 

sge.parCapply <- function(X, FUN, ..., 
                          join.method=cbind,
                          njobs,
                          batch.size=getOption('sge.block.size'),
                          packages=NULL,
                          savelist=NULL,
                          cluster=getOption('sge.use.cluster'),
                          trace=TRUE,
                          debug=FALSE,
                          file.prefix=getOption('sge.file.prefix')
                         ) {
  if(cluster) {
    sge.parParApply(t(X), FUN, ..., 
               join.method=join.method,  
               njobs=njobs, batch.size=batch.size,
               packages=packages, savelist=savelist,
               trace=trace, debug=debug, file.prefix=file.prefix, apply.method=2
               )
  } else {
    if(trace) cat("Running locally \n")
    apply(X=X, MARGIN=2 ,FUN=FUN, ...)
  }
 
}

sge.parRapply <- function(X, FUN, ...,
                          join.method=cbind,
                          njobs,
                          batch.size=getOption('sge.block.size'),
                          packages=NULL,
                          savelist=NULL,
                          cluster=getOption('sge.use.cluster'),
                          trace=TRUE,
                          debug=FALSE,
                          file.prefix=getOption('sge.file.prefix')
                         ) {
  if(cluster) {
    sge.parParApply(X, FUN, ...,  
                join.method=join.method, 
                njobs=njobs, batch.size=batch.size,
                packages=packages, savelist=savelist,
                trace=trace, debug=debug, file.prefix=file.prefix, apply.method=2
                )
  } else {
    if(trace) cat("Running locally \n")
    apply(X=X, MARGIN=1 ,FUN=FUN, ...)
  }
}

sge.parLapply <- function(X, FUN, ..., 
                          join.method=c, 
                          njobs,
                          batch.size=getOption('sge.block.size'),
                          packages=NULL,
                          savelist=NULL,
                          cluster=getOption('sge.use.cluster'),
                          trace=TRUE,
                          debug=FALSE,
                          file.prefix=getOption('sge.file.prefix')
                          ) {
  if(cluster) {
    sge.parParApply(X, FUN, ...,
                join.method=join.method, njobs=njobs, batch.size=batch.size,
                packages=packages, savelist=savelist,
                trace=trace, debug=debug, file.prefix=file.prefix, apply.method=1
                )
  } else {
    if(trace) cat("Running locally\n")
    lapply(X=X, FUN=FUN, ...)
  }
}

# this code was blatently taken from snow, whose code was taken from sapply.R

sge.parSapply <- function(X, FUN, ..., 
                          USE.NAMES=TRUE, simplify=TRUE,
                          join.method=c, 
                          njobs,
                          batch.size=getOption('sge.block.size'),
                          packages=NULL,
                          savelist=NULL,
                          cluster=getOption('sge.use.cluster'),
                          trace=TRUE,
                          debug=FALSE,
                          file.prefix=getOption('sge.file.prefix')
                         )
{
  
  if(cluster) {
    FUN <- match.fun(FUN) # should this be done on slave?
    answer <- sge.parParApply(X, FUN, ...,
               join.method=join.method, njobs=njobs, 
               batch.size=batch.size,
               packages=packages, savelist=savelist,
               trace=trace, debug=debug, 
               file.prefix=file.prefix, apply.method=1
              )
#    answer <- sge.parLapply(as.list(x), fun, ...)
      if (USE.NAMES && is.character(X) && is.null(names(answer)))
        names(answer) <- X
      if (simplify && length(answer) != 0) {
        common.len <- unique(unlist(lapply(answer, length)))
        if (common.len == 1)
            unlist(answer, recursive = FALSE)
        else if (common.len > 1)
            array(unlist(answer, recursive = FALSE),
                  dim = c(common.len, length(X)),
                  dimnames = list(names(answer[[1]]), names(answer)))
        else answer
      }
      else answer
  } else {
    if(trace) cat("Running locally\n")
    sapply(X=X, FUN=FUN, ..., simplify=simplify, USE.NAMES=USE.NAMES)
  } 
}

sge.parParApply <- function (X, FUN, ...,
                           join.method=cbind,
                           njobs,
                           batch.size=getOption('sge.block.size'),
                           trace=TRUE,
                           packages=NULL,
                           savelist=NULL,
                           debug=FALSE,
                           file.prefix=getOption('sge.file.prefix'),
                           apply.method=2

                         )
  {
    if(missing(njobs) && (is.matrix(X) || is.data.frame(X)))
      njobs <- max(1,ceiling(nrow(X)/batch.size))    
    else if(missing(njobs) && (is.vector(X) || is.list(X)))
      njobs <- max(1,ceiling(length(X)/batch.size))    

    if(debug) print(X)
    if(njobs>1)
      rowSet <- sge.split(X, njobs)
    else
      rowSet <- list(X)
    if(debug) print(rowSet)    
    prefix <- tempfile(pattern = file.prefix, tmpdir = getwd())
    filenames <- vector(length=length(rowSet))
    for (i in 1:length(rowSet)) {
      if(apply.method == 1) {
        filenames[i] <- sge.parPrep(
               lapply, X=rowSet[[i]], FUN=FUN, ..., 
    	       savelist=savelist, 
               packages=packages, 
               index=i,
               prefix=prefix
              )
      } else if(apply.method ==2) {
        filenames[i] <- sge.parPrep(
               apply, X=rowSet[[i]], MARGIN=1, FUN=FUN, ...,
               savelist=savelist,
               packages=packages,
               index=i,
               prefix=prefix
              )
      }
    } 
    if(trace) cat("Completed storing environment to disk\n")
    if(trace) cat("Submitting ",length(rowSet), "jobs...\n")
    if(debug) print(filenames)
    qsub          <- getOption("sge.qsub")
    qsub.options  <- getOption("sge.qsub.options")
    qsub.user.opt <- getOption("sge.user.options")
    qsub.blocking <- getOption("sge.qsub.blocking")
    qsub.script   <- getOption("sge.script")
    script <- paste(file.path(.path.package("Rsge"), qsub.script), prefix)
    result <- system(paste(qsub, " ",qsub.user.opt, " ", qsub.options, " ", qsub.blocking,  length(rowSet), " ", script, " 2>&1", sep=""), intern = TRUE)
    if(sge.checkNotNow(result)) {
      cat("now option set, could not run now on cluster, running local.\n")
      if(apply.method == 1) {
        return(lapply(X=X, FUN=FUN, ...))
      } else {
        return(apply(X=X, MARGIN=1, FUN=FUN, ...))
      }
    }
    if(debug) cat( result, "\n")
    if(trace) cat("All jobs completed\n") 
    jobid <- sge.get.jobid(result)
    # I am not sure how well R can handle this, maybe it will not scale
    system(paste("for i in `ls *.e",jobid,"*`; do cat $i; done", sep=""))
    results <- lapply( filenames, sge.get.result, jobid = jobid, remove = !debug)
    if(debug) print (results)
    retval <- docall(join.method, results)
    retval
  }
