\name{sge.parApply}
\alias{sge.parParApply}
\alias{sge.parRapply}
\alias{sge.parCapply}
\alias{sge.parLapply}
\alias{sge.parSapply}
\alias{sge.apply}
\title{Method used to integrate apply, lapply, sapply fcuntionality with qsub.}
\description{
  applies each row/column of a matrix or each element of a list to a function.
}

\usage{
  sge.apply   (X, MARGIN, FUN, ..., 
               join.method=cbind,
               njobs, batch.size=getOption('sge.block.size'),
               packages=NULL, savelist=NULL,
               cluster =getOption("sge.use.cluster"),
               trace=TRUE, debug=FALSE,
               file.prefix=getOption("sge.file.prefix"))
  sge.parRapply(X, FUN, ..., join.method=cbind, 
               njobs, batch.size=getOption('sge.block.size'),
               packages=NULL, savelist=NULL,
               cluster =getOption("sge.use.cluster"),
               trace=TRUE, debug=FALSE,
               file.prefix=getOption("sge.file.prefix"))
 
  sge.parCapply(X, FUN, ..., join.method=cbind, 
               njobs,batch.size=getOption('sge.block.size'),
               packages=NULL, savelist=NULL,
               cluster =getOption("sge.use.cluster"),
               trace=TRUE, debug=FALSE,
               file.prefix=getOption("sge.file.prefix"))
 
  sge.parLapply(X, FUN, ..., join.method=c, 
               njobs, batch.size=getOption('sge.block.size'),
               packages=NULL, savelist=NULL,
               cluster =getOption("sge.use.cluster"),
               trace=TRUE, debug=FALSE,
               file.prefix=getOption("sge.file.prefix")) 

  sge.parSapply(X, FUN, ...,  
                USE.NAMES=TRUE, simplify=TRUE,
                join.method=c, 
                njobs, batch.size=getOption('sge.block.size'),
                packages=NULL, savelist=NULL,
                cluster=getOption("sge.use.cluster"),
                trace=TRUE, debug=FALSE,
                file.prefix=getOption("sge.file.prefix"))
  sge.parParApply(X, FUN, ...,
               join.method=cbind,
               njobs,
               batch.size=getOption('sge.block.size'),
               trace=TRUE,
               packages=NULL,
               savelist=NULL,
               debug=FALSE,
               file.prefix=getOption('sge.file.prefix'),
               apply.method
              )

}
\arguments{
  \item{X}{Object to be applied to function (matrix, data.frame, list, array, or vector supported)}
  \item{MARGIN} {Used by sge.par to determime if (1) sge.Rapply or (2) sge.Capply should be called.}
  \item{FUN}{Function to be applied to object}
  \item{\dots}{Additional arguments to be applied to function}
  \item{njobs}{Number of parallel jobs to use}
  \item{join.method}{Function used to merge results from each job.}
  \item{batch.size}{Number of rows to include in parallel job if njobs is excluded.}
  \item{savelist}{Character vector giving the names of local objects
    that should be copied to each worker process before computation is
    started.}
  \item{packages}{List of library packages to be loaded by each worker
    process before computation is started. }
  \item{cluster} {determines if the job should be submitted to the cluster or run locally (default:submit to cluster, TRUE)}
  \item{trace}{Prints some information about job submission. (TRUE|FALSE)}
  \item{file.prefix} {Prefix for the data files used to pass information between nodes}
  \item{debug}{Prints debug level info about jobs. (TRUE|FALSE)}
  \item{USE.NAMES}{Determines if the sapply use names functionality should be used}
  \item{simplify}{Detemines if the sapply simplify functionality should be used}
  \item{apply.method}{tells apply if it should apply as a list or matrix}
}
\details{

  sge.parApply applies the function argument to either the rows, or elements of x. (depending on the value of apply.method)

    The arguments \code{packages} and \code{savelist} can be used to
  properly initialize the worker processes.

  sge.parCapply computes the transpose of X and uses it for the arguments of parRapply.

  sge.parLapply and sge.parSapply use lapply, sge.parCapply, and sge.parRapply use apply (Margin=1) 


}
\value{
Returns an object whose type is determined by the join.method. This object should be equivalent to the object that would be returned by an equivalent call to apply, sapply, or lapply. 
}

\author{Dan Bode \email{dbode@univaud.com}}

\examples{
}
\keyword{interface}
