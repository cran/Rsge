\name{sge.submit}
\alias{sge.submit}
\title{Performs an asynchronous submission of a function to the remote node}
\description{
  Used to asynchronously submit R tasks to a SGE cluster. 
}
\usage{
sge.submit(func, ..., 
           savelist=NULL, packages=NULL, 
           debug=FALSE, file.prefix=getOption('sge.file.prefix')
           )
}
\arguments{
  \item{func}{Function to be executed remotely.}
  \item{...}{Arguments to be passed to the function.}
  \item{savelist}{Character vector giving the names of local objects
    that should be copied to each worker process before computation is
    started.}
  \item{packages}{List of library packages to be loaded by each worker
    process before computation is started.}
  \item{debug}{Prints debug level info about jobs.}
  \item{file.prefix}{Prefix used to create communication file}
}
\details{
  Submits work to SGE with an asynchronous qsub call. 
  The user needs to use sge.job.status and sge.list.get.result to monitor the progress of jobs and retrieve results.
  
}
\value{
  Returns a list that has the named element filename for the name of the input file created to send commands to the SGE cluster and a named element jobid with the id of the job submitted.
  list(jobid=JOBID, filename=FILENAME)
}
 
\author{Dan Bode \email{dbode@univaud.com}}


\seealso{\code{\link[Rsge]{sge.list.get.result}}  }

\examples{

}
\keyword{interface}
