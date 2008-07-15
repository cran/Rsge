library(Rsge)
func1 <- function(x) {
#  print(x) 
  x+3
}
func2 <- function(x,y,z) {
  (x + y) * z
}


func9 <- function(fileprefix, join.method, size) {
  filenames <- vector(length=size)
  for(i in 1:size) {
    filenames[i] <- paste(fileprefix, i, sep="-")
  }
  lapply( filenames, sge.get.result, debug = TRUE, jobid = 0 )
}

#
# Tests for matrices
#

m1 <- matrix(rnorm(5), ncol=1)
m2 <- array(1:20, dim=c(4,5))

#mr1    <- apply(m1,1, func1)
#mr1Par <- sge.parRapply(m1, func1, njobs = 2, join.method = cbind)

# 1-D matrix no arguments numeric funtion join with c
mr1    <- sge.apply(m1,1, func1, cluster=FALSE)
mr1Par <- sge.parRapply(m1, func1, njobs = 2, join.method = c)
all(mr1 == mr1Par)

#testing different split sizes (different number of tasks)
mr1.1    <- sge.apply(m1,1, func1, cluster=FALSE, trace=FALSE)
mr1.1Par <- sge.parRapply(m1, func1, njobs = 1, join.method = c, trace=FALSE)
all(mr1.1 == mr1.1Par)

mr1.2  <- sge.apply(m1,1, func1, cluster=FALSE)
mr1.2Par <- sge.parRapply(m1, func1, njobs = 6, join.method = c)
all(mr1.2 == mr1.2Par)

#trying with an argument that takes additional functions
options(sge.use.cluster="FALSE")
mr1.3  <- sge.parRapply(m1, func2, 3, 4, njobs = 4, join.method = c) 
options(sge.use.cluster="TRUE")
mr1.3Par <- sge.parRapply(m1, func2, 3, 4, njobs = 4, join.method = c)
all(mr1.3 == mr1.3Par)

#cbind will only work with 1-D if the number of jobs is a multiple of the number of elements
options(sge.use.cluster="FALSE")
mr1.4  <- sge.parRapply(m1, func2, 3, 4)
mr1.5  <- sge.apply(m1,1, mean)
mr1.6  <- sge.parCapply(m1, func1)
mr1.7  <- sge.apply(m1,2, mean)

options(sge.use.cluster="TRUE")

mr1.4Par <- sge.parRapply(m1, func2, 3, 4, njobs = 5, join.method = cbind)
all(mr1.4 == mr1.4Par)

mr1.5Par <- sge.parRapply(m1, mean, njobs = 2, join.method = c)
all(mr1.5 == mr1.5Par)

mr1.6Par <- sge.parCapply(m1, func1, njobs = 2, join.method = cbind)
all(mr1.6 == mr1.6Par)

mr1.7Par <- sge.parCapply(m1, mean, njobs = 3, join.method = c)
all(mr1.7 == mr1.7Par)


options(sge.use.cluster="FALSE")
mr2      <- sge.parRapply(m2, func1)
mr2.1    <- sge.apply(m2,1, func1)
mr2.2    <- sge.apply(m2,1, func1)
mr2.3    <- sge.apply(m2,1, func1)
mr2.4    <- sge.apply(m2,1, mean)
mr2.5    <- sge.apply(m2,1, mean)
options(sge.use.cluster="TRUE")

mr2Par <- sge.parRapply(m2, func1, njobs = 5, join.method = cbind, trace=FALSE) 
all(mr2Par == mr2)

mr2.1Par <- sge.parRapply(m2, func1, njobs = 2, join.method = cbind, trace=FALSE) 
all(mr2.1Par == mr2.1)

mr2.2Par <- sge.parRapply(m2, func1, njobs = 1, join.method = cbind, trace=FALSE) 
all(mr2.2Par == mr2.2)

mr2.3Par <- sge.parRapply(m2, func1, njobs = 7, join.method = cbind, trace=FALSE) 
all(mr2.3Par == mr2.3)

mr2.4Par <- sge.parRapply(m2, mean, njobs = 3, trace=FALSE) 
all(mr2.4, mr2.4Par)

mr2.5Par <- sge.parRapply(m2, mean, njobs = 9, debug=TRUE) 
all(mr2.5, mr2.5Par)


# Does not work, I will need to get to the bottom of this and quickly
#
#
# in fun(quote(c(9, 12, 15, 18, 21, 24, 27, 30)), quote(c(33, 36,  :
#  number of columns of matrices must match (see arg 2)
#
options(sge.use.cluster="FALSE")
mr2.6    <- sge.apply(m2,1, func2, 2, 3)
mr2.7    <- sge.parCapply(m2, mean)
mr2.8    <- sge.parRapply(m2, func2, 4, 5, trace=FALSE)
options(sge.use.cluster="TRUE")

mr2.6Par <- sge.parCapply(m2, func2, 2, 3, njobs = 3, join.method = cbind, trace=FALSE) 
all(mr2.6, mr2.6Par)

mr2.7Par <- sge.parCapply(m2, mean, njobs = 3, join.method = c, trace=FALSE) 
all(mr2.7, mr2.7Par)

# try a function with arguments
mr2.8Par <- sge.parRapply(m2, func2, 4, 5, njobs = 3, debug=FALSE)
all(mr2.8 == mr2.8Par)

#vector
v1 <- c(12,24,25,26,27,38,4,7,435)
v2 <- c(list(1,2), list(4,5))
v3 <- c(c(1,2,3), c(10,11,12))
v4 <- c(array(1:20, dim=c(4,5)),array(4,5))

#vector tests

options(sge.use.cluster="FALSE")
vr1    <- sge.parLapply(v1, func1)
vr2    <- sge.parLapply(v2, func1)
vr3    <- sge.parLapply(v3, func1)
vr4    <- sge.parLapply(v4, func1)
options(sge.use.cluster="TRUE")

vr1Par <- sge.parLapply(v1, func1, njobs = 3, trace=FALSE)
all(vr1 == unlist(vr1Par))
vr2Par <- sge.parLapply(v2, func1, njobs = 3, join.method = c)
all(vr2 ==unlist( vr2Par))
vr3Par <- sge.parLapply(v3, func1, njobs = 3, join.method = c, debug=TRUE)
all(vr3 ==unlist( vr3Par))
vr4Par  <- sge.parLapply(v4, func1, njobs = 4, join.method = c, trace=FALSE)
all(vr4 ==unlist( vr4Par))
# test save lists

# test package loads


# list tests

l1 <- list(c(1,2,4),c(3,5,78),c(2,3,4,5,6))
l2 <- list(array(1:56),array(3:102),array(20:27 ))
l3 <- list(45, 46, 47, 48, 49)

options(sge.use.cluster="FALSE")
rl1    <- sge.parLapply(l1, mean)
rl2    <- sge.parLapply(l2, mean)
rl3    <- sge.parLapply(l3, func1)
rl4    <- sge.parLapply(l2, func1)
rl5    <- sge.parLapply(l1, func1)
options(sge.use.cluster="TRUE")

rl1Par <- sge.parLapply(l1, mean, njobs = 3, trace=FALSE)
all(rl1 == unlist(rl1Par))

rl2Par <- sge.parLapply(l2, mean, njobs = 3, trace=FALSE)
all(rl2 == unlist(rl2Par))

rl3Par <- sge.parLapply(l3, func1, njobs = 3, trace=FALSE)
all(rl3 == unlist(rl3Par))

rl4Par <- sge.parLapply(l2, func1, njobs = 3, trace=FALSE)
all(unlist(rl4) == unlist(rl4Par))

rl5Par <- sge.parLapply(l1, func1, njobs = 3, trace=FALSE)
all(unlist(rl5) == unlist(rl5Par))

#testing global variables and package imports
hugeVector <- c(1:1000000)
library("nlme")
GLOBAL1 <- 123
f1   <- function(x,y) mean(Milk[[1]]) + x + GLOBAL1 + y


x1   <-sge.parLapply(hugeVector, f1, 7, cluster=FALSE)

xPar <- sge.parLapply(hugeVector, f1, 7, packages=c("nlme"), savelist=c("GLOBAL1"), batch.size=10000) 
all(x1 == unlist(xPar))

hugeVector2 <- array(1:1000000, dim=c(1000,1000))

x2   <- sge.apply(hugeVector2, MARGIN=1, f1, 7, cluster=FALSE)

x2Par <- sge.parRapply(hugeVector2, f1, 7, packages=c("nlme"), savelist=c("GLOBAL1")) 
all(x2 == x2Par)
