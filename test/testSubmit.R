library(Rsge)
v1 <- c(1,2,3,4,5,6,7,8,9)
func1 <- function(x) {
 Sys.sleep(30) 
  x+7
}
l1 <- list(length=length(v1))
for(i in 1:length(v1)) {
  l1[[i]] <- sge.submit(func1, v1[[i]])
}

r1 <- lapply(l1, sge.job.status, qstat="qstat") 
while(! all(r1 == 0)) {
  Sys.sleep(4)
  r1 <- lapply(l1, sge.job.status)
}

lapply(l1, sge.list.get.result)

#savelist/package list test

GLOBAL1=77
GLOBAL2= list(a=100,b=200,c=300) 
f2 <- function(x) GLOBAL1 + GLOBAL2$b + x
r2 <- f2(2)
l2 <- sge.submit(f2, 2, savelist=c("GLOBAL1", "GLOBAL2"), debug=TRUE, file.prefix="YOYO")
x <- sge.job.status(l2$jobid, debug=TRUE)
while(! x == 0) {
  Sys.sleep(4)
  x <- sge.job.status(l2$jobid, debug=TRUE)
}
sge.list.get.result(l2, remove=FALSE)
r2Par <- sge.list.get.result(l2, remove=TRUE)
r2 == r2Par 

library("nlme")
f3 <- function(x) mean(Milk[[1]]) + GLOBAL2$c + x
r3 <- f3(3)
l3 <- sge.submit(f3, 3, savelist=c("GLOBAL2"), packages=c("nlme"), debug=TRUE, file.prefix="YOYO")
x <- sge.job.status(l3$jobid, debug=TRUE)
while(! x == 0) {
  Sys.sleep(4)
  x <- sge.job.status(l3$jobid, debug=TRUE)
}
sge.list.get.result(l3, remove=FALSE)
r3Par <- sge.list.get.result(l3, remove=TRUE)
r3 == r3Par 
