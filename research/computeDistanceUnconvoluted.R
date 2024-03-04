source("testStatisticUniform.R")

dst<-function(h){
  f<-function(x){
    return((fU(x)-fK(x,h))^2)
  }
  vf <- function(x) {
    if (is.vector(x)) {
      return(sapply(x,f))
    } else {
      return(f(x))
    }
  }
  
  res=integrate(vf,-h,1+h, subdivisions = 1000000)
  return(res$value)
  
}

vh=seq(0.1,1, by=0.1)
vdst=sapply(vh,dst)
res=data.frame(h=vh,dst=vdst)
write.csv(res,"dstUnconvoluted.csv")
