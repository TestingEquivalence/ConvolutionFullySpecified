source("testStatisticUniform.R")

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

dst<-function(h){
  res=integrate(vf,-h,1+h, subdivisions = 1000000)
  return(res$value)
  
}

h=0.33
dst(h)
 