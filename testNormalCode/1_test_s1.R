source("testStatisticNormal.R")

sigma=1

# test s1 if 
x1=0
x2=0

f<-function(x){
  return(K(x,x1,sigma)*K(x,x2,sigma))
}

vf <- function(x) {
  if (is.vector(x)) {
    return(sapply(x,f))
  } else {
    return(f(x))
  }
}

integrate(vf,x1-3*sigma,x2+3*sigma,subdivisions = 10000)
s1(x1,x2,sigma)
 