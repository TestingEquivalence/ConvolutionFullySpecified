source("testStatisticNormal.R")

sigma=0.1
xi=-1

f<-function(x){
  return((K(x,xi,sigma)-fK(x,sigma))^2)
}



vf <- function(x) {
  if (is.vector(x)) {
    return(sapply(x,f))
  } else {
    return(f(x))
  }
}

integrate(vf,xi-3*(1+sigma),xi+3*(1+sigma), subdivisions = 100000)
s1(xi,xi,sigma)-2*s2(xi,sigma)+s3(sigma)
