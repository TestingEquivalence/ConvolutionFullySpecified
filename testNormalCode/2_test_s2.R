source("testStatisticNormal.R")

sigma=2

# test s1 if Xi=Xj
x1=0

f<-function(x){
  return(K(x,x1,sigma)*fK(x,sigma))
}



vf <- function(x) {
  if (is.vector(x)) {
    return(sapply(x,f))
  } else {
    return(f(x))
  }
}

integrate(vf,x1-3*sigma,x1+3*sigma,subdivisions = 1000)
s2(x1,sigma)

