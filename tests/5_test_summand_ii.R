source("testStatisticUniform.R")

h=0.1
xi=0.5

f<-function(x){
  return((K(x,xi,h)-fK(x,h))^2)
}



vf <- function(x) {
  if (is.vector(x)) {
    return(sapply(x,f))
  } else {
    return(f(x))
  }
}

integrate(vf,xi-3*h,xi+3*h, subdivisions = 100000)
s11(h)-2*s2(xi,h)+s3(h)
