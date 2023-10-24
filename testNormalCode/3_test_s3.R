source("testStatisticNormal.R")

sigma=2

# test s1 if Xi=Xj

f<-function(x){
  return(fK(x,sigma)*fK(x,sigma))
}



vf <- function(x) {
  if (is.vector(x)) {
    return(sapply(x,f))
  } else {
    return(f(x))
  }
}

integrate(vf,-5*sigma,5*sigma,subdivisions = 1000)
s3(sigma)
