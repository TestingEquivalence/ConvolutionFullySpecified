source("testStatisticUniform.R")

h=0.3

# test s1 if Xi=Xj

f<-function(x){
  return(fK(x,h)*fK(x,h))
}



vf <- function(x) {
  if (is.vector(x)) {
    return(sapply(x,f))
  } else {
    return(f(x))
  }
}

integrate(vf,-h,1+h,subdivisions = 1000)
s3(h)
