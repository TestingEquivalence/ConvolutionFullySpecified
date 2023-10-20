source("testStatisticUniform.R")

h=0.1

# test s1 if Xi<>Xj
x1=0
x2=0.10

f<-function(x){
  return(K(x,x1,h)*K(x,x2,h))
}

vf <- function(x) {
  if (is.vector(x)) {
    return(sapply(x,f))
  } else {
    return(f(x))
  }
}

f(0.041)
integrate(vf,x1-h,x2+h,subdivisions = 1000)
s1(x1,x2,h)
