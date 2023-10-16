source("testStatisticUniform.R")

h=0.1

# test s1 if Xi=Xj
x1=-0.5

f<-function(x){
  return(K(x,x1,h)*K(x,x1,h))
}

vf <- function(x) {
  if (is.vector(x)) {
    return(sapply(x,f))
  } else {
    return(f(x))
  }
}

integrate(vf,x1-h,x1+h,subdivisions = 1000)
s11(h)
