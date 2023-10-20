source("testStatisticUniform.R")

h=0.1

# test s1 if Xi=Xj
x1=1

f<-function(x){
  return(K(x,x1,h)*fK(x,h))
}



vf <- function(x) {
  if (is.vector(x)) {
    return(sapply(x,f))
  } else {
    return(f(x))
  }
}

integrate(vf,x1-h/2,x1+h/2,subdivisions = 1000)
s2(x1,h)

