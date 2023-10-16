source("testStatisticUniform.R")

h=0.1

vx=runif(100)

testStatisticUniformU(vx,h)
testStatisticUniform1(vx,h)
testStatisticUniform2(vx,h)

f<-function(x){
  return((fn(x,h, vx)-fK(x,h))^2)
}



vf <- function(x) {
  if (is.vector(x)) {
    return(sapply(x,f))
  } else {
    return(f(x))
  }
}

integrate(vf,-h-2,1+h+2, subdivisions = 1000000)
testStatisticUniform2(vx,h)
