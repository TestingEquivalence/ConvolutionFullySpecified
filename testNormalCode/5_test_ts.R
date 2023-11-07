source("testStatisticNormal.R")

sigma=0.1

vx=rnorm(100)

testStatisticNormalU(vx,sigma)
testStatisticNormal1(vx,sigma)


f<-function(x){
  return((fn(x,sigma,vx)-fK(x,sigma))^2)
}



vf <- function(x) {
  if (is.vector(x)) {
    return(sapply(x,f))
  } else {
    return(f(x))
  }
}

integrate(vf,-3*(1+sigma),3*(1+sigma), subdivisions = 100000)
testStatisticNormal2(vx,sigma)
testStatisticNormalFull(vx,sigma)
