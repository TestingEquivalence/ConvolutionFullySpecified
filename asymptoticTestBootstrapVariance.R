library(boot)

bootstrapSD<-function(parameter){
  #calculate bootstrap volatility
  p=parameter
  
  vol.fun<-function(dat,ind){
    tryCatch({
      nx=dat[ind]
      ts=p$f(nx,p$h)
      return(ts)
    }, error = function(e){
      return(NA)
    })
  }
  
  res=boot(p$x,vol.fun,R=p$nSimulation)
 
  return(sd(res$t, na.rm=TRUE))
}

asymptoticTestBootstrapVariance<-function(parameter){
  p=parameter
  vol = bootstrapSD(parameter)
  qt=qnorm(1-p$alpha,0,1)
  dst=p$f(p$x,p$h)

  min_eps = dst+ qt*vol
  res=list()
  res$distance=dst
  res$min_eps=min_eps
  return(res)
}
