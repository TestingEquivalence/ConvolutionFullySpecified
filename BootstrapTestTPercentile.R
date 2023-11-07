source("asymptoticTestBootstrapVariance.R")

tPercentileBootstrapTest<-function(parameter){
  dst=parameter$f(parameter$x,parameter$h)
  stDev=bootstrapSD(parameter)
  

  #calculate bootstrap distribution
  t.fun<-function(dat,ind){
    tryCatch({
      p=parameter
      p$x=dat[ind]
      dstBst=p$f(p$x,p$h)
      stDevBst=bootstrapSD(p)
      return((dstBst-dst)/stDevBst)
    }, error = function(e){
      return(NA)
    })
  }

  res=boot(parameter$x,t.fun,R=parameter$nSimPercentileTBootstrap)

  #calculate quantile of bootstrap distribution
  qt=quantile(res$t, parameter$alpha, type=1, na.rm=TRUE)
  min_eps=dst-stDev*qt
  
  res=list()
  res$distance=dst
  res$min_eps=min_eps
  return(res)
}