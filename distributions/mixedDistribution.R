
rMixed<-function(n,w,rf,rg){
  vw=rbinom(n=n, size=1,prob=w)
  vf=rf(n)
  vg=rg(n)
  res=vw*vf+(1-vw)*vg
  return(res)
}

boundaryPoint<-function(epsilon,rf, distFun){
  vdst=c()
  set.seed(10071977)
  dst=distFun(rf) 
  w=epsilon/dst
  w=sqrt(w)
  
  fMixed<-function(n){
    return(rMixed(n,w,rf, runif))
  }
  
  res=list()
  res$w=w
  res$fMixed=fMixed
  
  return(res)
}

distFunU<-function(rf,h,n){
  vdst=c()
  for (j in c(1:100)){
    x=rf(n)
    vdst=c(vdst,testStatisticUniformU(x, h))
  }
  return(vdst)
}