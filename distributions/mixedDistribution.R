
dMixed<-function(x,w,f,g){
  return(w*f(x)+(1-w)*g(x))
}

pMixed<-function(x,w,H,G){
  return(w*H(x)+(1-w)*G(x))
}

rMixed<-function(n,w,rf,rg){
  vw=rbinom(n=n, size=1,prob=w)
  vf=rf(n)
  vg=rg(n)
  res=vw*vf+(1-vw)*vf
  return(res)
}

boundaryPoint<-function(epsilon,rf, rg, n, distFun){
  f<-function(w){
    set.seed(10071977)
    vdst=c()
    
    for (i in c(1:100)){
      x= rMixed(n, w, rf, rg)
      vdst=c(vdst,distFun(x))
    }
      
    dst=mean(vdst)
    return(dst-epsilon)
  }
  
  res=uniroot(f,c(0,1))
  return(res$root)
}
