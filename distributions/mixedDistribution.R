
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
    
    x= rMixed(n, w, rf, rg)

    dst=distFun(x)
    return(dst-epsilon)
  }
  
  res=uniroot(f,c(0,1))
  return(res$root)
}
