s1<-function(x1,x2,sigma){
  s=2*sigma*sigma
  s=sqrt(s)
  return(dnorm(x1,x2,s))
}

s2<-function(x,sigma){
  s=1+2*sigma*sigma
  s=sqrt(s)
  return(dnorm(0,x,s))
}

s3<-function(sigma){
  s=2*(1+sigma*sigma)
  s=sqrt(s)
  return(dnorm(0,0,s))
}

s<-function(x1,x2,sigma){
  return(s1(x1,x2,sigma)-s2(x1,sigma)-s2(x2,sigma)+s3(sigma))
}

testStatisticNormal1<-function(x,sigma){
  n=length(x)
  r=0
  for (i in c(2:n)){
    for (j in c(1:(i-1))){
      r=r+s(x[i],x[j],sigma)
    }
  }
  r=2*r/(n*(n-1))
  return(r)
}

testStatisticNormal2<-function(x,sigma){
  n=length(x)
  r=testStatisticNormal1(x,sigma)*n*(n-1)
  
  for (i in c(1:n)){
    r=r+s1(x[i],x[i],sigma)-2*s2(x[i],sigma)+s3(sigma)
  }
  
  r=r/(n*(n-1))
  
  return(r)
}

testStatisticNormalU<-function(x,sigma){
  n=length(x)
  r1=n*(n-1)*s3(sigma)/2
    
  r2=0
  for (i in c(1:n)){
    r2=r2+s2(x[i],sigma)
  }
  
  r3=0
  for (i in c(2:n)){
    for (j in c(1:(i-1))){
      r3=r3+s1(x[i],x[j],sigma)
    }
  }
  
  r=r1-(n-1)*r2+r3
  r=2*r/(n*(n-1))
  
  return(r)
}

fK<-function(x,sigma){
  s=sqrt(1+sigma*sigma)
  return(dnorm(x,0,s))
}

K<-function(x,xi,sigma){
  return(dnorm(x,xi,sigma))
}

fn<-function(x,sigma,vx){
  n=length(vx)
  r=0
  for(e in vx){
    r=r+K(x,e,sigma)
  }
  return(r/n)
}

testStatisticNormalFull<-function(x,sigma){
  n=length(x)
  r1=testStatisticNormalU(x,sigma)
  
  r2=0
  for (i in c(1:n)){
    r2=r2+s1(x[i],x[i],sigma)-2*s2(x[i],sigma)+s3(sigma)
  }
  
  r2=r2/(n*(n-1))
  
  return(r1+r2)
}

