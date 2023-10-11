s1<-function(x1,x2,h){
  delta=abs(x1-x2)
  if (delta>=h) return (0)
  return ((h-delta)/(h*h))
}

s2<-function(x,h){
  if (x<=0){
    return(0)
  }
  else if (x<h){
    r=x+h/2-(x*x)/(2*h)
    r=r/h
    return(r)
  }
  else if (x<(1-h)){
    return (1)
  }
  else if (x<1){
    r=1-x+h/2-((1-x)^2)/(2*h) 
    r=r/h
    return(r)
  }
  else {
    return(0)
  }
}

s3<-function(h){
  return(h/3)
}

s<-function(x1,x2,h){
  return(s1(x1,x2,h)-s2(x1,h)-s2(x2,h)+s3(h))
}

testStatisticUniform1<-function(x,h){
  n=length(x)
  r=0
  for (i in c(2:n)){
    for (j in c(1:(i-1))){
      r=r+s(x[i],x[j],h)
    }
  }
  r=2*r/(n*(n-1))
  return(r)
}

testStatisticUniform<-function(x,h){
  n=length(x)
  r1=n*(n-1)*s3(h)
    
  r2=0
  for (i in c(1:n)){
    r2=r2+s2(x[i],h)
  }
  
  r3=0
  for (i in c(2:n)){
    for (j in c(1:(i-1))){
      r3=r3+s1(x[i],x[j],h)
    }
  }
  
  r=r1-2*(n-1)*r2+r3
  r=r/(2*n*(n-1))
  
  return(r)
}
