s1<-function(x1,x2,h){
  delta=abs(x1-x2)
  if (delta>=h) return (0)
  return ((h-delta)/(h*h))
}

s11<-function(h){
  return (1/h)
}

s2<-function(x,h){
  if (x<0){
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
  else if (x<=1){
    r=1-x+h/2-((1-x)^2)/(2*h) 
    r=r/h
    return(r)
  }
  else {
    return(0)
  }
}

s3<-function(h){
  return(1-h/3)
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

testStatisticUniform2<-function(x,h){
  n=length(x)
  r=testStatisticUniform1(x,h)*n*(n-1)
  
  for (i in c(1:n)){
    r=r+s11(h)-2*s2(x[i],h)+s3(h)
  }
  
  r=r/(n*(n-1))
  
  return(r)
}

testStatisticUniformU<-function(x,h){
  n=length(x)
  r1=n*(n-1)*s3(h)/2
    
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
  
  r=r1-(n-1)*r2+r3
  r=2*r/(n*(n-1))
  
  return(r)
}

fK<-function(x,h){
  if (x<=-h/2){
    return(0)
  }
  else if (x<=h/2){
    return((x+h/2)/h) 
  }
  else if (x<=(1-h/2)){
    return(1)
  }
  else if (x<=(1+h/2)){
    return((1-x+h/2)/h)
  }
  else{
    return(0)
  }
}

K<-function(x,xi,h){
  if (-h/2<=(x-xi)){
    if((x-xi)<=h/2){
      return(1/h)
    }
  }
  return(0)
}

fn<-function(x,h,vx){
  n=length(vx)
  r=0
  for(e in vx){
    r=r+K(x,e,h)
  }
  return(r/n)
}

testStatisticUniformFull<-function(x,h){
  n=length(x)
  r1=n*(n-1)*s3(h)/2
  
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
  
  r=r1-(n-1)*r2+r3
  r=2*r
  
  for (i in c(1:n)){
    r=r+s11(h)-2*s2(x[i],h)+s3(h)
  }
  
  r=r/(n*(n-1))
  
  return(r)
}

