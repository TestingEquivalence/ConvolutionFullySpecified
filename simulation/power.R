listRDG<-function(){
  ls=list()
  
  ls[[1]]<-function(x){
    rbeta(x,0.5,0.5)
  } 
  
  ls[[2]]<-function(x){
    rbeta(x,0.5,1)
  } 
  
  ls[[3]]<-function(x){
    rbeta(x,0.5,1.5)
  } 
  
  ls[[4]]<-function(x){
    rbeta(x,0.5,2)
  } 
  
  ls[[5]]<-function(x){
    rbeta(x,1,1.5)
  } 
  
  ls[[6]]<-function(x){
    rbeta(x,1,2)
  } 
  
  ls[[7]]<-function(x){
    rbeta(x,1.5,1.5)
  } 
  
  ls[[8]]<-function(x){
    rbeta(x,1.5,2)
  } 
  
  ls[[9]]<-function(x){
    rbeta(x,2,2)
  } 
  
  ls[[10]]<-function(x){
    rA(x,0.25)
  }
  
  ls[[11]]<-function(x){
    rA(x,0.5)
  }
  
  ls[[12]]<-function(x){
    rA(x,1.5)
  }
  
  ls[[13]]<-function(x){
    rA(x,2)
  }
  
  ls[[14]]<-function(x){
    rA(x,2.5)
  }
  
  ls[[15]]<-function(x){
    rA(x,3)
  }
  
  ls[[16]]<-function(x){
    rB(x,0.25)
  }
  
  ls[[17]]<-function(x){
    rB(x,0.5)
  }
  
  ls[[18]]<-function(x){
    rB(x,1.5)
  }
  
  ls[[19]]<-function(x){
    rB(x,2)
  }
  
  ls[[20]]<-function(x){
    rB(x,2.5)
  }
  
  ls[[21]]<-function(x){
    rB(x,3)
  }
  
  ls[[22]]<-function(x){
    rC(x,0.25)
  }
  
  ls[[23]]<-function(x){
    rC(x,0.5)
  }
  
  ls[[24]]<-function(x){
    rC(x,1.5)
  }
  
  ls[[25]]<-function(x){
    rC(x,2)
  }
  
  ls[[26]]<-function(x){
    rC(x,2.5)
  }
  
  ls[[27]]<-function(x){
    rC(x,3)
  }
  return(ls)
}

