
simulatePowerAtPoint<-function(test, sampleSize, nSimulation,rDistr,orderName="temp"){
  #prepare
  if(!dir.exists(orderName)){
    dir.create(orderName)
  }
  
  
  set.seed(10071977)
  sim=list()
  for (i in c(1:nSimulation)){
    sim[[i]]=rDistr(sampleSize)
  }
  
  res=rep(0,nSimulation)
  for (i in c(1:nSimulation)){
    fname=paste0("r",i,".rds")
    fname=file.path(orderName,fname)
    
    if (file.exists(fname)){
      res[i]=readRDS(fname)
      print(i)
    }
    else {
      res[i]=test(sim[[i]])
      saveRDS(res[i],fname)
      print(i)
    }
  }

  unlink(orderName,recursive = TRUE)
  return(res)
}
