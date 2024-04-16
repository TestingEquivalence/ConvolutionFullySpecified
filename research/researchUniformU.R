source("testStatisticUniform.R")
source("asymptoticTestBootstrapVariance.R")
source("BootstrapTestTPercentile.R")
source("simulation/size.R")
source("simulation/power.R")
source("distributions/alternatives.R")
source("distributions/mixedDistribution.R")
# library(tictoc)


# study the test power at the uniform distribution on [0,1]
sampleSize=100
vh=seq(0.1, 0.5, by=0.1)
resPower=list()

for (h in vh){
  parameter=list()
  parameter$alpha=0.05
  parameter$h=h
  parameter$nSimulation=50
  parameter$nSimPercentileTBootstrap=200
  parameter$f=testStatisticUniformU
  
  test<-function(x){
    parameter$x=x
    # tres=asymptoticTestBootstrapVariance(parameter)
    tres=tPercentileBootstrapTest(parameter)
    return(tres$min_eps)
  }
  
  # simulate power at uniform distribution
  
  resPower[[paste0("h",h)]]=simulatePowerAtPoint(test, sampleSize, nSimulation = 1000, rDistr = runif)
}

write.csv(resPower,"resPower_tPBT_200_50.csv")

# compute distance to boundary base points

lrdg=listRDG()
vi=c(1:length(lrdg))
vmean=rep(0,length(lrdg))
vsd=rep(0,length(lrdg))

h=0.35
n=10000

for (i in vi){
  rf=lrdg[[i]]
  vdst=distFunU(rf,h,n)
  vmean[i]=mean(vdst)
  vsd[i]=sd(vdst)
  print(i)
}

resDistance=data.frame(i=vi, mean=vmean, sd=vsd)
write.csv(resDistance,"resDistance.csv")

# simulate power at the boundary point

i=1
lrdg=listRDG()
rf=lrdg[[i]]
h=0.35
n=10000
epsilon=0.070
sampleSize=100

distFun<-function(rf){
  return(mean(distFunU(rf,h,n)))
}

bp=boundaryPoint(epsilon,rf,distFun)

checkDst=distFun(bp$fMixed)

parameter=list()
parameter$alpha=0.05
parameter$h=h
parameter$nSimulation=1000
parameter$f=testStatisticUniformU

test<-function(x){
  parameter$x=x
  tres=asymptoticTestBootstrapVariance(parameter)
  return(tres$min_eps)
}

resPower=simulatePowerAtPoint(test, sampleSize, nSimulation = 1000, rDistr = bp$fMixed)
v=resPower[resPower<checkDst]
nPower=length(v)/length(resPower)
write.csv(nPower, "nPower.csv")