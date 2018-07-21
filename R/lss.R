lss<-function(n,time, mean, sd, sd.e,perc.up,perc.zero,perc.down, set.seed=NULL){
  set.seed(set.seed)
  if(perc.up+perc.zero+perc.down==1){
    #Zero trend
    n.zero= round(n*perc.zero)  # Number of record
    time  # Number of temporal mesurements
    base=round(rep(rnorm(n.zero, mean, sd)),digits = 2)
    y=round(base + rnorm(n.zero*time, mean=0, sd.e))
    data.z=matrix(y, ncol=time)
    #Positive trend
    n.positive= round(n*perc.up)  # Number of record
    time  # Number of temporal mesurements
    base=round(rnorm(n.positive, mean, sd),digits = 2)
    base.1=base
    for(i in 2:time){base.1<-c(base.1,base*(i))}
    y=round(base.1 + rnorm(n.positive*time, mean=0, sd.e))
    data.p=matrix(y, ncol=time)
    #Negative trend
    n.negative=round(n*perc.down)  # Number of record
    time  # Number of temporal mesurements
    base=round(rnorm(n.negative, mean, sd),digits = 2)
    base.1=base
    for(i in 2:time){base.1<-c(base.1,base/(i))}
    y=round(base.1 + rnorm(n.negative*time, mean=0, sd.e))
    data.u=matrix(y, ncol=time)
  }else(cat("Error. Total percentage is",perc.down+perc.up+perc.zero))
  return(rbind(data.z,data.p,data.u))
}
