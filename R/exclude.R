exclude<-function(data){
  db.null<-data
  db.exclude<-data
  for(i in 1:dim(data)[1]){
    for(l in 1:dim(data)[2]){
      if(is.na(data[i,l])==T){db.null[i,l]<-0
      }else(db.null[i,l]<-1)
    }
  }
  for(i in 1:dim(data)[1]){
    if(sum(db.null[i,])==0){db.exclude<-data[-i,]}
  }
  return(db.exclude)
}