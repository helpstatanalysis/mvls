normalize <- function(data){
  index<-NULL
  for (i in 1:dim(data)[1]){
    index[i]<-max(data[i,], na.rm = T)
    data[i,]<-data[i,]/max(data[i,], na.rm = T)
  }
  return(list(data=data,index=index))
}
