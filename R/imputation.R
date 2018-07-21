imputation<-function(data, vari.matrix, method='k', cluster=6, nstart=20){
  if(method=='k'){
    clusters <- kmeans(vari.matrix, cluster, nstart)
    clusterCut <- clusters$cluster
  }else if(method=='h'){
    clusters <- hclust(dist(vari.matrix))
    clusterCut <- cutree(clusters, cluster)
  }
  clu.matrix<-matrix(rep(clusterCut,dim(data)[2]),ncol =(dim(data)[2]))
  sd.1<-matrix(c(rep(NA,(dim(data)[2]*dim(data)[1]))),ncol=dim(data)[2],nrow=dim(data)[1])
  for(i in 1:dim(data)[2]){
    for(l in 1:dim(data)[1]){
      if(is.na(data[l,i])==T){
        NonNAindex <- which(!is.na(data[l,]))
        index<-which.min(abs(NonNAindex - i))
        index<-NonNAindex[index]
        val<-as.vector(as.matrix(which(clusterCut==clusterCut[l]))[,1])
        vector.na<-data[val,i]
        vector.notna<-data[val,index]
        dif<-vector.na-vector.notna
        if(length(!is.na(dif)[!is.na(dif) == TRUE])==0 || length(!is.na(dif)[!is.na(dif) == TRUE])==1){
          cat("Error 1")
        }else{
          sd.1[l,i]<-rnorm(1, mean=0, sd=sd(dif,na.rm = T))
          data[l,i]<-data[l,index]+median(dif, na.rm = T)+sd.1[l,i]
        }
      }
    }
  }
  return(list(data=data, 
              clu.matrix=clu.matrix, 
              sd.1=sd.1))
}
