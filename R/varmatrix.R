var.matrix<-function(data,d=.1){
  matrix.1<-data.frame(NULL)
  for (z in 1:(dim(data)[2]-1)){
    for (i in 1:dim(data)[1]){
      if(is.na(data[i,z])==T){
        matrix.1[i,z]<-3
      } else if(is.na(data[i,z+1])==T){
        matrix.1[i,z]<-3
      } else if(data[i,z+1]>(data[i,z]+d)){
        matrix.1[i,z]<-1
      } else if(data[i,z+1]<(data[i,z]-d)){
        matrix.1[i,z]<-2
      } else{matrix.1[i,z]<-0
      }
    }
  }
  matrix.2<-data.frame(NULL)
  for (z in 1:(dim(data)[2]-2)){
    for (i in 1:dim(data)[1]){
      if(is.na(data[i,z])==T){
        matrix.2[i,z]<-3
      } else if(is.na(data[i,z+2])==T){
        matrix.2[i,z]<-3
      } else if(data[i,z+2]>(data[i,z]+d)){
        matrix.2[i,z]<-1
      } else if(data[i,z+2]<(data[i,z]-d)){
        matrix.2[i,z]<-2
      } else{matrix.2[i,z]<-0
      }
    }
  }
  matrix.3<-data.frame(NULL)
  for (i in 1:dim(data)[1]){
    if(is.na(data[i,1])==T){
      matrix.3[i,1]<-3
    } else if(is.na(data[i,dim(data)[2]])==T){
      matrix.3[i,1]<-3
    } else if(data[i,dim(data)[2]]>(data[i,1]+d)){
      matrix.3[i,1]<-1
    } else if(data[i,dim(data)[2]]<(data[i,1]-d)){
      matrix.3[i,1]<-2
    } else{matrix.3[i,1]<-0
    }
  }
  matrix<-cbind(matrix.1,matrix.2,matrix.3)
  return(matrix)
}
