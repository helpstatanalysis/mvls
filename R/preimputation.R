preimputation<-function(data, imp.method='mean'){
  if(imp.method=='mean'){
    data<-mice(data, method = 'mean', printFlag = F)
    datapreimput<-complete(data)
  }else if(imp.method=='locf'){
    data<-t(data)
    data<-na.locf(data)
    data<-t(data)
    data<-mice(data, method = 'mean', printFlag = F)
    datapreimput<-complete(data)
  }else{cat("Error. pre-imputation method not correct")}
  return(datapreimput)
}
