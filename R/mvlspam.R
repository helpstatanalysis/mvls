#' mvls.pam
#'
#' Takes a longitudinal dataset and impute missing value with pam (k by pamk) machine learning-based method.
#'
#' @author Lorenzo Querci <lorenzo.querci@studio.unibo.it>
#'
#' @param data A dataset (more than two longitudinal mesurements).
#' @param imp.method Type od pre-imputation. pam and pamk don't use missing value. Possible "mean" or "locf".
#' @param krange Range for k in pamk (fpc package) function.
#' @param scaling TRUE/FALSE value (default T). Scale data before clustering.
#'
#' @return $data It's the data-set with imputation.
#' @return $pam It's a pam object
#' @return $sd.1 It contains the sd for each data imputed at single imputation method
#'
#' @import "fpc"
#' @import "cluster"
#' @import "mice"
#' @import "zoo"
#'
#' @export

mvls.pam<-function(data, imp.method='mean', krange=2:12, scaling=T){
  data<-exclude(data)
  data.pi<-preimputation(data,imp.method)
  k<-pamk(data.pi,krange, scaling)$nc
  pam<-pam(data.pi, k)
  pam$clustering
  clusterCut<-pam$clustering
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
        if(length(!is.na(dif)[!is.na(dif) == TRUE])==0 ||
           length(!is.na(dif)[!is.na(dif) == TRUE])==1){
          cat("Error 1: no sufficient memeber in the cluster. Change k.")
        }else{
          sd.1[l,i]<-rnorm(1, mean=0, sd=sd(dif,na.rm = T))
          data[l,i]<-data[l,index]+median(dif, na.rm = T)+sd.1[l,i]
        }
      }
    }
  }
  return(list(data=data, pam=pam, sd.1=sd.1))
}
