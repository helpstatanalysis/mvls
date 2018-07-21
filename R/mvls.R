#' mvls
#'
#' Takes a longitudinal dataset and impute missing value with a machine learning-based method.
#'
#' @author Lorenzo Querci <lorenzo.querci@studio.unibo.it>
#'
#' @param data A dataset (more than two longitudinal mesurements)
#' @param d Percentage of change between two-sided mesurements to consider it bigger, smaller or the same, It useful to built the var.matrix.
#' @param method It represent the type of machine learning algorithm. 'k' for k-mean and 'h' for hierical.
#' @param cluster It's the number of cluster. Default setting it's 6. It depends on number of longitudinal mesurements. It could be use mvls.print to decide best cluster number.
#' @param nstart It is the nstart setting of function k-mean. Defualt it'20. Not requested for 'h' method.
#' @param pre.imp TRUE/FALSE (default F). It permit to pre-impute data to built the vari.matrix, It could be reduce cluster with only missing value.
#' @param imp.method It's the type of pre-imputation. Defaul it's 'mean', but there is also 'locf' possibility.
#'
#' @return $data It's the data-set with imputation.
#' @return $cluster It's the cluster matrix.
#' @return $matrix It's the vari.matrix.
#' @return $sd.1 It contains the sd for each data imputed at single imputation method. Different from sd.2.
#' @return $vari.matrix It's the variation matrix
#' @return $data.norm It's the imputation dataset normalized
#'
#' @import "zoo"
#' @import "mice"
#'
#' @export

mvls<-function(data, d=0.1, method='k', cluster=6, nstart=20, pre.imp=F, imp.method='mean'){
  if(pre.imp==T){
    results<-toclusterfunc.imp(data,d,imp.method)
  }else if(pre.imp==F){
    results<-toclusterfunc.noimp(data,d)
  }
  data.f<-results$data
  index.f<-results$index
  vari.matrix.f<-results$vari.matrix
  result<-imputation(data.f, vari.matrix.f, method, cluster, nstart)
  db<-result$data*index.f
  sd.1<-result$sd.1*index.f
  return(list(data=db, #Imputation dataset
              vari.matrix=vari.matrix.f, #Variation matrix
              data.norm=result$data, #Imputation dataset normalized
              cluster=result$clu.matrix, #Matrix of cluster
              matrix=results$matrix, #Data after exclusion, prior to nomrmalization
              sd.1=sd.1))
}
