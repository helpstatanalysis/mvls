#' mvlsboot
#'
#' Takes a longitudinal dataset and impute missing value with a machine learning-based method many time.
#'
#' @author Lorenzo Querci <lorenzo.querci@studio.unibo.it>
#'
#' @param data A dataset (more than two longitudinal mesurements)
#' @param d It is the percentage of change between two-sided mesurements to consider it bigger, smaller or the same, It useful to built the var.matrix
#' @param method It represent the type of machine learning algorithm. 'k' for k-mean and 'h' for hierical
#' @param cluster It's the number of cluster. Default setting it's 6. It depends on number of longitudinal mesurements. It could be use mvls.print to decide best cluster number.
#' @param nstart It is the nstart setting of function k-mean. Defualt it'20. Not requested for 'h' method.
#' @param pre.imp TRUE/FALSE (default F). It permit to pre-impute data to built the vari.matrix, It could be reduce cluster with only missing value.
#' @param imp.method It's the type of pre-imputation. Defaul it's 'mean', but there is also 'locf' possibility
#' @param boot Impute many times to reduce error from cluster imputation. 'low' for 5 imputation, 'medium' for 10 imputation and 'high' for 15 imputation.
#'
#' @return $data It's the data-set with imputation.
#' @return $sd.2 It contain the sd for each data imputed at multiple imputation method. Different from sd.1.
#' @return $db.boot It's a list of all imputed dataset (n=boot).
#'
#' @import "zoo"
#' @import "mice"
#'
#' @export

mvlsboot<-function(data, d=0.1, method='k', cluster=6, nstart=20, pre.imp=F, imp.method='mean', boot=10){
  sd.2<-matrix(c(rep(NA,(dim(data)[2]*dim(data)[1]))),ncol=dim(data)[2],nrow=dim(data)[1])
  result.boot<-data
  db.boot<-replicate(boot, mvls(data, d, method, cluster, nstart,pre.imp, imp.method)$data, simplify = "array")
  for (i in 1:dim(result.boot)[2]){
    for (l in 1:dim(result.boot)[1]) {
      sd.2[l,i]<-rnorm(1,mean=0, sd=sd(db.boot[l, i, 1:boot], na.rm = T))
      result.boot[l,i]<-mean(db.boot[l, i, 1:boot],na.rm = T)+sd.2[l,i]
      }
    }
  return(list(data=result.boot,
              sd.2=sd.2,
              db.boot=db.boot))
}
