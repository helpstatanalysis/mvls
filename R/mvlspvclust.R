#' mvls.pvclust
#'
#' Hierical cluster control with pvclust application
#'
#' @author Lorenzo Querci <lorenzo.querci@studio.unibo.it>
#'
#' @param mvls A mvls object after h cluster analysis
#' @param data Type of data analysis. "data" for analysis on real data after imputtion or "var.matrix" for analysis on vari.matrix.
#' @param nboot Number of boot operation.
#'
#' @return $pvclust It's a pvclust object.
#'
#' @import "pvclust"
#'
#' @export

mvls.pvclust<-function(mvls, data="data",nboot=1000){
  if(data=="data"){
    result<-pvclust(mvls$data, method.hclust="average",method.dist="correlation",use.cor="pairwise.complete.obs",nboot)
  }else if(data=="vari.matrix"){
    result<-pvclust(mvls$vari.matrix, method.hclust="average",method.dist="correlation",use.cor="pairwise.complete.obs",nboot)
  }
  return(result)
}
