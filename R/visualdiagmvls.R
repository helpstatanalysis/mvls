#' visualdiagmvls
#'
#' Takes a mvls object and give a plot about cluster distribution
#'
#' @author Lorenzo Querci <lorenzo.querci@studio.unibo.it>
#'
#' @param mvls A object class mvls
#' @param method Indicate if all cluster plot together or single one (need to select it). "single" or "multiple"
#' @param cluster It's the number of clustet to plot. Number of pattern, max is the k decided for mvls function
#' @param norm Permit to plot from normalized data. TRUE/FALSE value
#'
#' @import "ggplot2"
#' @import "reshape2"
#'
#' @export

visualdiagmvls<-function(mvls, method="multiple", cluster, norm=F){
  if(method=="multiple"){
    if(norm=="TRUE"){matrix<-mvls$data.norm}else if(norm=="FALSE"){matrix<-mvls$matrix}
    Cluster<-as.character(as.vector(mvls$cluster[,1]))
    id<-seq(1,dim(matrix)[1], by=1)
    matrix<-data.frame(id,matrix,Cluster)
    matrix.ggplot<-reshape(na.omit(matrix),idvar ="id", varying=list(2:(dim(matrix)[2]-1)), direction = "long")
    ggplot(data=matrix.ggplot, aes(x=time, y=V1, colour=Cluster, group=id))+geom_line(alpha=.5)+ggtitle("Distribuzione dei pattern")+labs (x="Time", y = "Values")+theme_classic()
  }else if(method=="single"){
    if(norm=="TRUE"){matrix<-mvls$data.norm}else if(norm=="FALSE"){matrix<-mvls$matrix}
    Cluster<-as.vector(mvls$cluster[,1])
    data<-cbind(matrix,Cluster)
    data.1<-subset(data, data$Cluster == cluster)
    matrix.1<-data.1[,-dim(data.1)[2]]
    id<-seq(1,dim(matrix.1)[1], by=1)
    matrix.ggplot<-reshape(na.omit(matrix.1),idvar ="id", varying=list(1:(dim(matrix.1)[2])), direction = "long")
    ggplot(data=matrix.ggplot, aes(x=time, y=V1, group=id))+geom_line(alpha=.5)+ggtitle(paste0("Distribuzione del pattern",cluster))+labs (x="Time", y = "Values")+theme_classic()
  }
}

