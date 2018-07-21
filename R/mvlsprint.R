#' mvlsprint
#'
#' Takes a longitudinal dataset and give you graphic plot to choose number of cluster
#'
#' @author Lorenzo Querci <lorenzo.querci@studio.unibo.it>
#'
#' @param data A dataset (more than two longitudinal mesurements)
#' @param d percentage of change between two-sided mesurements to consider it bigger, smaller or the same, It useful to built the var.matrix
#' @param method it represent the type of machine learning algorithm. 'k' for k-mean and 'h' for hierical
#' @param kmax It's the maximum number of cluster for k-mean. Default setting it's 10. It's the elbow k-mean method.
#' @param varmatrix TRUE/FALSE value (default it's F). It permit to see cluster for var.matrix
#'
#' @return Plot
#'
#' @import "randomForestSRC"
#'
#' @export

mvls.print<-function(data, d, method='h', varmatrix=F, kmax=10){
  if(method=='h' && varmatrix==F){
    data<-exclude(data)
    clusters <- hclust(dist(data))
    return(plot(clusters))
  } else if(method=='h' && varmatrix==T){
    data<-toclusterfunc.noimp(data,d)$vari.matrix
    clusters <- hclust(dist(data))
    return(plot(clusters))
  }else if (method=='k' && varmatrix==F){
    wss <- sapply(1:kmax, function(k){kmeans(na.omit(data), k, nstart=50,iter.max = 15 )$tot.withinss})
    plot(1:kmax, wss, type="b", pch = 19, frame = FALSE,
         xlab="Number of clusters K",
         ylab="Total within-clusters sum of squares")
  }else if (method=='k' && varmatrix==T){
    data<-toclusterfunc.noimp(data,d)$vari.matrix
    wss <- sapply(1:kmax, function(k){kmeans(na.omit(data), k, nstart=50,iter.max = 15 )$tot.withinss})
    plot(1:kmax, wss, type="b", pch = 19, frame = FALSE,
         xlab="Number of clusters K",
         ylab="Total within-clusters sum of squares")
  }else{cat('Error: only "k" method and "h" method permitted')}
}
