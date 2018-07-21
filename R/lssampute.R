#' lss.ampute
#'
#' Create and ampute a longitudinal dataset
#'
#' @author Lorenzo Querci <lorenzo.querci@studio.unibo.it>
#'
#' @param n It's the number of record (subjects).
#' @param time It's the number of longitudinal mesurements.
#' @param mean It's the mean for the first longitudinal mesurements.
#' @param sd It's the standard deviation for the first mesurment.
#' @param sd.e It's the error on single mesurements.
#' @param perc.up It's the percentage (0.00 to 1.00) of the positive trend longitudinal mesurements.
#' @param perc.zero It's the percentage (0.00 to 1.00) of the zero trend longitudinal mesurements.
#' @param perc.down It's the percentage (0.00 to 1.00) of the negative trend longitudinal mesurements.
#' @param perc.ampute It's the percentage of missing data (after amputation)
#' @param ampute It's the type of amputation methods (default it's "mcar"): "mcar", "mar" and "mnar" are allowed.
#' @param set.seed Default it's NULL, but it could be set as you prefer.
#'
#' @return $data It's the database with real values,
#' @return $data.ampute It's the database with missing data.
#'
#' @export

lss.ampute<-function(n, time, mean, sd, sd.e,perc.up, perc.zero, perc.down, perc.ampute, ampute="mcar", set.seed=NULL){
  if(ampute=="mcar"){
    data=lss(n,time, mean, sd, sd.e,perc.up,perc.zero,perc.down,set.seed)
    mcar=runif(dim(data)[1]*dim(data)[2], min=0, max=1)
    data.ampute=as.vector(data)
    data.ampute=ifelse(mcar<perc.ampute, NA, data)
    data.ampute=matrix(data.ampute,ncol=time)
  }else if(ampute=="mar"){
    data=lss(n,time, mean, sd, sd.e,perc.up,perc.zero,perc.down)
    data.ampute<-data
    for(i in 1:dim(data)[1]){
      for(j in 4:dim(data)[2]){
        dif1 = data[i,j-2]-data[i,j-3]
        dif2 = data[i,j-1]-data[i,j-2]
        if(dif1>0 && dif2>0){  # if weight goes up twice, drops out
          data.ampute[i,j:dim(data)[2]] = NA;  break
        }
      }
    }
  }else if(ampute=="mnar"){
    data=lss(n,time, mean, sd, sd.e,perc.up,perc.zero,perc.down)
    data.mnar<-as.vector(data)
    sort.data.mnar = sort(data.mnar, decreasing=TRUE)
    nmar   = sort.data.mnar[ceiling(perc.ampute*length(sort.data.mnar))] #Limit to NA
    data.ampute = ifelse(data.mnar>nmar, NA, data.mnar)  # doesn't show up when heavier
    data.ampute=matrix(data.ampute,ncol=time)
  }
  return(list(data=data, data.ampute=data.ampute))
}
