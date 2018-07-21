toclusterfunc.noimp<-function(data, d){
  data<-exclude(data)
  matrix<-data
  result<-normalize(data)
  data<-result$data
  index<-result$index
  db.var<-var.matrix(data,d)
  result<-return(list(data=data,
                      index=index,
                      vari.matrix=db.var,
                      matrix=matrix))
}
