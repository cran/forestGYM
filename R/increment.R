increment<-function(Vpre){
  M.df=Vpre
  M.df$mean=M.df$M/M.df$year
  xy<-list()
  for(i in 2:length(M.df$year)){
    xy[[1]]<-NA
    xy[[i]]<-M.df$M[i]-M.df$M[i-1]
  }
  M.df$annal<-unlist(xy)
  return(M.df)
}