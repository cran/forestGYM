Vres<-function(x){
  res_growth<-rbind(do.call(rbind,lapply(x[1:(length(x)-1)],function(x) x[-nrow(x),])),
                    x[[length(x)]])
  res_total<-rbind(x[[1]],do.call(rbind,lapply(x[2:length(x)],function(x) x[-1,])))
  res_harvest<-data.frame(year=res_total[,1],G=res_total$G-res_growth$G,
                          M=res_total$M-res_growth$M)
  Total=res_growth[nrow(res_growth),]
  Total$G=Total$G+sum(res_harvest$G)
  Total$M=Total$M+sum(res_harvest$M)
  list(Growth=res_growth,Harvest=res_harvest,Total=Total)
}
