clutter_simopt<-function(b0,b1,b2,b3,a0,a1,
                         B1,SI,t1,growth_years,
                         times,smallest_interval,
                         thinning_intensity){
  thinning_years=c((t1+1):(growth_years-1))
  interval<-function(x,smallest_interval=3){
    fun<-function(x){
      xy<-list()
      for(i in 2:length(x)){
        xy[[1]]=x[1]
        xy[[i]]=x[i]-x[i-1]

      }
      return(unlist(xy)[-1])
    }
    xy<-list()
    for(i in 1:nrow(x)){
      xy[[i]]<-fun(x[i,])
    }
    res<-do.call(rbind,xy)
    xy1<-list()
    for(i in 1:nrow(res)){
      xy1[[i]]<-which(all(res[i,]>=smallest_interval))
    }
    return(x[which(xy1==1),])
  }
  df.thinning_years <- interval(gtools::combinations(length(thinning_years), times, thinning_years),
                                smallest_interval=smallest_interval)
  df.thinning_intensity<- expand.grid(rep(list(thinning_intensity),times))
  combined_df<-function(x,y){
    xy<-list()
    for(i in 1:nrow(x)){
      xy[[i]]<-cbind(do.call(rbind,rep(list(t(as.matrix(x[i,]))),
                                       nrow(y))),y)
    }
    xy1<-as.data.frame(do.call(rbind,xy))
    return(xy1)
  }
  samres<-combined_df(df.thinning_years,df.thinning_intensity)
  res_total<-list()
  for(i in 1:nrow(samres)){
    res_total[[i]]<-Vres(clutter_simulation(b0=b0,b1=b1,b2=b2,b3=b3,a0=a0,a1=a1,
                                            B1=B1,SI=SI,t1=t1,growth_years=growth_years,
                                            thinning_years=unlist(samres[i,1:times]),
                                            thinning_intensity=unlist(samres[i,(times+1):ncol(samres)])))$Total
  }
  opt_year_intensity<-samres[order(do.call(rbind,res_total)$M,decreasing=TRUE)[1],]
  opt_years<-unlist(opt_year_intensity[1:times])
  names(opt_years)<-paste("year",1:length(opt_years),sep="")
  opt_intensity<-unlist(opt_year_intensity[(times+1):length(opt_year_intensity)])
  names(opt_intensity)<-paste("intensity",1:length(opt_intensity),sep="")
  res_sim<-Vres(clutter_simulation(b0=b0,b1=b1,b2=b2,b3=b3,a0=a0,a1=a1,
                                   B1=B1,SI=SI,t1=t1,growth_years=growth_years,
                                   thinning_years=opt_years,
                                   thinning_intensity=opt_intensity))
  rownames(res_sim$Growth)<-1:nrow(res_sim$Growth)
  res_control<-Vres(clutter_simulation(b0=b0,b1=b1,b2=b2,b3=b3,a0=a0,a1=a1,
                                       B1=B1,SI=SI,t1=t1,growth_years=growth_years,
                                       thinning_years=opt_years,
                                       thinning_intensity=rep(0,length=length(opt_years))))
  rownames(res_control$Growth)<-1:nrow(res_control$Growth)
  Comparison<-rbind(res_control$Total,res_sim$Total)
  row.names(Comparison)<-c("Control","Optimal")
  list(
    Opt_years=opt_years,
    Opt_intensity=opt_intensity,
    Comparison=Comparison,
    Opt_sim=res_sim,
    Opt_control=res_control)
}

