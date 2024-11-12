clutter_simulation<-function(b0,b1,b2,b3,a0,a1,
                             B1,SI,t1,growth_years,
                             thinning_years,thinning_intensity){
  V<-list()
  if(length(thinning_years)==1){
    V[[1]]=estV(b0=b0,b1=b1,b2=b2,b3=b3,a0=a0,a1=a1,
                B1=B1,SI=SI,t1=t1,t2=c((t1+1):thinning_years))$Value
    V[[2]]<-estV(b0=b0,b1=b1,b2=b2,b3=b3,a0=a0,a1=a1,
                 B1=V[[1]]$G[nrow(V[[1]])]*(1-thinning_intensity[1]),
                 SI=SI,t1=thinning_years[length(thinning_years)],
                 t2=c((thinning_years[length(thinning_years)]+1):growth_years))$Value
  }else if(length(thinning_years)>1){
    for(i in 1:(length(thinning_years)-1)){
      V[[1]]<-estV(b0=b0,b1=b1,b2=b2,b3=b3,a0=a0,a1=a1,
                   B1=B1,SI=SI,t1=t1,t2=c((t1+1):thinning_years[1]))$Value
      V[[i+1]]<-estV(b0=b0,b1=b1,b2=b2,b3=b3,a0=a0,a1=a1,
                     B1=V[[i]]$G[V[[i]]$year==thinning_years[i]]*(1-thinning_intensity[i]),
                     SI=SI,t1=thinning_years[i],
                     t2=c((thinning_years[i]+1):thinning_years[i+1]))$Value
    }
    V[[length(thinning_years)+1]]<-estV(b0=b0,b1=b1,b2=b2,b3=b3,a0=a0,a1=a1,
                                        B1=V[[length(thinning_years)]]$G[V[[length(thinning_years)]]$year==thinning_years[length(thinning_years)]]*(1-thinning_intensity[i]),
                                        SI=SI,t1=thinning_years[length(thinning_years)],
                                        t2=c((thinning_years[length(thinning_years)]+1):growth_years))$Value
  }
  return(V)
}




