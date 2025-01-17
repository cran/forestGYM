clutter_mod<-function(growthdata,object="coef"){
  t1=growthdata$t1
  t2=growthdata$t2
  G1=growthdata$G1
  G2=growthdata$G2
  M1=growthdata$M1
  M2=growthdata$M2
  SI=growthdata$SI
  x1=SI
  x2=(1/t2)
  x3=((t1/t2)*log(G1))
  x4=(1-t1/t2)
  x5=(SI*(1-t1/t2))
  lmmod<-stats::lm(log(M2)~x1+x2+x3+x4+x5,data=growthdata)
  coef<-coef(lmmod)
  names(coef)<-c("b0","b1","b2","b3","a0","a1")
  list(Coef=coef,
       Summary=summary(lmmod),
       Variables=c("x1=SI","x2=(1/t2)",
                   "x3=((t1/t2)*log(G1))",
                   "x4=(1-t1/t2)",
                   "x5=(SI*(1-t1/t2))"),
       Object=match.fun(object)(lmmod))
}
