\name{Vres}
\alias{Vres}
\title{
Integrated results of clutter_simulation function.
}
\description{
Integrated results of clutter_simulation function.
}
\usage{
Vres(x)
}
\arguments{
  \item{x}{
Results of clutter_simulation function.
 }
}
\details{
Integrated results of clutter_simulation function and to make the data presentation more intuitive and easy to understand.
}
\value{
prediction results of stand volume prediction.
}
\author{
Zongzheng Chai, chaizz@126.com
}
\examples{
Vresult<-clutter_simulation(b0=2.0137,b1=0.0795,b2=-16.9509,b3=0.7924,
                       a0=1.1656,a1=0.1376,
                       B1=3.1,SI=12,t1=10,growth_years=30,
                       thinning_years=c(15,25),thinning_intensity=c(0.1,0.5))
Vres(Vresult)
}
