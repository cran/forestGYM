\name{increment}
\alias{increment}
\title{
Calculation of annal and mean increment of stand volume.
}
\description{
Calculation of annal and mean increment of stand volume based on growth dynamic data of stand volume
}
\usage{
increment(Vpre)
}
\arguments{
  \item{Vpre}{
Growth dynamic data of stand volume, the data format is the data.frame.
 }
}
\details{
Growth dynamic data of stand volume, the data format is the data.frame.
}
\value{
Data included the annal and mean increment of stand volume.
}
\author{
Zongzheng Chai, chaizz@126.com
}
\references{
NULL
}
\examples{
Vdyn<-estV(b0=2.0137,b1=0.0795,b2=-16.9509,b3=0.7924,
     a0=1.1656,a1=0.1376,
     B1=3.1,t1=10,t2=11:100,SI=12)
increment(Vpre=Vdyn$Value)
}
