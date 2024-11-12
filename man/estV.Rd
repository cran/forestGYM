\name{estV}
\alias{estV}
\title{
Estimation of stand volume growth dynamic based on Clutter model.
}
\description{
The dynamic prediction of stand volume in a specified prediction year is based on the Clutter model.
}
\usage{
estV(b0,b1,b2,b3,a0,a1,B1,t1,t2,SI)
}
\arguments{
  \item{b0}{
Regression coefficients of Clutter model.
 }
  \item{b1}{
Regression coefficients of Clutter model.
 }
  \item{b2}{
Regression coefficients of Clutter model.
 }
  \item{b3}{
Regression coefficients of Clutter model.
 }
  \item{a0}{
Regression coefficients of Clutter model.
 }
  \item{a1}{
Regression coefficients of Clutter model.
 }
  \item{SI}{
Site index
 }
  \item{t1}{
Initial stand age,the unit is year.
 }
  \item{t2}{
Stand age in the future period corresponding to volume prediction,the unit is year.
 }
   \item{B1}{
Basal area in t1, the unit is m2/ha.
 }
}
\details{
Both t1 and t2 should be integers, the value of t2 should be bigger than t1,the unit is year.
}
\value{
prediction results of stand volume in a specified prediction year is based on the Clutter model.
}
\author{
Zongzheng Chai, chaizz@126.com
}
\references{
Clutter, J. L. (1963) Compatible Growth For Loblolly by the Southeastern, Forest Science, 9(3), pp. 354–371. Sullivan, A. D. and Clutter, J. L. (1972) A Simultaneous Growth and Yield for Loblolly Pine, Forest Science, 18(1), pp. 76–86.
}
\examples{
#Volume prediction for a specific year.
estV(b0=2.0137,b1=0.0795,b2=-16.9509,b3=0.7924,
     a0=1.1656,a1=0.1376,
     B1=3.1,t1=10,t2=100,SI=12)

#Volume prediction for several specific years.
estV(b0=2.0137,b1=0.0795,b2=-16.9509,b3=0.7924,
     a0=1.1656,a1=0.1376,
     B1=3.1,t1=10,t2=c(15,30,46,85),SI=12)

#Volume prediction for continuous years.
estV(b0=2.0137,b1=0.0795,b2=-16.9509,b3=0.7924,
            a0=1.1656,a1=0.1376,
            B1=3.1,t1=10,t2=11:100,SI=12)
}
