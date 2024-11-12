\name{clutter_simopt}
\alias{clutter_simopt}
\title{
Stand growth prediction of Clutter model based on optimal logging.
}
\description{
Through the enumeration method, achieve the optimal volume growth based on independent simulated logging.
}
\usage{
clutter_simopt(b0,b1,b2,b3,a0,a1,
              B1,SI,t1,growth_years,
              times,smallest_interval,
              thinning_intensity)
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
   \item{B1}{
Basal area in t1, the unit is m2/ha.
 }
  \item{growth_years}{
The final logging period is the main cutting period of the stand,the unit is year.
 }
   \item{times}{
Logging times.
 }
   \item{smallest_interval}{
Smallest interval among Logging times (times).
 }
  \item{thinning_intensity}{
Range of logging intensities,the value is betwee 0 and 1.
 }
}
\details{
Through the enumeration method, achieve the optimal volume growth based on independent simulated logging.
}
\value{
The returned data format is a list, data summary for the optimal volume growth based on independent simulated logging.
}
\author{
Zongzheng Chai, chaizz@126.com
}
\references{
Clutter, J. L. (1963) Compatible Growth For Loblolly by the Southeastern, Forest Science, 9(3), pp. 354–371. Sullivan, A. D. and Clutter, J. L. (1972) A Simultaneous Growth and Yield for Loblolly Pine, Forest Science, 18(1), pp. 76–86.
}

\examples{
clutter_simopt(b0=2.0137,b1=0.0795,b2=-16.9509,b3=0.7924,
               a0=1.1656,a1=0.1376,
               B1=3.1,SI=12,t1=10,
               growth_years=30,
               times=2,smallest_interval=5,
               thinning_intensity=seq(0.1,0.3,0.1))
}
