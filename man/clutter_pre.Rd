\name{clutter_pre}
\alias{clutter_pre}
\title{
Data summary for stand growth prediction of Clutter model integrating simulated logging.
}
\description{
At the determined final harvest period, through the setting of different logging periods and the determination of logging intensities for different cutting periods, the Clutter model is used to realize stand growth prediction.
}
\usage{
clutter_pre(b0,b1,b2,b3,a0,a1,
                   B1,SI,t1,growth_years,
                   thinning_years,thinning_intensity)
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
  \item{thinning_years}{
Different logging periods,the value is between t1 and growth_years,the unit is year.
 }
  \item{thinning_intensity}{
Logging intensities corresponding to the thinning_years,the value is betwee 0 and 1.
 }
}
\details{
Both growth_years and thinning_years should be integers, the value of thinning_years is between t1 and growth_years,the unit is year.
}
\value{
The returned data format is a list, data summary for stand growth prediction of Clutter model integrating simulated logging.
}
\author{
Zongzheng Chai, chaizz@126.com
}
\references{
Clutter, J. L. (1963) Compatible Growth For Loblolly by the Southeastern, Forest Science, 9(3), pp. 354–371. Sullivan, A. D. and Clutter, J. L. (1972) A Simultaneous Growth and Yield for Loblolly Pine, Forest Science, 18(1), pp. 76–86.
}
\examples{
clutter_simulation(b0=2.0137,b1=0.0795,b2=-16.9509,b3=0.7924,
                   a0=1.1656,a1=0.1376,
                   B1=3.1,SI=12,t1=10,growth_years=30,
                   thinning_years=c(15,25),thinning_intensity=c(0.1,0.5))
}
