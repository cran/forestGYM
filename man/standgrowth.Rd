\name{standgrowth}
\alias{standgrowth}
\title{
Data for construction of stand growth model.
}
\description{
The forest survey data of two periods typically contain valuable information for analyzing forest growth and changes.
}
\usage{data("standgrowth")}
\format{
  A data frame with 330 observations on the following 16 variables from the forest survey data of two periods
  \describe{
    \item{\code{plot}}{Id of forest plot.}
    \item{\code{SI}}{Site index}
    \item{\code{t1}}{Time period 1, the unit is year.}
    \item{\code{D1}}{Average DBH in t1, the unit is cm.}
    \item{\code{H1}}{Average tree height in t1, the unit is m.}
    \item{\code{DH1}}{Top height in t1, the unit is m.}
    \item{\code{N1}}{Stand density in t1, the unit is N/ha.}
    \item{\code{G1}}{Basal area in t1, the unit is m2/ha.}
    \item{\code{M1}}{Volume in t1, the unit is m3/ha.}
    \item{\code{t2}}{Time period 2, the unit is year.}
    \item{\code{D2}}{Average DBH in t2, the unit is cm.}
    \item{\code{H2}}{Average tree height in t2, the unit is m.}
    \item{\code{DH2}}{Top height in t2, the unit is m.}
    \item{\code{N2}}{Stand density in t2, the unit is N/ha.}
    \item{\code{G2}}{Basal area in t2, the unit is m2/ha.}
    \item{\code{M2}}{Volume in t2, the unit is m3/ha.}
  }
}
\details{
The forest survey data of two periods typically contain valuable information for analyzing forest growth and changes.
}
\author{
Zongzheng Chai, chaizz@126.com
}
\examples{
data(standgrowth)
standgrowth
}
