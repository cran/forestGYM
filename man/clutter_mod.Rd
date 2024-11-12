\name{clutter_mod}
\alias{clutter_mod}
\title{
Construction of stand growth model based on Clutter model.
}
\description{
Construction of stand growth model based on Clutter model.
}
\usage{
clutter_mod(growthdata,object="coef")
}
\arguments{
  \item{growthdata}{
The data used to construct the stand growth model is in the format of data.frame and includes at least t1, t2, G1, G2, M1, M2, and SI. For specific meanings, see standgrowth.
 }
   \item{object}{
object is a type of fitted model object. It has methods for the generic functions anova, coef, confint, deviance, df.residual, fitted, formula, logLik, predict, print, profile, residuals, summary, vcov and weights.see Details of nls function.
 }
}
\details{
Construction of stand growth model based on Clutter model.
}
\value{
The returned data format is a list, data summary for Clutter model.
}
\author{
Zongzheng Chai, chaizz@126.com
}
\references{
Clutter, J. L. (1963) Compatible Growth For Loblolly by the Southeastern, Forest Science, 9(3), pp. 354–371. Sullivan, A. D. and Clutter, J. L. (1972) A Simultaneous Growth and Yield for Loblolly Pine, Forest Science, 18(1), pp. 76–86.
}
\examples{
data(standgrowth)
clutter_mod(growthdata=standgrowth,object="coef")
}
