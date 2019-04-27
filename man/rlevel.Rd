\name{rlevel}
\alias{rlevel}
\title{Changing the Reference Group without Re-Running the Model}
\description{To re-calculate the coefficients and the standard deviation when changing the reference group.}
\usage{
rlevel(fit,var,oldrf,newrf,cl=NA)
}
\arguments{
  \item{fit}{Fitted model using glm, lm, glmnet or cv.glmnet}
  \item{var}{Vector of variable name in the model you plan to change reference}
  \item{oldrf}{Vector of old reference group name}
  \item{newrf}{Vector of new reference group name}
  \item{cl}{Column number of coefficient, only available when using glmnet or cv.glmnet}
}
\details{Make sure the name of variables, name of old reference, and name of new reference much add quotation marks}
\value{
  New estimation of coefficient with statistics and P value (no statistics and P value for glmnet and cv.glmnet)
}
\examples{
# similated data
relapse <- rbinom(100,1,0.3)
BUP <- c(rep("No",61),rep("Yes",39))
Gender <-sample(c('Male', 'Female'), 100, replace=TRUE)
score <-sample(1:10,100,replace=TRUE)
model=glm(relapse~BUP+Gender+score,family="binomial")
rlevel(model,c("BUP","Gender"),c("No","Female"),c("Yes","Male"))
}
