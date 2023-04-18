#' @title Estimate t values of mixed linear model
#' @description perform mixed linear regression in lmerTest package for getting
#' observed t values or permutation test
#' @param data A dataframe specifying the data to be analysed
#' @param formula A formula in the form like'y~x1+x2+x3+(1|x4) or like'y~x1+x2+x3
#' +(x3|x4) in lmer function
#' @return An object of class "lmerTest"
#' @export
#' @import lmerTest
#' @examples
#' data<-mtcars
#' formula<-mpg~cyl+(1|gear)
#' statistic<-s2(data=data,formula=formula)

s2 <- function(data, formula) {
  fit <- lmer(formula, data = data)
  return(fit)
}
