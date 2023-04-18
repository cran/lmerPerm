#' @title Return t values of general linear model
#' @description perform mixed linear regression in lmer/lmertest package
#' for getting observed t values or permutation test
#' @param data A dataframe specifying the data to be analysed
#' @param formula A formula in the form like'y~x1+x2+x3 in lm function
#' @return An object of class "lm"
#' @export
#' @importFrom stats lm
#' @examples
#' data<-mtcars
#' formula<-mpg~cyl
#' s1(data=data,formula=formula)

s1 <- function(data, formula) {
  fit <- lm(formula, data = data)
  return(fit)
}
