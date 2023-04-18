#' @title This function defines the permutation strategry
#' @description perform permuation on response variable i.e. y,
#' using the stop criterion suggested by Anscombe
#' @param data A data frame specifying the data to be analysed.
#' @param mle A string that indicated response variable
#' @return A data frame containing the data with a permuted y.
#' @export
#' @examples
#' data<-mtcars
#' permute<-permute_fun(data=data,mle='mpg')

permute_fun <- function(data, mle)
{
  n <- nrow(data)
  pdata <- data
  pdata[, mle] <- sample(pdata[, mle])
  return(pdata)
}
