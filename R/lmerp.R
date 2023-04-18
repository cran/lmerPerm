#' @title This function is used for permutation test for general and mixed linear regression
#' @description perform general and mixed linear regression by lm function in R base or lmer
#' function in lmer/lmertest package and permutation tests on observed t values of beta coef
#' -ficients
#' @param formula Regression formula in the form 'y~x1+x2+x3' for general linear function or
#' 'y~x1+x2+x3+(1|x4)' or 'y~x1+x2+x3+(x3|x4)' for mixed linear function
#' @param data A data frame specifying the data to be analysed
#' @param thresh  Threshold to stop iteration, default value is 0.1
#' @param R The maximum number of iteration, default value is 1000
#' @param mixed A logic value indicates if you desire to perform mixed linear  model or not.
#' Default value is FALSE.
#' @param minimum The minimum number of iteration, default value is 50
#' @return A list contains 2 items: Results and T_perm, the former contains results of origi
#' -nal parameter test and results of permutation test including adjusted confident interval
#' (Ci_perm), p values (P_perm), iteration number(Iteration), the later contains a list cont
#' -ains all t values generated in each permutation
#' @export
#' @import lmerTest
#' @importFrom stats lm sd vcov confint quantile
#' @examples
#' formula<-mpg~cyl
#' data<-mtcars
#' my_perm<-lmerp(formula,data)

lmerp <- function(formula,
                  data,
                  thresh,
                  R,
                  mixed,
                  minimum)
{
  # Check if a parameter is missing and set as default
  if (missing(thresh)) {
    thresh <- 0.1
  }
  if (missing(R)) {
    R <- 1000
  }
  if (missing(mixed)) {
    statistic <- s1
    mixed <- FALSE
  } else if (mixed == TRUE) {
    statistic <- s2
    mixed <- TRUE
  } else {
    statistic <- s1
    mixed <- FALSE
  }
  if (missing(minimum)) {
    minimum <- 50
  }
  mle <- as.character(formula[2][[1]])
  sta <- 't value'
  # Compute the observed t-statistic
  Model_org <- statistic(data = data, formula = formula)
  Sum_org <- summary(Model_org)
  coef_orig <- Sum_org$coefficients
  tvals_orig <- coef_orig[, sta]
  pval_orig <- coef_orig[, "Pr(>|t|)"]
  ci_orig <- confint(Model_org)
  # set permute function
  permute <- permute_fun
  # Initialize the number of permutations and the standard deviation of p-values
  T_perm <- list()
  N_perm <- matrix(0, nrow = 1, ncol = length(tvals_orig))
  P_perm <- matrix(0, nrow = 1, ncol = length(tvals_orig))
  # Calculate total iteration for each variable
  # Loop until the stopping criterion is met
  for (vid in 1:length(tvals_orig)) {
    nperm <- 0
    sd_p <- 1
    est_p <- 1
    T <- matrix(NA, nrow = 1, ncol = 1)
    pv <- matrix(NA, nrow = 1, ncol = 1)
    while (sd_p >= thresh * est_p) {
      # Generate a new permutation of the response variable
      datau <- permute(data = data, mle = mle)
      nperm <- nperm + 1

      # Compute the statistic for the permuted data
      Model_perm <- statistic(data = datau, formula = formula)
      Sum_perm <- summary(Model_perm)
      tvals_perm <- Sum_perm$coefficients[, sta]
      T <- append(T, tvals_perm[vid])
      T <- T[!is.na(T)]

      # Compute the p-value for the permutation test
      est_p <- mean(abs(T) >= abs(tvals_orig[vid]))
      pv <- append(pv, est_p)
      pv <- pv[!is.na(pv)]
      if (nperm <= minimum) {
        sd_p <- sd_p
      } else {
        sd_p <- sd(pv)
      }
      # Set stop condition
      if (nperm > R) {
        break
      }
    }
    # Collect results
    T_perm[[vid]] <- T
    P_perm[vid] <- est_p
    N_perm[vid] <- nperm
  }
  P_perm <- t(P_perm)
  N_perm <- t(N_perm)
  colnames(P_perm) <- 'P_perm'
  colnames(N_perm) <- 'Iteration'
  # Calculate adjusted ci
  # confidence interval for T_perm
  alpha <- 0.05
  t.crit <- matrix(NA, nrow = length(tvals_orig), ncol = 2)
  for (i in 1:length(tvals_orig)) {
    t.crit[i,] <- quantile(T_perm[[i]], c(alpha / 2, 1 - alpha / 2))
  }

  # Calculate confidence intervals for each beta coefficient based on the critical t-values and standard errors.
  Ci_perm <- matrix(NA, nrow = length(tvals_orig), ncol = 2)
  for (i in 1:length(tvals_orig)) {
    Ci_perm[i,] <-
      tvals_orig[i] + coef_orig[, 'Std. Error'][i] * t.crit[i,]
  }
  colnames(Ci_perm) <- c('Ci_perm_2.5%', 'Ci_perm_97.5%')
  colnames(ci_orig) <- c('Ci_2.5%', 'Ci_97.5%')
  if (mixed) {
    ci_orig <- ci_orig[-c(1, 2), ]
  }
  # Get total results
  Results <-
    cbind(coef_orig[, 'Estimate'], ci_orig, Ci_perm, coef_orig[, 2:ncol(coef_orig)], P_perm, N_perm)
  colnames(Results)[1] <- 'Estimate'
  # Give name to results
  names(T_perm) <- names(tvals_orig)
  # Give results to original model
  Model_fin <- list(Results, T_perm)
  names(Model_fin) <- c('Results', 'T_perm')
  return(Model_fin)
}
