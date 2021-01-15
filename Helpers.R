extract.pggls <- function (model, include.rsquared = TRUE, include.adjrs = TRUE, 
                           include.nobs = TRUE, ...){
  s <- summary(model, ...)
  coefficient.names <- rownames(s$CoefTable)
  coefficients <- s$CoefTable[, 1]
  standard.errors <- s$CoefTable[, 2]
  significance <- s$CoefTable[, 4]
  rs <- s$rsqr
  n <- length(s$resid)
  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()
  if (include.rsquared == TRUE) {
    gof <- c(gof, rs)
    gof.names <- c(gof.names, "R$^2$")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.nobs == TRUE) {
    gof <- c(gof, n)
    gof.names <- c(gof.names, "Num. obs.")
    gof.decimal <- c(gof.decimal, FALSE)
  } 
  if (include.adjrs == TRUE){
      p <- length(coefficient.names)
      ars <- 1 - ((1 - rs)*(n-1)/(n-p-1))
      gof <- c(gof, ars)
      gof.names <- c(gof.names, "Adjusted R$^2$")
      gof.decimal <- c(gof.decimal, TRUE)
    
    }

  tr <- createTexreg(coef.names = coefficient.names, 
                     coef = coefficients, 
                     se = standard.errors, 
                     pvalues = significance, 
                     gof.names = gof.names, 
                     gof = gof, 
                     gof.decimal = gof.decimal)
  return(tr)
}


