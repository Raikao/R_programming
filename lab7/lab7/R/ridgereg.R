#' Perform ridge regression using QR decomposition
#'
#' Use \code{ridgereg} to perform ridge regression.
#'
#' @param formula an object of \code{\link{class}} \code{\link{formula}} (or one
#'   that can be coerced to that class): a symbolic description of the model to
#'   be fitted.
#' @param data a data frame or (or object coercible by
#'   \code{\link{as.data.frame}}).
#' @param lambda the lambda of the data set. Default is 0.
#'
#' @return \code{ridgereg} returns an object of class "\code{Ridgereg}".


ridgereg <- function(formula, data, subset, na.action, lambda = 0, model = FALSE, x = FALSE, y = FALSE, contrasts = NULL, ...) {
  m <- match.call(expand.dots = FALSE)
  m$model <- m$x <- m$y <- m$contrasts <- m$... <- m$lambda <- NULL
  m[[1L]] <- quote(stats::model.frame)
  m <- eval.parent(m)
  Terms <- attr(m, "terms")
  Y <- stats::model.response(m)
  X <- stats::model.matrix(Terms, m, contrasts)
  n <- nrow(X)
  p <- ncol(X)
  offset <- stats::model.offset(m)
  if (!is.null(offset))
    Y <- Y - offset
  if (Inter <- attr(Terms, "intercept")) {
    Xm <- colMeans(X[, -Inter])
    Ym <- mean(Y)
    p <- p - 1
    X <- X[, -Inter] - rep(Xm, rep(n, p))
    Y <- Y - Ym
  }
  else  Ym <- Xm <- NA
  Xscale <- drop(rep(1/n, n) %*% X^2)^0.5
  X <- X/rep(Xscale, rep(n, p))
  Xs <- svd(X)
  rhs <- t(Xs$u) %*% Y
  d <- Xs$d
  #this is the actual formula used to determine coefficients
  #save coefficients as lscoef
  answer.qr = qr(crossprod(X) + diag(n*lambda,p,p))
  lscoef = qr.coef(answer.qr,crossprod(X,Y))
  #only modify the two lines above
  lsfit <- X %*% lscoef
  resid <- Y - lsfit
  s2 <- sum(resid^2)/(n - p - Inter)
  HKB <- (p - 2) * s2/sum(lscoef^2)
  LW <- (p - 2) * s2 * n/sum(lsfit^2)
  k <- length(lambda)
  dx <- length(d)
  div <- d^2 + rep(lambda, rep(dx, k))
  a <- drop(d * rhs)/div
  dim(a) <- c(dx, k)
  coef <- Xs$v %*% a
  dimnames(coef) <- list(names(Xscale), format(lambda))
  name <- deparse(substitute(data))
  GCV <- colSums((Y - X %*% coef)^2)/(n - colSums(matrix(d^2/div,
                                                         dx)))^2
  res <- list(coef = drop(coef), scales = Xscale, Inter = Inter,
              lambda = lambda, ym = Ym, xm = Xm, GCV = GCV, kHKB = HKB,
              kLW = LW, formula = formula, name=name, data = data)
  attr(res, "class") <- "ridgereg"
  res
}
#' print ridgereg
#' 
#' Print the ridgereg class
#' 
#'@param x linreg class object
#'@param ... other parameters
#'@export

print.ridgereg <- function(x, ...){
  cat("\nCall:\n",
      "ridgereg(formula = ",paste(deparse(x$formula), sep = "\n", collapse = "\n"),", data = ",x$name,")", "\n\n", sep = "")
  cat("Coefficients: \n")
  cat(" ")
  cat("\t",names(coef(x)))
  cat(sep="\n")
  cat(sep="   ",coef(x))
}
#'coef ridgereg
#'
#'Return the coefficients of the ridge regression
#'
#'@param x linreg class object
#'@export
coef.ridgereg <- function(x){
  X <- model.matrix(x$formula, x$data)
  X<- X[,-(1)]
  betaHat.unscaled <- x$coef
  betaHat.scaled <- betaHat.unscaled/x$scales
  interceptHat<-x$ym -  mean(X %*% betaHat.scaled)
  betaHat<-append(betaHat.scaled,interceptHat,after=0)
  return(betaHat)
}
#'predict ridgereg
#'
#'Return the fitted values of the ridgeear regression
#'
#'@param x linreg class object
#'@export
predict.ridgereg<- function(x){
  X <- model.matrix(x$formula, x$data)
  
  # Get estimated coefficients
  betaHat <- coef(x)
  
  yHat <- betaHat %*% t(X)
  # # Format in the same way as lm()
  yHatVector <- as.vector(yHat)
  names(yHatVector) <- colnames(yHat)
  yHat <- yHatVector
  return(yHat)
}

