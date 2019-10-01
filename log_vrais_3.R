L_exp <- function(X_N, lambda, L) {
  return (sum(log(lambda) - lambda * (X_N - L)))
}

L_cauchy <- function(X_N, x0, alpha) {
  pi <- 3.1418
  return (sum(log(alpha / pi) - log((X_N - x0)**2 + alpha**2)))
}

L_exp_1 <- function(lambda, L) {
  x <- rexp(N,lambda)
  R <- dexp(x,lambda)*exp(-lambda*L)
  -sum(log(R))
}


L_cauchy_1 <- function(x0, alpha) {
  x<- rcauchy(N,x0,alpha)
  R <- dcauchy(x,-2,0.4)
  pi <- 3.1418
  -sum(log(R))
}
