
L_bern <- function(sample,p){
  vect_vraisemblance <- dbinom(sample,size=1,p)
  vrais=prod(vect_vraisemblance)
  return(vrais)
}

log_L_bern <- function(sample, p) {
  #vect_vraisemblance <- dbinom(sample,size=1,p)
  vect_vraisemblance<-sample
  s   <- sum(vect_vraisemblance)
  n   <- length(vect_vraisemblance)
  log_vraisemblance <- s * log(p) + (n - s) * log(1 - p)
  return (log_vraisemblance)
}
