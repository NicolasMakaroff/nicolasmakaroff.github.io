L_norm <- function(sample,mu,sigma){
  vect_vraisemblance <- dnorm(sample,mu,sigma)
  vrais=prod(vect_vraisemblance)
  return(vrais)
}

L_norm_1 <- function(sample,mu,sigma){
  #vect_vraisemblance <- dnorm(sample,mu,sigma)
  vrais=prod(sample)
  return(vrais)
}

log_L_norm <-  function(sample,mu,sigma){
  return(sum(log(dnorm(sample,mu,sigma))))
}
