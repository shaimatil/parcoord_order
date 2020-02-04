# Generic function
# f matrix
# h matrix
# G univariate function

GI<- function(f, h, G) {

  S <- G(f/h)*h
  S[is.nan(S)] <- 0
  G <- sum(S)
  return(G)
}
