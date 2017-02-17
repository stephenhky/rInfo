
is.valid.pvector<- function(pvector) all(pvector>=0)

#' Normalize probability distribution
#'
#' @param pvector a vector of all probability distribution
#' @return a norm vector for all probability distribution
#' @export
normalize.pvector<- function(pvector) pvector / sum(pvector)

#' Compute Kullback-Leibler divergence
#'
#' @param p1vector a vector of probability distribution
#' @param p2vector a vector of probability distribution
#' @return Kullback-Leibler divergence
#' @export
KLD<- function(p1vector, p2vector) {
  normp1vector<- normalize.pvector(p1vector)
  normp2vector<- normalize.pvector(p2vector)
  sum( normp1vector*log(normp1vector/normp2vector), na.rm = TRUE)
}

#' Compute Jenson-Shannon divergence
#'
#' @param p1vector a vector of probability distribution
#' @param p2vector a vector of probability distribution
#' @return Jensen-Shannon divergence
#' @export
JSD<- function(p1vector, p2vector) {
  0.5*KLD(p1vector, 0.5*(p1vector+p2vector))+0.5*KLD(p2vector, 0.5*(p1vector+p2vector))
}
