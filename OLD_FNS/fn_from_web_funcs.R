#' @title compute.sample.size.reqs
#' @description FUNCTION_DESCRIPTION
#' @param design PARAM_DESCRIPTION
#' @param test_alpha PARAM_DESCRIPTION
#' @param test_beta PARAM_DESCRIPTION
#' @param parameters PARAM_DESCRIPTION
#' @param ncoefficients PARAM_DESCRIPTION
#' @param nalts PARAM_DESCRIPTION
#' @param nchoices PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details # This implements as a function the code described here:
#' https://www.erim.eur.nl/choice-modelling/resources/software-tools/sample-size-requirements/
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname compute.sample.size.reqs
#' @export

compute.sample.size.reqs <- function(design,
                                     test_alpha,
                                     test_beta,
                                     parameters,
                                     ncoefficients,
                                     nalts,
                                     nchoices){
  z_one_minus_alpha<-qnorm(1-test_alpha)
  z_one_minus_beta<-qnorm(1-test_beta)
  #compute the information matrix
  # initialize a matrix of size ncoefficients by ncoefficients filled with zeros.
  info_mat=matrix(rep(0,ncoefficients*ncoefficients), ncoefficients, ncoefficients)
  # compute exp(design matrix times initial parameter values)
  exputilities=exp(design%*%parameters)
  # loop over all choice sets
  for (k_set in 1:nchoices) {
    # select alternatives in the choice set
    alternatives=((k_set-1)*nalts+1) : (k_set*nalts)
    # obtain vector of choice shares within the choice set
    p_set=exputilities[alternatives]/sum(exputilities[alternatives])
    # also put these probabilities on the diagonal of a matrix that only contains zeros
    p_diag=diag(p_set)
    # compute middle term P-pp’
    middle_term<-p_diag-p_set%o%p_set
    # pre- and postmultiply with the Xs from the design matrix for the alternatives in this choice set
    full_term<-t(design[alternatives,])%*%middle_term%*%design[alternatives,]
    # Add contribution of this choice set to the information matrix
    info_mat<-info_mat+full_term
  } # end of loop over choice sets
  #get the inverse of the information matrix (i.e., gets the variance-covariance matrix)
  sigma_beta<-solve(info_mat,diag(ncoefficients))
  # Use the parameter values as effect size. Other values can be used here.
  effectsize<-parameters
  # formula for sample size calculation is n>[(z_(beta)+z_(1-alpha))*sqrt(Σγκ)/delta]^2
  N<-((z_one_minus_beta + z_one_minus_alpha)*sqrt(diag(sigma_beta))/abs(effectsize))^2
  return(N) # Return results (required sample size for each coefficient)
}
#' @title one.parameter.set
#' @description return coefficients for the contstant, categorical attributes and continuous attribute
#' @param constant.coeff PARAM_DESCRIPTION
#' @param first.coeff PARAM_DESCRIPTION
#' @param coeff.multiplier PARAM_DESCRIPTION
#' @param continuous.coeff PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[purrr]{map2}}
#' @rdname one.parameter.set
#' @export
#' @importFrom purrr pmap
one.parameter.set <- function(constant.coeff,
                              first.coeff,
                              coeff.multiplier,
                              continuous.coeff){
  second.coeff<-first.coeff*coeff.multiplier
  partial.list<-purrr::pmap(list(a=first.coeff,
                                 b=second.coeff),
                            ~ c(..1,..2))
  partial.vect<-unlist(partial.list)
  # Sunmmarise the coefficients here
  parameters<-c(constant.coeff,partial.vect,continuous.coeff)
  return(parameters)
}
#' @title prop.params.ok
#' @description estimate what proportion of parameters have required sample sizes less than our envisioned sample
#' @param sample.size.results PARAM_DESCRIPTION
#' @param samplesize PARAM_DESCRIPTION
#' @param nbr.parameters PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname prop.params.ok
#' @export

prop.params.ok <- function(sample.size.results,
                           samplesize,
                           nbr.parameters){
  countokwithsamplesize<-length(which(sample.size.results<samplesize))
  propokwithsamplesize<-countokwithsamplesize/length(nbr.parameters)
  return(propokwithsamplesize)
}
