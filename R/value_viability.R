#' Determining Value Viability
#'
#' @description Evaluate the viability of an observed value given the values of all other features for that observation and the the CPT fitted on all other observations in the data.
#' @param dat The dataframe
#' @param network Object of class bn that specifies the structure of network
#' @param r Row containing missing data
#' @param missing_var String indicating the variable/feature that you would like to test predictability
#' @param method Method of CPT generation for the bn.fit function (i.e. "mle", "bayes")
#'
#' @return a dataframe with 5 slots: observed, observed_prob, match, MPE, MPE_prob
#' @slot observed The observed value of the specified feature
#' @slot observed_prob Probability of the observed value of the specified feature
#' @slot match Logical value indicating if the value of the MPE matches the observed value
#' @slot MPE Most probable value for the specified feature based on the query
#' @slot MPE_prob Probability of the MPE
#'
#' @example viable_ex.R
#' @import gRain
#' @import bnlearn
#' @export


value_viability <- function(dat, network, method="mle", r, missing_var){
  #unit tests for the input with error messages:
  if (class(network)[1] != "bn") stop("Error: network is not of class bn")

  others <- which(colnames(dat) != missing_var)
  other_names <- colnames(dat)[others]
  output <- as.data.frame(matrix(NA, ncol = 5, nrow = length(r)))
  for (i in 1:length(r)){
    fit_net <- bnlearn::bn.fit(network, data = na.omit(dat[-i,]), method = method)
    fit_net <-  gRbase::compile(bnlearn::as.grain(fit_net))

    true_val <- as.character(dat[r[i], missing_var])
    evi <- lapply(others, function(x) as.character(dat[r[i], x]) )
    names(evi) <- other_names

    m_query <- gRain::querygrain(setEvidence(fit_net, evidence = evi))[[missing_var]]
    missing <- m_query[order(m_query, decreasing = T)]

    colnames(output) <- c("observed", "observed_prob", "match", "MPE", "MPE_prob")
    output$observed[i] <- true_val
    output$observed_prob[i] <- as.vector(missing[which(names(missing) == true_val)])
    output$match[i] <- true_val == names(missing)[1]
    output$MPE[i] <- names(missing)[1]
    output$MPE_prob[i] <- as.vector(missing[1])
    #output$thresh <- missing[1] >= threshold
  }
  return(na.omit(output))
}


#example:
#value_viability(dat = adp_dat2, network = emp_bn, r= 1 ,missing_var = "job")
