#' Predicting Missing Values
#'
#' @description Predict the top 3 most probable values for missing categorical data using a trained bayesian network
#' @param dat the dataframe
#' @param network object of class CPT or CPTgrain that specifies the structure of network
#' @param r row containing missing data
#' @param missing_var string indicating the variable/feature that you would like to test predictability
#'
#' @return a dataframe with 6 slots: m1, m1_prob, m2, m2_prob, m3, m3_prob
#' @slot  m1: most probable value for the missing feature
#' @slot m1_prob: probability of m1
#' @slot m2: second most probable value for the missing feature
#' @slot m2_prob: probability of m2
#' @slot m3: third most probable value for the missing feature
#' @slot m3_prob: probability of m3
#'
#' @example example.R
#' @import gRain
#' @import bnlearn
#' @import plyr
#' @export


m_predict <- function(dat, network, r, missing_var){

  #unit tests for the input with error messages:
  if (class(network)[1] != "CPTgrain") stop("Error: network is not of class CPT grain")
  missing_var <- which(colnames(dat) == missing_var)
  if (length(missing_var) == 0) stop("cannot execute query for missing variable that is not in dataframe")

  #evidence must be input to the querygrain function as a list. this code creates this list, regardless of number of covariates or which variable is missing.
  others <- which(1:dim(dat)[2] != missing_var)
  other_names <-  colnames(dat)[others]
  evi <- vector("list", length = length(others))
  evi <- lapply(others,  function(x) as.character(dat[r, x]) )
  names(evi) <- other_names

  #querying the network: output of this query is an array of probabilities
  missing_list <- gRain::querygrain(gRain::setEvidence(network, evidence = evi))[[colnames(dat)[missing_var]]]
  #putting them in order from most to least probable
  missing <- missing_list[order(missing_list, decreasing = T)]

  if (is.na(missing[1]) == T) {missing.vec <- t(as.data.frame(c(rep(NA, 6))))}
  else{
    #m1 is the most probable missing value
    m1 <- missing[1]
    m2 <- missing[2]
    m3 <- missing[3]
    #create a matrix and add this observation to the missing.mat matrix
    missing.mat <- as.matrix(c(m1, m2, m3))
    #make this into a vector in order for it to append to the dataframe
    missing.vec <- t(as.data.frame(c(rownames(missing.mat), missing.mat[ ,1])))}
  colnames(missing.vec) <- c("m1", "m2", "m3", "m1_prob", "m2_prob", "m3_prob")
  rownames(missing.vec) <- r
  return(missing.vec[ ,c(1, 4, 2, 5, 3, 6)])

}

#example:
#m_predict(dat= adp_dat25, network= emp_bn25, r= as.numeric(rownames(adp_dat25[1,])),missing_var = "job", threshold= .7)
