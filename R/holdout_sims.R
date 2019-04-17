#' Simulating Imputation to Assess Accuuracy
#'
#' @param dat the dataframe
#' @param network  object of class bn that specifies the structure of network
#' @param n.sims the number of simulations to cycle through
#' @param missing_var string indicating the variable/feature that you would like to test predictability
#' @param threshold the cuttoff point for "sufficient" probability to impute
#' @param holdout.prop proportion of observations to be held out
#'
#' @return a dataframe with 4 slots: holdout, n.sims, thresh_correct, hits_thresh, thresh_correct_prop
#' @slot  holdout Number many NAs created)
#' @slot  n.sims Number of simulations
#' @slot  thresh_correct Number of predictions that hit the threshold and were correct
#' @slot  hits_thresh Number of predictions that hit the threshold
#' @slot  thresh_correct_prop Proportion of predictions that hit the threshold which were correct (thresh_correct/hits_thresh)
#'
#' @import gRain
#' @import bnlearn
#' @import plyr
#' @export


holdout_sims <- function(dat, network, n.sims, threshold=0.5, missing_var, holdout.prop){
  #unit tests for the input with error messages:
  if (holdout.prop >= 1 | holdout.prop <= 0) stop("Error: holdout.prop value is out of range")
  if (threshold >= 1 | threshold < 0) stop("Error: threshold value is out of range")
  if (class(network)[1] != "bn") stop("Error: network is not of class 'bn'")

  holdout <- round(holdout.prop * nrow(dat))
  dat
  tries_thresh_correct <- NULL
  tries_hits_thresh <- NULL
  tries_prop_correct <- NULL
  for (i in 1:n.sims){
    dat_na <- dat
    dat_na[sample(1:nrow(dat), holdout), missing_var] <- NA

    emp_net <- bnlearn::bn.fit(network, data = na.omit(dat_na))
    emp_bn <-  gRbase::compile(bnlearn::as.grain(emp_net))

    missing <- plyr::adply(dat[is.na(dat_na[ ,missing_var]) == T,], 1, function(x) m_predict(dat = dat, network = emp_bn, r = max(as.numeric(rownames(x))), missing_var = missing_var), .progress = "text")

    colnames(missing)[(ncol(missing)-5):ncol(missing)] <- c("m1", "m1_prob", "m2", "m2_prob", "m3", "m3_prob")

    thresh_rows <- which(as.numeric(missing$m1_prob) > threshold)
    thresh_correct <- sum(na.omit(missing[thresh_rows, missing_var] == missing[thresh_rows,]$m1))#/nrow(missing[,])
    hit_thresh <- nrow(missing[thresh_rows, ])#/nrow(missing)
    prop_correct <- thresh_correct / hit_thresh

    tries_thresh_correct <- c(tries_thresh_correct, thresh_correct)
    tries_hits_thresh <- c(tries_hits_thresh, hit_thresh)
    tries_prop_correct <- c(tries_prop_correct, prop_correct)
  }
  out <- as.data.frame(cbind(rep(holdout,n.sims), rep(n.sims,n.sims), tries_thresh_correct, tries_hits_thresh, tries_prop_correct))
  colnames(out) <- c("holdout", "n.sims", "thresh_correct", "hits_thresh", "thresh_correct_prop")
  return(out)
}

##### Output for holdout_sims ##############################################################################################
## out.list: a dataframe with 4 columns: holdout, n.sims, thresh_correct, hits_thresh                                     ##
##        holdout: vector(length 1) number many NAs created                                                               ##
##        n.sims: vector(length 1) number of simulations                                                                  ##
##        thresh_correct: vector(length n.sims) number of predictions that hit the threshold and were correct             ##
##        hits_thresh: vector(length n.sims) number of predictions that hit the threshold                                 ##
##        thresh_correct_prop:  vector(length n.sims) proportion of predictions that hit the threshold which were correct ##
############################################################################################################################

#example:
#holdout_sims(dat= adp_dat2, network= net, n.sims=15, threshold= .7, missing_var= "job", holdout.prop= .3)
