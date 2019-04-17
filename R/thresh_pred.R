#' Prediction Quality at Different Thresholds
#'
#' @description Produces the number and proportion of correctly and incorrectly predicted values based on output of m_predict.
#'
#' @param predict_mat output of m_predict with original dataframe values appended
#' @param missing_var string indicating the variable/feature that you would like to test predictability
#' @param threshold the vector of cutoff point for "sufficient" probability to impute
#'
#' @return a dataframe with 8 rows and as many columns as number of thresholds
#' @return count_match_thresh & prop_match_thresh: observations where prediction and true value match and reaches threshhold
#' @return count_nomatch_thresh & prop_nomatch_thresh: observations where prediction and true value do not match and reaches threshhold
#' @return count_match_nothresh & prop_match_nothresh: observations where prediction and true value match and fail to reach threshhold
#' @return count_nomatch_nothresh & prop_nomatch_nothresh: observations where prediction and true value do not match and fail to reach threshhold
#'
#' @example thresh_ex.R
#' @import plyr
#' @export


thresh_pred <- function(predict_mat, threshold=seq(0,1,0.05), missing_var){

  match <- as.character(predict_mat[ ,missing_var]) == as.character(predict_mat$m1)
  no_match <- as.character(predict_mat[ ,missing_var]) != as.character(predict_mat$m1)

  all_thresh <-  as.data.frame(cbind(plyr::ldply(threshold, function(x) nrow(predict_mat[match & as.numeric(predict_mat$m1_prob) > x,])),
                                     plyr::ldply(threshold, function(x) nrow(predict_mat[match & as.numeric(predict_mat$m1_prob) > x,]) / (nrow(predict_mat[as.numeric(predict_mat$m1_prob) > x,]))),
                                     plyr::ldply(threshold, function(x) nrow(predict_mat[no_match & as.numeric(predict_mat$m1_prob) > x,])),
                                     plyr::ldply(threshold, function(x) nrow(predict_mat[no_match & as.numeric(predict_mat$m1_prob) > x,]) / (nrow(predict_mat[as.numeric(predict_mat$m1_prob) > x,]))),
                                     plyr::ldply(threshold, function(x) nrow(predict_mat[match & as.numeric(predict_mat$m1_prob) <= x,])),
                                     plyr::ldply(threshold, function(x) nrow(predict_mat[match & as.numeric(predict_mat$m1_prob) <= x,]) / nrow(predict_mat[as.numeric(predict_mat$m1_prob) <= x,])),
                                     plyr::ldply(threshold, function(x) nrow(predict_mat[no_match & as.numeric(predict_mat$m1_prob) <= x,])),
                                     plyr::ldply(threshold, function(x) nrow(predict_mat[no_match & as.numeric(predict_mat$m1_prob) <= x,]) / nrow(predict_mat[as.numeric(predict_mat$m1_prob) <= x,]))),
                               row.names = paste0(threshold * 100, "%"))
  colnames(all_thresh) <- c("count_match_thresh", "prop_match_thresh", "count_nomatch_thresh", "prop_nomatch_thresh", "count_match_nothresh", "prop_match_nothresh", "count_nomatch_nothresh", "prop_nomatch_nothresh")
  return(t(all_thresh))
}


#### Outputs: ################################################################################################
## a dataframe with 8 rows and as many columns as number of thresholds                                      ##
## rows: value matches and hits threshhold (count_match_thresh and prop_match_thresh)                       ##
##       value does not match and hits threshhold (count_nomatch_thresh and prop_nomatch_thresh)            ##
##       value matches and doesn't hit threshhold (count_match_nothresh and prop_match_nothresh)            ##
##       value does not match and doesn't hit threshhold (count_nomatch_nothresh and prop_nomatch_nothresh) ##
##############################################################################################################
