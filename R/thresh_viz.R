#' Visualizing thresh_pred output
#'
#' @description Visualizes the number of correctly and incorrectly predicted values based on output of thresh_pred.
#'
#' @param thresh_tab output of thresh_pred()
#' @param colors vector of 4 colors for barplot
#' @param main title for plot
#' @param xlab label of the x axis
#' @param ylab label of the y axis
#' @param pos type of barplot to produce ("stack", "dodge")
#' @param text Logical statement. Include text indicating the count in each segment of the bar
#' @param t.size size of the within-plot text (using ggplot sizing)
#'
#' @return a ggplot object that visualizes the number of correctly and incorrectly predicted values
#'
#' @example thresh_ex.R
#' @import ggplot2
#' @export

thresh_viz <- function(thresh_tab, colors = c("slategray4", "firebrick4", "firebrick1", "slategray1"),
                       main = "Imputation Quality at Different Thresholds", xlab = "Threshold", ylab = "Prediction Quality Count",
                       pos = "stack", text = F, t.size = 5){

thresh_tab2 <- thresh_tab[seq(1, nrow(thresh_tab), 2), ]
thresh_tab2 <- as.table(data.matrix(thresh_tab2))
rownames(thresh_tab2) <- c("Correctly Imputed", "Incorrectly Imputed", "Incorrrectly Not Imputed", "Correctly Not Imputed")

#plotting all thresholds as stacked bar plots
 p <- ggplot2::ggplot(as.data.frame(thresh_tab2), aes(x = factor(Var2), y = Freq, fill = Var1, ymax = 500)) +
   ggplot2::geom_bar(stat = "identity", position = pos) +
   ggplot2::scale_fill_manual(values = colors) +
   ggplot2::labs(title = main, x = xlab, y = ylab) +
   ggplot2::theme(legend.title = element_blank())

 if (text == T) {p <- p + ggplot2::geom_text(aes(label = round(Freq)), colour = "white", position = "stack", vjust = 1, size = t.size) }

  return(p)

}


