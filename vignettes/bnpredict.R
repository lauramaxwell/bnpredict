## ------------------------------------------------------------------------
net <- bnlearn::model2network("[compensation|age:full_time][age]
                          [job|compensation:full_time][full_time]
                          [cluster|job:compensation:age:full_time]")

## ---- eval = F-----------------------------------------------------------
#  bnlearn::graphviz.plot(net, shape="ellipse")

## ---- fig.show = 'hold', fig.align = 'center', echo=FALSE----------------
suppressMessages(bnlearn::graphviz.plot(net, shape="ellipse"))

## ---- echo=FALSE---------------------------------------------------------
this <- cbind(colnames(dat), as.vector(plyr::aaply(colnames(dat), 1, function(x) length(levels(as.factor(dat[,x]))))))
colnames(this) <- c("feature", "# categories")
knitr::kable(this)

## ------------------------------------------------------------------------
data(dat, package = "bnpredict")
dat25 <- dat
dat25[sample(1:nrow(dat25),500),]$job <- NA

## ---- , eval = F---------------------------------------------------------
#  emp_net25 <- bnlearn::bn.fit(net, data = na.omit(dat25), method = "mle")
#  emp_bn25 <-  gRbase::compile(bnlearn::as.grain(emp_net25))

## ---- , echo = F---------------------------------------------------------
suppressWarnings(emp_net25 <- bnlearn::bn.fit(net, data = na.omit(dat25), method = "mle"))
suppressWarnings(emp_bn25 <-  gRbase::compile(bnlearn::as.grain(emp_net25)))

## ---- echo = F-----------------------------------------------------------
tab <- cbind(as.numeric(dat25[1:10,1]),as.numeric(dat25[1:10,2]), dat25[1:10,c(3,4)], as.numeric(dat25[1:10,5]))
colnames(tab) <- colnames(dat25)
knitr::kable(tab)

## ------------------------------------------------------------------------
m_predict(dat= dat25, network= emp_bn25, r= as.numeric(rownames(dat25[1,])),missing_var = "job")

## ------------------------------------------------------------------------
job_pred <- plyr::adply(dat[is.na(dat25$job) == T,], 1, 
                          function(x) m_predict(dat= dat, 
                                               network= emp_bn25, 
                                               r= max(as.numeric(rownames(x))), 
                                               missing_var = "job"))

## ---- echo = F-----------------------------------------------------------
job_pred <- na.omit(job_pred)

## ---- echo = F, fig.width = 7, fig.height = 4.2--------------------------
job_pred$match <- as.character(job_pred$m1) == as.character(job_pred$job)
job_pred$Prediction <- factor(job_pred$match, levels = c("FALSE", "TRUE"), 
                              labels = c("Not Match", "Match"))

ggplot2::ggplot(job_pred, ggplot2::aes(x = as.numeric(m1_prob), fill = Prediction)) + 
  ggplot2::geom_histogram(binwidth = .99/30) +
  ggplot2::scale_fill_manual( values = c("firebrick1", "slategray3")) +
  ggplot2::labs(x = "MPE probability", title = "Distribution of job MPE probabilities and correct predictions") +
  ggplot2::theme_classic()

## ------------------------------------------------------------------------
job_thresh <-  thresh_pred(job_pred, missing_var="job")

## ---- echo=FALSE, results='asis'-----------------------------------------
 knitr::kable(job_thresh[,c(1:10)], digits = 2)
 knitr::kable(job_thresh[,c(11:21)], digits = 2)

## ------------------------------------------------------------------------
job_thresh_cut <-  thresh_pred(job_pred, missing_var="job", 
                               threshold = seq(from = 0.5, to = 0.9, by = 0.05))

## ---- echo=FALSE, results='asis'-----------------------------------------
knitr::kable(job_thresh_cut, digits = 2)

## ---- fig.width = 8, fig.height = 5, warn = -1---------------------------
thresh_viz(job_thresh_cut)

## ---- fig.width = 8, fig.height = 5, warn = -1---------------------------
thresh_viz(job_thresh_cut, colors = c("lightskyblue1", "darkorange1", "darkorange3", "lightskyblue3"),
           main = "My Custom Title", text = T, t.size = 3)

