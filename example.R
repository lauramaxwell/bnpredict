data(dat, package = "bnpredict")
net <- bnlearn::model2network("[compensation|age:full_time][age][job|compensation:full_time][full_time][cluster|job:compensation:age:full_time]")
#bnlearn::graphviz.plot(net, shape="ellipse",
#                         Rgraphviz::layout="dot")
dat25 <- dat
dat25[sample(1:nrow(dat25),500),]$job <- NA
emp_net25 <- bnlearn::bn.fit(net, data = na.omit(dat25))
emp_bn25 <-  gRbase::compile(bnlearn::as.grain(emp_net25))

##predicting the first row for missing job titles
m_predict(dat= dat25, network= emp_bn25, r= as.numeric(rownames(dat25[1,])),missing_var = "job")

##predicting all rows (this uses the package plyr)
m_pred_out <- plyr::adply(dat[is.na(dat25$job) == T,], 1, function(x) m_predict(dat= dat25, network= emp_bn25, r= max(as.numeric(rownames(x))), missing_var = "job"), .progress = "text")
