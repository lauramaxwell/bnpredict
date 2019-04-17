data(dat, package = "bnpredict")
net <- bnlearn::model2network("[compensation|age:full_time][age][job|compensation:full_time]
                      [full_time][cluster|job:compensation:age:full_time]")

#evaluating viability of compensation in the first row of dat
value_viability(dat= dat, r= 15, network = net,  missing_var = "compensation")

#using plyr to evaluate viability of first 200 rows of dat for compensation and job title
comp_out <- plyr::adply(dat[1:200,], 1, function(x) value_viability(dat = dat, network = net,
                                                              r= max(as.numeric(rownames(x))),
                                                              missing_var = "compensation"), .progress = "text")

job_out <- plyr::adply(dat[1:200,], 1, function(x) value_viability(dat = dat, network = net,
                                                             r= max(as.numeric(rownames(x))),
                                                             missing_var = "job"), .progress = "text")
