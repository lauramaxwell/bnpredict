#m_pred_out is output from an m_predict() command
data(m_pred_out, package = "bnpredict")

#default thresholds
job_thresh <-  thresh_pred(m_pred_out, missing_var="job")
job_thresh

#custom thresholds- 0.5 to 0.9 by 0.05
job_thresh_cut <-  thresh_pred(m_pred_out, missing_var="job", threshold = seq(from = 0.5, to = 0.9, by = 0.05))
job_thresh_cut

#visualize
thresh_viz(job_thresh_cut)

#custom visualization
thresh_viz(job_thresh_cut, colors = c("lightskyblue1", "darkorange1", "darkorange3", "lightskyblue3"),
           main = "My Custom Title", text = T, t.size = 3)
