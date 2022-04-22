library(mgcv)

mgcv::vis.gam(fit, view = c("cmeanT_2d", "cmeanR_25d"))



observe.vs.pred <- function(aoi = NULL, fo.pred = NULL) {
  fit <- gam(fo.pred, family = nb(), data = aoi, method = "REML")
  pred <- as.data.frame(predict.gam(fit, aoi, type = "response", se.fit = TRUE))
  aoi$pred <- pred$fit
  aoi[is.na(aoi)] <- 0
  
  plot <- aoi %>% 
    
}

test <- dfC

pred <- as.data.frame(predict.gam(fit.C, dfC, type = "response", se.fit = TRUE))#

test$pred <- pred$fit

test[is.na(test)] <- 0

plot <- test %>% 
ggplot(aes(x = pred, y = frequency)) +
  geom_point(stat = "identity", fill = "darkorchid4")
  #labs(title = paste0(area,"- Total Monthly Mean Temperature of entire area "),
       #subtitle = "Data plotted by year",
       #y = "Mean Temperature (°C)",
       #x = "Month") + theme_bw(base_size = 10)+
  #scale_x_date(date_labels = "%b")
ggsave(plot.temp, file = paste0(path.temp, area, ".png"))
