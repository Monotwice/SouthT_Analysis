var = c( "cmeanR_2d",                 
         "cmeanR_25d", 
         
         "cmeanT_2d",
         "nSnow_25d",
         "year",
         "month"
)


fo1<- frequency ~ 
  s(cmeanR_2d)+ 
  s(cmeanR_25d)+
  s(cmeanT_2d)+ 
  

  s(nSnow_25d)+
  s(year, k = 4)+
  s(month, bs = "cc", k = 12)

fo2<- frequency ~ 
  s(cmeanR_2d)+ 
  s(cmeanR_25d)+
  s(cmeanT_2d)+ 
  
  
  s(nSnow_25d)+
  s(year, k = 8)+
  s(month, bs = "cc", k = 12)

fo3<- frequency ~ 
  s(cmeanR_2d)+ 
  s(cmeanR_25d)+
  s(cmeanT_2d)+ 
  
  
  s(nSnow_25d)+
  s(year, k = 12)+
  s(month, bs = "cc", k = 12)

fo4<- frequency ~ 
  s(cmeanR_2d)+ 
  s(cmeanR_25d)+
  s(cmeanT_2d)+ 
  
  
  s(nSnow_25d)+
  s(year, k = 16)+
  s(month, bs = "cc", k = 12)

fo5<- frequency ~ 
  s(cmeanR_2d)+ 
  s(cmeanR_25d)+
  s(cmeanT_2d)+ 
  
  
  s(nSnow_25d)+
  s(year, k = 20)+
  s(month, bs = "cc", k = 12)




forward.chaining.rmse <- function(aoi = NULL, var = NULL){
  # 2000-2004
  train <-aoi[aoi$date >= "2000-01-01" & aoi$date <= "2003-12-31",]
  fit.train <- gam(fo1, family = nb(), data = train, method = "REML")
  
  test <- aoi[aoi$date >= "2004-01-01" & aoi$date <= "2004-12-31",] 
  testm <- test[, var]
  
  pred = predict.gam(fit.train, testm, type = "response", se.fit = TRUE)
  pred = as.data.frame(pred)
  
  res <- cbind(test, pred)
  ls <- res[res$frequency >= 1, c("date", "frequency", "fit")]
  nols <- res[res$frequency == 0, c("date", "frequency", "fit")]
  
  RMSE.ls <- sqrt(mean((ls$frequency - ls$fit)^2))
  RMSE.nols <- sqrt(mean((nols$frequency - nols$fit)^2))
  
  res01 <- cbind(RMSE.ls, RMSE.nols)
  ############################################################################
  # 2000-2008
  train <-aoi[aoi$date >= "2000-01-01" & aoi$date <= "2007-12-31",]
  fit.train <- gam(fo2, family = nb(), data = train, method = "REML")
  
  test <- aoi[aoi$date >= "2008-01-01" & aoi$date <= "2008-12-31",] 
  testm <- test[, var]
  
  pred = predict.gam(fit.train, testm, type = "response", se.fit = TRUE)
  pred = as.data.frame(pred)
  
  res <- cbind(test, pred)
  ls <- res[res$frequency >= 1, c("date", "frequency", "fit")]
  nols <- res[res$frequency == 0, c("date", "frequency", "fit")]
  
  RMSE.ls <- sqrt(mean((ls$frequency - ls$fit)^2))
  RMSE.nols <- sqrt(mean((nols$frequency - nols$fit)^2))
  
  res02 <- cbind(RMSE.ls, RMSE.nols)
  
  ############################################################################
  # 2000-2012
  train <-aoi[aoi$date >= "2000-01-01" & aoi$date <= "2011-12-31",]
  fit.train <- gam(fo3, family = nb(), data = train, method = "REML")
  
  test <- aoi[aoi$date >= "2012-01-01" & aoi$date <= "2012-12-31",] 
  testm <- test[, var]
  
  pred = predict.gam(fit.train, testm, type = "response", se.fit = TRUE)
  pred = as.data.frame(pred)
  
  res <- cbind(test, pred)
  ls <- res[res$frequency >= 1, c("date", "frequency", "fit")]
  nols <- res[res$frequency == 0, c("date", "frequency", "fit")]
  
  RMSE.ls <- sqrt(mean((ls$frequency - ls$fit)^2))
  RMSE.nols <- sqrt(mean((nols$frequency - nols$fit)^2))
  
  res03 <- cbind(RMSE.ls, RMSE.nols)
  
  ############################################################################
  # 2000-2016
  train <-aoi[aoi$date >= "2000-01-01" & aoi$date <= "2015-12-31",]
  fit.train <- gam(fo4, family = nb(), data = train, method = "REML")
  
  test <- aoi[aoi$date >= "2016-01-01" & aoi$date <= "2016-12-31",] 
  testm <- test[, var]
  
  pred = predict.gam(fit.train, testm, type = "response", se.fit = TRUE)
  pred = as.data.frame(pred)
  
  res <- cbind(test, pred)
  ls <- res[res$frequency >= 1, c("date", "frequency", "fit")]
  nols <- res[res$frequency == 0, c("date", "frequency", "fit")]
  
  RMSE.ls <- sqrt(mean((ls$frequency - ls$fit)^2))
  RMSE.nols <- sqrt(mean((nols$frequency - nols$fit)^2))
  
  res04 <- cbind(RMSE.ls, RMSE.nols)
  
  ############################################################################
  # 2000-2020
  train <-aoi[aoi$date >= "2000-01-01" & aoi$date <= "2019-12-31",]
  fit.train <- gam(fo5, family = nb(), data = train, method = "REML")
  
  test <- aoi[aoi$date >= "2020-01-01" & aoi$date <= "2020-12-29",] 
  testm <- test[, var]
  
  pred = predict.gam(fit.train, testm, type = "response", se.fit = TRUE)
  pred = as.data.frame(pred)
  
  res <- cbind(test, pred)
  ls <- res[res$frequency >= 1, c("date", "frequency", "fit")]
  nols <- res[res$frequency == 0, c("date", "frequency", "fit")]
  
  RMSE.ls <- sqrt(mean((ls$frequency - ls$fit)^2))
  RMSE.nols <- sqrt(mean((nols$frequency - nols$fit)^2))
  
  res05 <- cbind(RMSE.ls, RMSE.nols)
  
  res <- rbind(res01, res02, res03, res04, res05)
  
  
  return(res)
}

path.rmse <- "figures/RMSE/"


RMSE.C <- forward.chaining.rmse(dfC,var = var) 
folds <- c(1, 2, 3, 4, 5)
RMSE.C <- as.data.frame(cbind(folds, RMSE.C))

#####################################################
colors <- c("RMSE.ls" = "blue", "RMSE.nols" = "red")
plot.rmse.C <- ggplot(RMSE.C,aes(x = folds))+
  geom_point(aes(y = RMSE.ls, color = "RMSE.ls"), size = 3)+
  geom_point(aes(y = RMSE.nols, color = "RMSE.nols"), size = 3)+
  labs(x = "Folds",
       y = "RMSE",
       color = "Legend",
       title = "Center - RMSE")+
  scale_color_manual(values = colors)
ggsave(plot.rmse.C, file = paste0(path.rmse, "Center", ".png"), width = 10, height = 5)

################################################################
RMSE.S <- forward.chaining.rmse(dfS,var = var) 
folds <- c(1, 2, 3, 4, 5)
RMSE.S <- as.data.frame(cbind(folds, RMSE.S))


colors <- c("RMSE.ls" = "blue", "RMSE.nols" = "red")
plot.rmse.S <- ggplot(RMSE.S,aes(x = folds))+
  geom_point(aes(y = RMSE.ls, color = "RMSE.ls"), size = 3)+
  geom_point(aes(y = RMSE.nols, color = "RMSE.nols"), size = 3)+
  labs(x = "Folds",
       y = "RMSE",
       color = "Legend",
       title = "South - RMSE")+
  scale_color_manual(values = colors)
ggsave(plot.rmse.S , file = paste0(path.rmse, "South", ".png"), width = 10, height = 5)

################################################################
RMSE.T <- forward.chaining.rmse(dfT,var = var) 
folds <- c(1, 2, 3, 4, 5)
RMSE.T <- as.data.frame(cbind(folds, RMSE.T))


colors <- c("RMSE.ls" = "blue", "RMSE.nols" = "red")
plot.rmse.T <- ggplot(RMSE.T,aes(x = folds))+
  geom_point(aes(y = RMSE.ls, color = "RMSE.ls"), size = 3)+
  geom_point(aes(y = RMSE.nols, color = "RMSE.nols"), size = 3)+
  labs(x = "Folds",
       y = "RMSE",
       color = "Legend",
       title = "South Tyrol - RMSE")+
  scale_color_manual(values = colors)
ggsave(plot.rmse.T , file = paste0(path.rmse, "South TYrol", ".png"), width = 10, height = 5)

################################################################
RMSE.W <- forward.chaining.rmse(dfW,var = var) 
folds <- c(1, 2, 3, 4, 5)
RMSE.W <- as.data.frame(cbind(folds, RMSE.W))


colors <- c("RMSE.ls" = "blue", "RMSE.nols" = "red")
plot.rmse.W <- ggplot(RMSE.T,aes(x = folds))+
  geom_point(aes(y = RMSE.ls, color = "RMSE.ls"), size = 3)+
  geom_point(aes(y = RMSE.nols, color = "RMSE.nols"), size = 3)+
  labs(x = "Folds",
       y = "RMSE",
       color = "Legend",
       title = "West - RMSE")+
  scale_color_manual(values = colors)
ggsave(plot.rmse.W , file = paste0(path.rmse, "West", ".png"), width = 10, height = 5)

######################################################
RMSE.E <- forward.chaining.rmse(dfE,var = var) 
folds <- c(1, 2, 3, 4, 5)
RMSE.E <- as.data.frame(cbind(folds, RMSE.E))


colors <- c("RMSE.ls" = "blue", "RMSE.nols" = "red")
plot.rmse.E <- ggplot(RMSE.E,aes(x = folds))+
  geom_point(aes(y = RMSE.ls, color = "RMSE.ls"), size = 3)+
  geom_point(aes(y = RMSE.nols, color = "RMSE.nols"), size = 3)+
  labs(x = "Folds",
       y = "RMSE",
       color = "Legend",
       title = "East - RMSE")+
  scale_color_manual(values = colors)
ggsave(plot.rmse.E , file = paste0(path.rmse, "East", ".png"), width = 10, height = 5)



################################################################################