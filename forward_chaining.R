var = c( "rain_max_0",
         "rain_min_0",
         "R.max.mean_2d",
         "R.max.mean_5d",                   
         "cmeanR_25d", 
         
         "T.max.mean_2d", 
         "T.min.mean_5d",
         "T.min.mean_25d", 
         "nDry_25d",
         "nSnow_25d",
         "year",
         "DoY",
         "month"
)


fo1<- frequency ~ 
  s(rain_max_0)+
  s(rain_min_0)+
  s(R.max.mean_2d)+
  s(R.max.mean_5d)+                   
  s(cmeanR_25d)+ 
  
  s(T.max.mean_2d)+ 
  s(T.min.mean_5d)+
  s(T.min.mean_25d)+ 
  s(nDry_25d)+
  s(nSnow_25d)+
  s(year, k = 4)+
  s(DoY, bs = "cc")+
  s(month, bs = "cc", k = 12)


fo2<- frequency ~ 
  s(rain_max_0)+
  s(rain_min_0)+
  s(R.max.mean_2d)+
  s(R.max.mean_5d)+                   
  s(cmeanR_25d)+ 
  
  s(T.max.mean_2d)+ 
  s(T.min.mean_5d)+
  s(T.min.mean_25d)+ 
  s(nDry_25d)+
  s(nSnow_25d)+
  s(year, k = 8)+
  s(DoY, bs = "cc")+
  s(month, bs = "cc", k = 12)

fo3<- frequency ~ 
  s(rain_max_0)+
  s(rain_min_0)+
  s(R.max.mean_2d)+
  s(R.max.mean_5d)+                   
  s(cmeanR_25d)+ 
  
  s(T.max.mean_2d)+ 
  s(T.min.mean_5d)+
  s(T.min.mean_25d)+ 
  s(nDry_25d)+
  s(nSnow_25d)+
  s(year, k = 12)+
  s(DoY, bs = "cc")+
  s(month, bs = "cc", k = 12)

fo4<- frequency ~ 
  s(rain_max_0)+
  s(rain_min_0)+
  s(R.max.mean_2d)+
  s(R.max.mean_5d)+                   
  s(cmeanR_25d)+ 
  
  s(T.max.mean_2d)+ 
  s(T.min.mean_5d)+
  s(T.min.mean_25d)+ 
  s(nDry_25d)+
  s(nSnow_25d)+
  s(year, k = 16)+
  s(DoY, bs = "cc")+
  s(month, bs = "cc", k = 12)

fo5<- frequency ~ 
  s(rain_max_0)+
  s(rain_min_0)+
  s(R.max.mean_2d)+
  s(R.max.mean_5d)+                   
  s(cmeanR_25d)+ 
  
  s(T.max.mean_2d)+ 
  s(T.min.mean_5d)+
  s(T.min.mean_25d)+ 
  s(nDry_25d)+
  s(nSnow_25d)+
  s(year, k = 20)+
  s(DoY, bs = "cc")+
  s(month, bs = "cc", k = 12)


forward.chaining <- function(aoi = NULL, var = NULL){
  # 2000-2004
  train <-aoi[aoi$date >= "2000-01-01" & aoi$date <= "2003-12-31",]
  fit.train <- gam(fo1, family = nb(), data = train, method = "REML")
  
  test <- aoi[aoi$date >= "2005-01-01" & aoi$date <= "2005-12-31",] 
  testm <- test[, var]
  
  pred = predict.gam(fit.train, testm, type = "response", se.fit = TRUE)
  pred = as.data.frame(pred)
  
  res <- cbind(test, pred)
  ls <- res[res$frequency >= 1, c("date", "frequency", "fit")]
  nols <- res[res$frequency == 0, c("date", "frequency", "fit")]
  
  MSE.ls <- mean((ls$frequency - ls$fit)^2)
  MSE.nols <- mean((nols$frequency - nols$fit)^2)
  
  res01 <- cbind(MSE.ls, MSE.nols)
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
  
  MSE.ls <- mean((ls$frequency - ls$fit)^2)
  MSE.nols <- mean((nols$frequency - nols$fit)^2)
  
  res02 <- cbind(MSE.ls, MSE.nols)
  
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
  
  MSE.ls <- mean((ls$frequency - ls$fit)^2)
  MSE.nols <- mean((nols$frequency - nols$fit)^2)
  
  res03 <- cbind(MSE.ls, MSE.nols)
  
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
  
  MSE.ls <- mean((ls$frequency - ls$fit)^2)
  MSE.nols <- mean((nols$frequency - nols$fit)^2)
  
  res04 <- cbind(MSE.ls, MSE.nols)
  
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
  
  MSE.ls <- mean((ls$frequency - ls$fit)^2)
  MSE.nols <- mean((nols$frequency - nols$fit)^2)
  
  res05 <- cbind(MSE.ls, MSE.nols)
  
  res <- rbind(res01, res02, res03, res04, res05)
 
  
  return(res)
}


MSE.C <- forward.chaining(dfC,var = var) 
folds <- c(1, 2, 3, 4, 5)
MSE.C <- as.data.frame(cbind(folds, MSE.C))

#####################################################
colors <- c("MSE.ls" = "blue", "MSE.nols" = "red")
ggplot(MSE.C,aes(x = folds))+
  geom_point(aes(y = MSE.ls, color = "MSE.ls"), size = 3)+
  geom_point(aes(y = MSE.nols, color = "MSE.nols"), size = 3)+
  labs(x = "Folds",
       y = "MSE",
       color = "Legend",
       title = "Center - MSE")+
  scale_color_manual(values = colors)

################################################################
MSE.S <- forward.chaining(dfS,var = var) 
folds <- c(1, 2, 3, 4, 5)
MSE.S <- as.data.frame(cbind(folds, MSE.S))


colors <- c("MSE.ls" = "blue", "MSE.nols" = "red")
ggplot(MSE.S,aes(x = folds))+
  geom_point(aes(y = MSE.ls, color = "MSE.ls"), size = 3)+
  geom_point(aes(y = MSE.nols, color = "MSE.nols"), size = 3)+
  labs(x = "Folds",
       y = "MSE",
       color = "Legend",
       title = "South - MSE")+
  scale_color_manual(values = colors)

################################################################
MSE.T <- forward.chaining(dfT,var = var) 
folds <- c(1, 2, 3, 4, 5)
MSE.T <- as.data.frame(cbind(folds, MSE.T))


colors <- c("MSE.ls" = "blue", "MSE.nols" = "red")
ggplot(MSE.T,aes(x = folds))+
  geom_point(aes(y = MSE.ls, color = "MSE.ls"), size = 3)+
  geom_point(aes(y = MSE.nols, color = "MSE.nols"), size = 3)+
  labs(x = "Folds",
       y = "MSE",
       color = "Legend",
       title = "South Tyrol - MSE")+
  scale_color_manual(values = colors)

################################################################
MSE.W <- forward.chaining(dfW,var = var) 
folds <- c(1, 2, 3, 4, 5)
MSE.W <- as.data.frame(cbind(folds, MSE.W))


colors <- c("MSE.ls" = "blue", "MSE.nols" = "red")
ggplot(MSE.T,aes(x = folds))+
  geom_point(aes(y = MSE.ls, color = "MSE.ls"), size = 3)+
  geom_point(aes(y = MSE.nols, color = "MSE.nols"), size = 3)+
  labs(x = "Folds",
       y = "MSE",
       color = "Legend",
       title = "West - MSE")+
  scale_color_manual(values = colors)

######################################################
MSE.E <- forward.chaining(dfE,var = var) 
folds <- c(1, 2, 3, 4, 5)
MSE.E <- as.data.frame(cbind(folds, MSE.E))


colors <- c("MSE.ls" = "blue", "MSE.nols" = "red")
ggplot(MSE.E,aes(x = folds))+
  geom_point(aes(y = MSE.ls, color = "MSE.ls"), size = 3)+
  geom_point(aes(y = MSE.nols, color = "MSE.nols"), size = 3)+
  labs(x = "Folds",
       y = "MSE",
       color = "Legend",
       title = "East - MSE")+
  scale_color_manual(values = colors)
  

