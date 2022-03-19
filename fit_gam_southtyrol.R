library(mgcv)
library("countreg")
library("MASS")
library(DHARMa)
library(mgcViz)

set.seed(3)
n<-400
dat <- gamSim(1,n=n)
g <- exp(dat$f/5)

## negative binomial data... 
dat$y <- rnbinom(g,size=3,mu=g)
## known theta fit ...
b0 <- gam(y~s(x0)+s(x1)+s(x2)+s(x3),family=negbin(3),data=dat)
plot(b0,pages=1)
print(b0)

## same with theta estimation...
b <- gam(y~s(x0)+s(x1)+s(x2)+s(x3),family=nb(),data=dat)
plot(b,pages=1)
print(b)
b$family$getTheta(TRUE) ## extract final theta estimate


## another example...
set.seed(1)
f <- dat$f
f <- f - min(f)+5;g <- f^2/10
dat$y <- rnbinom(g,size=3,mu=g)
b2 <- gam(y~s(x0)+s(x1)+s(x2)+s(x3),family=nb(link="sqrt"),
          data=dat,method="REML") 
plot(b2,pages=1)
print(b2)
rm(dat)



###############################################
############# south tyrol dataset

nbgamC <-gam(frequency ~ date +s(rain_mean_0) + s(rain_max_0) + 
              s(rain_min_0) + s(rain_sd_0 ) + s(cmeanR_2d) + s(R.max.mean_2d)+ s(R.min.mean_2d)+ s(sdnormR_2d)+ 
 s(cmeanR_5d )+ s(R.max.mean_5d)+ s(R.min.mean_5d)+ s(sdnormR_25d)+ s(nDry_2d)+ s(nDry_5d)+ s(nDry_25d)+ s(snow)+ 
   s(nSnow_2d)+ s(nSnow_25d)+ s(temp_mean_0 )+ s(temp_max_0)+ s(temp_min_0)+ s(temp_sd_0 )+ s(T.max.mean_2d)+ 
  s(T.min.mean_2d)+ s(cnls_2d)+ s(sdnormT_2d)+ s(cmeant_5d)+ s(T.max.mean_5d)+ s(T.min.mean_5d)+ s(cnls_5d )+ 
   s(sdnormT_5d)+ s(cmeanT_25d)+ s(T.max.mean_25d)+ s(T.min.mean_25d)+ s(cnls_25d)+s(sdnormT_25d),
family = nb(), data = dfC)

#####################################

m_pois <-gam(frequency ~ date + s(rain_mean_0)+ s(temp_mean_0 ), family = poisson(), data = dfC, method = "REML")
summary(m_pois)
plot(m_pois)
gam.check(m_pois)


nbgam <-gam(frequency ~ date + s(rain_mean_0)+ s(temp_mean_0), family = nb(), data = dfC, method = "REML")
summary(nbgam)
plot(nbgam)
gam.check(nbgam)

#### Set k not higher than 100

nbgam <-gam(cnls_25d ~ date + s(rain_mean_0)+ s(temp_mean_0), family = nb(), data = dfC, method = "REML")
summary(nbgam)
plot(nbgam)
gam.check(nbgam)

############ bivariate Term

nbgam <-gam(frequency ~ date + s(rain_mean_0, temp_mean_0, bs = "tp" ) + 
              s(rain_mean_0, bs = "cc"), # cyclic smoother 
            family = nb(), data = dfC, method = "REML")
summary(nbgam)
plot(nbgam)
gam.check(nbgam)


##### Plot Residuals / Fitted Values

ggplot(data.frame(Fitted = fitted(nbgam), Resid = resid(nbgam)), aes(Fitted, Resid))+
       geom_point()


##### Check Overdipersion

root_pois <- rootogram(m_pois, style = "hanging", plot = FALSE)
root_nb   <- rootogram(nbgam, style = "hanging", plot = FALSE)
autoplot(root_nb)
autoplot(root_pois)
cowplot::plot_grid(autoplot(root_nb), autoplot(root_pois))

sum(residuals(m_pois, type = "pearson")^2) / df.residual(m_pois)

sum(residuals(nbgam, type = "pearson")^2) / df.residual(nbgam)


sim_fmnb <- DHARMa::simulateResiduals(nbgam, refit=T, n=99)
plotSimulatedResiduals(sim_fmnb)
testOverdispersion(sim_fmnb)







nbgamC <-gam(frequency ~ date +s(rain_mean_0) +
                              + s(rain_max_0) + 
                              s(rain_min_0) + 
                              s(rain_sd_0 ) + 
                              s(cmeanR_2d) + 
                              s(R.max.mean_2d) + 
                              s(R.min.mean_2d)+
                              s(sdnormR_2d)+
                              s(cmeanR_5d )+ 
                              s(R.max.mean_5d)+
                              s(R.min.mean_5d)+ 
                              s(sdnormR_25d)+
                              s(temp_mean_0 )+ 
                              s(temp_max_0)+ 
                              s(temp_min_0)+ 
                              s(temp_sd_0 )+ 
                              s(T.max.mean_2d)+ 
                              s(T.min.mean_2d)+  
                              s(sdnormT_2d)+ 
                              s(cmeant_5d)+ 
                              s(T.max.mean_5d)+ 
                              s(T.min.mean_5d)+ 
                              s(sdnormT_5d)+ 
                              s(cmeanT_25d)+ 
                              s(T.max.mean_25d)+ 
                              s(T.min.mean_25d)+ 
                              s(sdnormT_25d)+
                              s(nDry_25d),
                              
             family = nb(3), data = dfC)

nbgamC25 <-gam(cnls_25d ~ date +s(rain_mean_0) 
             + s(rain_max_0) + 
               s(rain_min_0) + 
               s(rain_sd_0 ) + 
               s(cmeanR_2d) + 
               s(R.max.mean_2d) + 
               s(R.min.mean_2d)+
               s(sdnormR_2d)+
               s(cmeanR_5d )+ 
               s(R.max.mean_5d)+
               s(R.min.mean_5d)+ 
               s(sdnormR_25d)+
               s(temp_mean_0 )+ 
               s(temp_max_0)+ 
               s(temp_min_0)+ 
               s(temp_sd_0 )+ 
               s(T.max.mean_2d)+ 
               s(T.min.mean_2d)+  
               s(sdnormT_2d)+ 
               s(cmeant_5d)+ 
               s(T.max.mean_5d)+ 
               s(T.min.mean_5d)+ 
               s(sdnormT_5d)+ 
               s(cmeanT_25d)+ 
               s(T.max.mean_25d)+ 
               s(T.min.mean_25d)+ 
               s(sdnormT_25d),
             
             family = nb(), data = dfC)
