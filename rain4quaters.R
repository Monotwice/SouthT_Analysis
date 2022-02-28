#######################
### CENTER Rain 

rainC_1Q <- rain_ex(startday = "1999-12-08", 
                    endday = "2005-01-01", catchment_sf = center)
save.image("rain4quaters.RData")
.rs.restartR()

rainC_2Q <- rain_ex(startday = "2005-01-03", 
                    endday = "2010-01-01", catchment_sf = center)
save.image("rain4quaters.RData")
.rs.restartR()

rainC_3Q <- rain_ex(startday = "2010-01-03", 
                    endday = "2015-01-01", catchment_sf = center)
save.image("rain4quaters.RData")
.rs.restartR()

rainC_4Q <- rain_ex(startday = "2015-01-03", 
                    endday = "2020-01-01", catchment_sf = center)
save.image("rain4quaters.RData")
.rs.restartR()

rainC_5Q <- rain_ex(startday = "2020-01-03", 
                    endday = "2020-12-30", catchment_sf = center)
save.image("rain4quaters.RData")
.rs.restartR()

#####################
rain <- rbind(rainC_1Q,rainC_2Q, rainC_3Q, rainC_4Q, rainC_5Q)
#####################################################

# function normalised days
normSD <- function(x){
  # x: mean precip. or temp for given days
  norm_x <- x/sum(x)
  norm_sd <- sd(norm_x)
}

#calculate parameters days 0-1
setDT(rain)[, cmeanR_2d := round(frollsum(rain_mean_0, 2),2)]
setDT(rain)[, R.max.mean_2d := round(frollapply(rain_mean_0, 2, FUN = max),2)]
setDT(rain)[, R.min.mean_2d := round(frollapply(rain_mean_0, 2, FUN = min),2)]
setDT(rain)[, sdnormR_2d := round(frollapply(rain_mean_0, 2, FUN = normSD), 2)]


#calculate parameters days 0-5
setDT(rain)[, cmeanR_5d := round(frollsum(rain_mean_0, 5),2)]
setDT(rain)[, R.max.mean_5d := round(frollapply(rain_mean_0, 5, FUN = max),2)]
setDT(rain)[, R.min.mean_5d := round(frollapply(rain_mean_0, 5, FUN = min),2)]
setDT(rain)[, sdnormR_5d := round(frollapply(rain_mean_0, 5, FUN = normSD), 2)]

#calculate parameters days 0-25
setDT(rain)[, cmeanR_25d := round(frollsum(rain_mean_0, 25),2)]
setDT(rain)[, R.max.mean_25d := round(frollapply(rain_mean_0, 25, FUN = max),2)]
setDT(rain)[, R.min.mean_25d := round(frollapply(rain_mean_0, 25, FUN = min),2)]
setDT(rain)[, sdnormR_25d := round(frollapply(rain_mean_0, 25, FUN = normSD), 2)]

# calculate dry Days
rain$dryday= as.numeric(lapply(rain$rain_mean_0, function(x){
  fifelse(x < 1.1, 1, 0)
}))

setDT(rain)[, nDry_2d := frollsum(rain$dryday, 2)]
setDT(rain)[, nDry_5d := frollsum(rain$dryday, 5)]
setDT(rain)[, nDry_25d := frollsum(rain$dryday, 25)]

rainC <- rain
save.image("rain4quaters.RData")


###########################################################################
##### Merge with Temperature
df <- merge(tempC, rainC, by = "date")
df$snow <- fifelse(df$rain_mean_0 > 1.1 & df$temp_mean_0 < 2, 1, 0)

setDT(df)[, nSnow_2d := frollsum(df$snow, 2)]
setDT(df)[, nSnow_5d := frollsum(df$snow, 5)]
setDT(df)[, nSnow_25d := frollsum(df$snow, 25)]

dfC <- df#[-c(1:25),]





############################################################################
###########################################################################
###########################################################################
############################################################################
#############################################################################
#WEST Rain


rainW_1Q <- rain_ex(startday = "1999-12-08", 
                    endday = "2005-01-01", catchment_sf = west)
save.image("rain4quaters.RData")
.rs.restartR()

rainW_2Q <- rain_ex(startday = "2005-01-03", 
                    endday = "2010-01-01", catchment_sf = west)
save.image("rain4quaters.RData")
.rs.restartR()

rainW_3Q <- rain_ex(startday = "2010-01-03", 
                    endday = "2015-01-01", catchment_sf = west)
save.image("rain4quaters.RData")
.rs.restartR()

rainW_4Q <- rain_ex(startday = "2015-01-03", 
                    endday = "2020-01-01", catchment_sf = west)
save.image("rain4quaters.RData")
.rs.restartR()

rainW_5Q <- rain_ex(startday = "2020-01-03", 
                    endday = "2020-12-30", catchment_sf = west)
save.image("rain4quaters.RData")
.rs.restartR()

#####################
rain <- rbind(rainW_1Q,rainW_2Q, rainW_3Q, rainW_4Q, rainW_5Q)

#####################################################


# function normalised days
normSD <- function(x){
  # x: mean precip. or temp for given days
  norm_x <- x/sum(x)
  norm_sd <- sd(norm_x)
}

#calculate parameters days 0-1
setDT(rain)[, cmeanR_2d := round(frollsum(rain_mean_0, 2),2)]
setDT(rain)[, R.max.mean_2d := round(frollapply(rain_mean_0, 2, FUN = max),2)]
setDT(rain)[, R.min.mean_2d := round(frollapply(rain_mean_0, 2, FUN = min),2)]
setDT(rain)[, sdnormR_2d := round(frollapply(rain_mean_0, 2, FUN = normSD), 2)]


#calculate parameters days 0-5
setDT(rain)[, cmeanR_5d := round(frollsum(rain_mean_0, 5),2)]
setDT(rain)[, R.max.mean_5d := round(frollapply(rain_mean_0, 5, FUN = max),2)]
setDT(rain)[, R.min.mean_5d := round(frollapply(rain_mean_0, 5, FUN = min),2)]
setDT(rain)[, sdnormR_5d := round(frollapply(rain_mean_0, 5, FUN = normSD), 2)]

#calculate parameters days 0-25
setDT(rain)[, cmeanR_25d := round(frollsum(rain_mean_0, 25),2)]
setDT(rain)[, R.max.mean_25d := round(frollapply(rain_mean_0, 25, FUN = max),2)]
setDT(rain)[, R.min.mean_25d := round(frollapply(rain_mean_0, 25, FUN = min),2)]
setDT(rain)[, sdnormR_25d := round(frollapply(rain_mean_0, 25, FUN = normSD), 2)]

# calculate dry Days
rain$dryday= as.numeric(lapply(rain$rain_mean_0, function(x){
  fifelse(x < 1.1, 1, 0)
}))

setDT(rain)[, nDry_2d := frollsum(rain$dryday, 2)]
setDT(rain)[, nDry_5d := frollsum(rain$dryday, 5)]
setDT(rain)[, nDry_25d := frollsum(rain$dryday, 25)]

rainW <- rain
save.image("rain4quaters.RData")


###########################################################################
##### Merge with Temperature
df <- merge(tempW, rainW, by = "date")
df$snow <- fifelse(df$rain_mean_0 > 1.1 & df$temp_mean_0 < 2, 1, 0)

setDT(df)[, nSnow_2d := frollsum(df$snow, 2)]
setDT(df)[, nSnow_5d := frollsum(df$snow, 5)]
setDT(df)[, nSnow_25d := frollsum(df$snow, 25)]

dfW <- df
save.image("rain4quaters.RData")






############################################################################
###########################################################################
###########################################################################
############################################################################
#############################################################################
#EAST Rain


rainE_1Q <- rain_ex(startday = "1999-12-08", 
                    endday = "2005-01-01", catchment_sf = east)
save.image("rain4quaters.RData")
.rs.restartR()

rainE_2Q <- rain_ex(startday = "2005-01-03", 
                    endday = "2010-01-01", catchment_sf = east)
save.image("rain4quaters.RData")
.rs.restartR()

rainE_3Q <- rain_ex(startday = "2010-01-03", 
                    endday = "2015-01-01", catchment_sf = east)
save.image("rain4quaters.RData")
.rs.restartR()

rainE_4Q <- rain_ex(startday = "2015-01-03", 
                    endday = "2020-01-01", catchment_sf = east)
save.image("rain4quaters.RData")
.rs.restartR()

rainE_5Q <- rain_ex(startday = "2020-01-03", 
                    endday = "2020-12-30", catchment_sf = east)
save.image("rain4quaters.RData")
.rs.restartR()

#####################
rain <- rbind(rainE_1Q,rainE_2Q, rainE_3Q, rainE_4Q, rainE_5Q)

#####################################################

#calculate parameters days 0-1
setDT(rain)[, cmeanR_2d := round(frollsum(rain_mean_0, 2),2)]
setDT(rain)[, R.max.mean_2d := round(frollapply(rain_mean_0, 2, FUN = max),2)]
setDT(rain)[, R.min.mean_2d := round(frollapply(rain_mean_0, 2, FUN = min),2)]
setDT(rain)[, sdnormR_2d := round(frollapply(rain_mean_0, 2, FUN = normSD), 2)]


#calculate parameters days 0-5
setDT(rain)[, cmeanR_5d := round(frollsum(rain_mean_0, 5),2)]
setDT(rain)[, R.max.mean_5d := round(frollapply(rain_mean_0, 5, FUN = max),2)]
setDT(rain)[, R.min.mean_5d := round(frollapply(rain_mean_0, 5, FUN = min),2)]
setDT(rain)[, sdnormR_5d := round(frollapply(rain_mean_0, 5, FUN = normSD), 2)]

#calculate parameters days 0-25
setDT(rain)[, cmeanR_25d := round(frollsum(rain_mean_0, 25),2)]
setDT(rain)[, R.max.mean_25d := round(frollapply(rain_mean_0, 25, FUN = max),2)]
setDT(rain)[, R.min.mean_25d := round(frollapply(rain_mean_0, 25, FUN = min),2)]
setDT(rain)[, sdnormR_25d := round(frollapply(rain_mean_0, 25, FUN = normSD), 2)]

# calculate dry Days
rain$dryday= as.numeric(lapply(rain$rain_mean_0, function(x){
  fifelse(x < 1.1, 1, 0)
}))

setDT(rain)[, nDry_2d := frollsum(rain$dryday, 2)]
setDT(rain)[, nDry_5d := frollsum(rain$dryday, 5)]
setDT(rain)[, nDry_25d := frollsum(rain$dryday, 25)]

rainE <- rain
save.image("rain4quaters.RData")


###########################################################################
##### Merge with Temperature
df <- merge(tempE, rainE, by = "date")
df$snow <- fifelse(df$rain_mean_0 > 1.1 & df$temp_mean_0 < 2, 1, 0)

setDT(df)[, nSnow_2d := frollsum(df$snow, 2)]
setDT(df)[, nSnow_5d := frollsum(df$snow, 5)]
setDT(df)[, nSnow_25d := frollsum(df$snow, 25)]

dfE <- df
save.image("rain4quaters.RData")





############################################################################
###########################################################################
###########################################################################
############################################################################
#############################################################################
#SOUTH

rainS_1Q <- rain_ex(startday = "1999-12-08", 
                    endday = "2005-01-01", catchment_sf = south)
save.image("rain4quaters.RData")
.rs.restartR()

rainS_2Q <- rain_ex(startday = "2005-01-03", 
                    endday = "2010-01-01", catchment_sf = south)
save.image("rain4quaters.RData")
.rs.restartR()

rainS_3Q <- rain_ex(startday = "2010-01-03", 
                    endday = "2015-01-01", catchment_sf = south)
save.image("rain4quaters.RData")
.rs.restartR()

rainS_4Q <- rain_ex(startday = "2015-01-03", 
                    endday = "2020-01-01", catchment_sf = south)
save.image("rain4quaters.RData")
.rs.restartR()

rainS_5Q <- rain_ex(startday = "2020-01-03", 
                    endday = "2020-12-30", catchment_sf = south)
save.image("rain4quaters.RData")
.rs.restartR()

#####################
rain <- rbind(rainS_1Q,rainS_2Q, rainS_3Q, rainS_4Q, rainS_5Q)
#####################################################


#calculate parameters days 0-1
setDT(rain)[, cmeanR_2d := round(frollsum(rain_mean_0, 2),2)]
setDT(rain)[, R.max.mean_2d := round(frollapply(rain_mean_0, 2, FUN = max),2)]
setDT(rain)[, R.min.mean_2d := round(frollapply(rain_mean_0, 2, FUN = min),2)]
setDT(rain)[, sdnormR_2d := round(frollapply(rain_mean_0, 2, FUN = normSD), 2)]


#calculate parameters days 0-5
setDT(rain)[, cmeanR_5d := round(frollsum(rain_mean_0, 5),2)]
setDT(rain)[, R.max.mean_5d := round(frollapply(rain_mean_0, 5, FUN = max),2)]
setDT(rain)[, R.min.mean_5d := round(frollapply(rain_mean_0, 5, FUN = min),2)]
setDT(rain)[, sdnormR_5d := round(frollapply(rain_mean_0, 5, FUN = normSD), 2)]

#calculate parameters days 0-25
setDT(rain)[, cmeanR_25d := round(frollsum(rain_mean_0, 25),2)]
setDT(rain)[, R.max.mean_25d := round(frollapply(rain_mean_0, 25, FUN = max),2)]
setDT(rain)[, R.min.mean_25d := round(frollapply(rain_mean_0, 25, FUN = min),2)]
setDT(rain)[, sdnormR_25d := round(frollapply(rain_mean_0, 25, FUN = normSD), 2)]

# calculate dry Days
rain$dryday= as.numeric(lapply(rain$rain_mean_0, function(x){
  fifelse(x < 1.1, 1, 0)
}))

setDT(rain)[, nDry_2d := frollsum(rain$dryday, 2)]
setDT(rain)[, nDry_5d := frollsum(rain$dryday, 5)]
setDT(rain)[, nDry_25d := frollsum(rain$dryday, 25)]

rainS <- rain
save.image("rain4quaters.RData")


###########################################################################
##### Merge with Temperature
df <- merge(tempS, rainS, by = "date")
df$snow <- fifelse(df$rain_mean_0 > 1.1 & df$temp_mean_0 < 2, 1, 0)

setDT(df)[, nSnow_2d := frollsum(df$snow, 2)]
setDT(df)[, nSnow_5d := frollsum(df$snow, 5)]
setDT(df)[, nSnow_25d := frollsum(df$snow, 25)]

dfS <- df
save.image("rain4quaters.RData")


