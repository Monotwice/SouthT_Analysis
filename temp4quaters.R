######################
# Frequency data C
lsC_freq <-landslide_freq("2000-01-01", "2020-12-30", landslide_sf = lsC )

#delete 2021
lsC_freq <- lsC_freq[-c(7671:7672),]


#######################
### Temperature Data

tempC_1Q <- temp_ex(startday = "2000-01-02", 
                endday = "2005-01-01", catchment_sf = center)
save.image("temp4quaters.RData")
.rs.restartR()

tempC_2Q <- temp_ex(startday = "2005-01-03", 
                    endday = "2010-01-01", catchment_sf = center)
save.image("temp4quaters.RData")
.rs.restartR()

tempC_3Q <- temp_ex(startday = "2010-01-03", 
                    endday = "2015-01-01", catchment_sf = center)
save.image("temp4quaters.RData")
.rs.restartR()

tempC_4Q <- temp_ex(startday = "2015-01-03", 
                    endday = "2020-01-01", catchment_sf = center)
save.image("temp4quaters.RData")
.rs.restartR()

tempC_5Q <- temp_ex(startday = "2020-01-03", 
                    endday = "2020-12-30", catchment_sf = center)
save.image("temp4quaters.RData")
.rs.restartR()

#####################
temp <- rbind(tempC_1Q,tempC_2Q, tempC_3Q, tempC_4Q, tempC_5Q)
temp <- merge(lsC_freq, temp, by = "date", all = T)
#####################################################

# function normalised days
normSD <- function(x){
  # x: mean precip. or temp for given days
  norm_x <- x/sum(x)
  norm_sd <- sd(norm_x)
}

#calculate parameters days 0-1
setDT(temp)[, cmeanT_2d := round(frollsum(temp_mean_0, 2),2)]
setDT(temp)[, T.max.mean_2d := round(frollapply(temp_mean_0, 2, FUN = max),2)]
setDT(temp)[, T.min.mean_2d := round(frollapply(temp_mean_0, 2, FUN = min),2)]
setDT(temp)[, cnls_2d:= frollsum(frequency, 2)]
setDT(temp)[, sdnormT_2d := round(frollapply(temp_mean_0, 2, FUN = normSD), 2)]


#calculate parameters days 0-5
setDT(temp)[, cmeant_5d := round(frollsum(temp_mean_0, 5),2)]
setDT(temp)[, T.max.mean_5d := round(frollapply(temp_mean_0, 5, FUN = max),2)]
setDT(temp)[, T.min.mean_5d := round(frollapply(temp_mean_0, 5, FUN = min),2)]
setDT(temp)[, cnls_5d:= frollsum(frequency, 5)]
setDT(temp)[, sdnormT_5d := round(frollapply(temp_mean_0, 5, FUN = normSD), 2)]

#calculate parameters days 0-25
setDT(temp)[, cmeanT_25d := round(frollsum(temp_mean_0, 25),2)]
setDT(temp)[, T.max.mean_25d := round(frollapply(temp_mean_0, 25, FUN = max),2)]
setDT(temp)[, T.min.mean_25d := round(frollapply(temp_mean_0, 25, FUN = min),2)]
setDT(temp)[, cnls_25d:= frollsum(frequency, 25)]
setDT(temp)[, sdnormT_25d := round(frollapply(temp_mean_0, 25, FUN = normSD), 2)]

tempC <- temp

save.image("temp4quaters.RData")


############################################################################
###########################################################################
###########################################################################
############################################################################
#############################################################################
#WEST

######################
# Frequency data W
lsW_freq <-landslide_freq("2000-01-01", "2020-12-30", landslide_sf = lsW )
tail(lsW_freq)


#######################
### Temperature Data

tempW_1Q <- temp_ex(startday = "2000-01-02", 
                    endday = "2005-01-01", catchment_sf = west)
save.image("temp4quaters.RData")
.rs.restartR()

tempW_2Q <- temp_ex(startday = "2005-01-03", 
                    endday = "2010-01-01", catchment_sf = west)
save.image("temp4quaters.RData")
.rs.restartR()

tempW_3Q <- temp_ex(startday = "2010-01-03", 
                    endday = "2015-01-01", catchment_sf = west)
save.image("temp4quaters.RData")
.rs.restartR()

tempW_4Q <- temp_ex(startday = "2015-01-03", 
                    endday = "2020-01-01", catchment_sf = west)
save.image("temp4quaters.RData")
.rs.restartR()

tempW_5Q <- temp_ex(startday = "2020-01-03", 
                    endday = "2020-12-30", catchment_sf = west)
save.image("temp4quaters.RData")
.rs.restartR()

#####################
temp <- rbind(tempW_1Q,tempW_2Q, tempW_3Q, tempW_4Q, tempW_5Q)
temp <- merge(lsW_freq, temp, by = "date", all = T)
#####################################################

# function normalised days
normSD <- function(x){
  # x: mean precip. or temp for given days
  norm_x <- x/sum(x)
  norm_sd <- sd(norm_x)
}

#calculate parameters days 0-1
setDT(temp)[, cmeanT_2d := round(frollsum(temp_mean_0, 2),2)]
setDT(temp)[, T.max.mean_2d := round(frollapply(temp_mean_0, 2, FUN = max),2)]
setDT(temp)[, T.min.mean_2d := round(frollapply(temp_mean_0, 2, FUN = min),2)]
setDT(temp)[, cnls_2d:= frollsum(frequency, 2)]
setDT(temp)[, sdnormT_2d := round(frollapply(temp_mean_0, 2, FUN = normSD), 2)]


#calculate parameters days 0-5
setDT(temp)[, cmeant_5d := round(frollsum(temp_mean_0, 5),2)]
setDT(temp)[, T.max.mean_5d := round(frollapply(temp_mean_0, 5, FUN = max),2)]
setDT(temp)[, T.min.mean_5d := round(frollapply(temp_mean_0, 5, FUN = min),2)]
setDT(temp)[, cnls_5d:= frollsum(frequency, 5)]
setDT(temp)[, sdnormT_5d := round(frollapply(temp_mean_0, 5, FUN = normSD), 2)]

#calculate parameters days 0-25
setDT(temp)[, cmeanT_25d := round(frollsum(temp_mean_0, 25),2)]
setDT(temp)[, T.max.mean_25d := round(frollapply(temp_mean_0, 25, FUN = max),2)]
setDT(temp)[, T.min.mean_25d := round(frollapply(temp_mean_0, 25, FUN = min),2)]
setDT(temp)[, cnls_25d:= frollsum(frequency, 25)]
setDT(temp)[, sdnormT_25d := round(frollapply(temp_mean_0, 25, FUN = normSD), 2)]

tempW <- temp

save.image("temp4quaters.RData")


############################################################################
###########################################################################
###########################################################################
############################################################################
#############################################################################
#EAST

######################
# Frequency data
lsE_freq <-landslide_freq("2000-01-01", "2020-12-30", landslide_sf = lsE )

#delete 2021
#lsC_freq <- lsC_freq[-c(7671:7672),]


#######################
### Temperature Data

tempE_1Q <- temp_ex(startday = "2000-01-02", 
                    endday = "2005-01-01", catchment_sf = east)
save.image("temp4quaters.RData")
.rs.restartR()

tempE_2Q <- temp_ex(startday = "2005-01-03", 
                    endday = "2010-01-01", catchment_sf = east)
save.image("temp4quaters.RData")
.rs.restartR()

tempE_3Q <- temp_ex(startday = "2010-01-03", 
                    endday = "2015-01-01", catchment_sf = east)
save.image("temp4quaters.RData")
.rs.restartR()

tempE_4Q <- temp_ex(startday = "2015-01-03", 
                    endday = "2020-01-01", catchment_sf = east)
save.image("temp4quaters.RData")
.rs.restartR()

tempE_5Q <- temp_ex(startday = "2020-01-03", 
                    endday = "2020-12-30", catchment_sf = east)
save.image("temp4quaters.RData")
.rs.restartR()

#####################
temp <- rbind(tempE_1Q,tempE_2Q, tempE_3Q, tempE_4Q, tempE_5Q)
temp <- merge(lsE_freq, temp, by = "date", all = T)
#####################################################

# function normalised days
normSD <- function(x){
  # x: mean precip. or temp for given days
  norm_x <- x/sum(x)
  norm_sd <- sd(norm_x)
}

#calculate parameters days 0-1
setDT(temp)[, cmeanT_2d := round(frollsum(temp_mean_0, 2),2)]
setDT(temp)[, T.max.mean_2d := round(frollapply(temp_mean_0, 2, FUN = max),2)]
setDT(temp)[, T.min.mean_2d := round(frollapply(temp_mean_0, 2, FUN = min),2)]
setDT(temp)[, cnls_2d:= frollsum(frequency, 2)]
setDT(temp)[, sdnormT_2d := round(frollapply(temp_mean_0, 2, FUN = normSD), 2)]


#calculate parameters days 0-5
setDT(temp)[, cmeant_5d := round(frollsum(temp_mean_0, 5),2)]
setDT(temp)[, T.max.mean_5d := round(frollapply(temp_mean_0, 5, FUN = max),2)]
setDT(temp)[, T.min.mean_5d := round(frollapply(temp_mean_0, 5, FUN = min),2)]
setDT(temp)[, cnls_5d:= frollsum(frequency, 5)]
setDT(temp)[, sdnormT_5d := round(frollapply(temp_mean_0, 5, FUN = normSD), 2)]

#calculate parameters days 0-25
setDT(temp)[, cmeanT_25d := round(frollsum(temp_mean_0, 25),2)]
setDT(temp)[, T.max.mean_25d := round(frollapply(temp_mean_0, 25, FUN = max),2)]
setDT(temp)[, T.min.mean_25d := round(frollapply(temp_mean_0, 25, FUN = min),2)]
setDT(temp)[, cnls_25d:= frollsum(frequency, 25)]
setDT(temp)[, sdnormT_25d := round(frollapply(temp_mean_0, 25, FUN = normSD), 2)]

tempE <- temp

save.image("temp4quaters.RData")

############################################################################
###########################################################################
###########################################################################
############################################################################
#############################################################################
#SOUTH

######################
# Frequency data
lsS_freq <-landslide_freq("2000-01-01", "2020-12-30", landslide_sf = lsS )

#delete 2021
#lsC_freq <- lsC_freq[-c(7671:7672),]


#######################
### Temperature Data

tempS_1Q <- temp_ex(startday = "2000-01-02", 
                    endday = "2005-01-01", catchment_sf = south)
save.image("temp4quaters.RData")
.rs.restartR()

tempS_2Q <- temp_ex(startday = "2005-01-03", 
                    endday = "2010-01-01", catchment_sf = south)
save.image("temp4quaters.RData")
.rs.restartR()

tempS_3Q <- temp_ex(startday = "2010-01-03", 
                    endday = "2015-01-01", catchment_sf = south)
save.image("temp4quaters.RData")
.rs.restartR()

tempS_4Q <- temp_ex(startday = "2015-01-03", 
                    endday = "2020-01-01", catchment_sf = south)
save.image("temp4quaters.RData")
.rs.restartR()

tempS_5Q <- temp_ex(startday = "2020-01-03", 
                    endday = "2020-12-30", catchment_sf = south)
save.image("temp4quaters.RData")
.rs.restartR()

#####################
temp <- rbind(tempS_1Q,tempS_2Q, tempS_3Q, tempS_4Q, tempS_5Q)
temp <- merge(lsS_freq, temp, by = "date", all = T)
#####################################################

# function normalised days
normSD <- function(x){
  # x: mean precip. or temp for given days
  norm_x <- x/sum(x)
  norm_sd <- sd(norm_x)
}

#calculate parameters days 0-1
setDT(temp)[, cmeanT_2d := round(frollsum(temp_mean_0, 2),2)]
setDT(temp)[, T.max.mean_2d := round(frollapply(temp_mean_0, 2, FUN = max),2)]
setDT(temp)[, T.min.mean_2d := round(frollapply(temp_mean_0, 2, FUN = min),2)]
setDT(temp)[, cnls_2d:= frollsum(frequency, 2)]
setDT(temp)[, sdnormT_2d := round(frollapply(temp_mean_0, 2, FUN = normSD), 2)]


#calculate parameters days 0-5
setDT(temp)[, cmeant_5d := round(frollsum(temp_mean_0, 5),2)]
setDT(temp)[, T.max.mean_5d := round(frollapply(temp_mean_0, 5, FUN = max),2)]
setDT(temp)[, T.min.mean_5d := round(frollapply(temp_mean_0, 5, FUN = min),2)]
setDT(temp)[, cnls_5d:= frollsum(frequency, 5)]
setDT(temp)[, sdnormT_5d := round(frollapply(temp_mean_0, 5, FUN = normSD), 2)]

#calculate parameters days 0-25
setDT(temp)[, cmeanT_25d := round(frollsum(temp_mean_0, 25),2)]
setDT(temp)[, T.max.mean_25d := round(frollapply(temp_mean_0, 25, FUN = max),2)]
setDT(temp)[, T.min.mean_25d := round(frollapply(temp_mean_0, 25, FUN = min),2)]
setDT(temp)[, cnls_25d:= frollsum(frequency, 25)]
setDT(temp)[, sdnormT_25d := round(frollapply(temp_mean_0, 25, FUN = normSD), 2)]

tempS <- temp

save.image("temp4quaters.RData")


