library(pacman)

p_load(raster)
p_load(sf)
p_load(sp)
p_load(dplyr)
p_load(rgdal)
p_load(ggplot2)
p_load(rainfallR)
p_load(stats)

p_load(lubridate)
p_load(tidyverse)
p_load(astsa)
p_load(forecast)
p_load(readxl)
p_load(urca)
p_load(ggfortify)
p_load(tsutils)
p_load(mapview)
p_load(stars)

p_load(ncdf4)
p_load(tidyverse)
p_load(ncdump)
p_load(sf)
p_load(parallel)
p_load(data.table)

####################### Input data landslides and aoi
nc <- brick("PREC_GRIDS_MASK/2020/DAILYPCP_202010.nc")

ls <- read_rds("spacetime.Rds")
ls <- st_transform(ls, crs = proj4string(nc))

c <- sf::st_read("UnitsForFrequency/Units_4fold.shp") %>% 
  st_as_sf() %>%
  st_transform(c, crs = proj4string(nc))

######################################
ls$count= as.numeric(lapply(ls$day_prs, function(x){
  fifelse(x == "TRUE", 1, 0)
}))


##############
#split and aoiinto single sf


center <- c[c$NAME_DE == "Center",] %>% na.omit 
lsC <- st_intersection(center, ls)

west <- c[c$NAME_DE == "West",] %>% na.omit 
lsW <- st_intersection(west, ls)

east <- c[c$NAME_DE == "East",] %>% na.omit 
lsE <- st_intersection(east, ls)

south <- c[c$NAME_DE == "South",] %>% na.omit 
lsS <- st_intersection(south, ls)



##############################################
#file path

temp_data_path <- "TEMP_GRIDS_MASK/"

##############################################
# function frequency landslide timeline
landslide_freq <- function(startday = NULL, 
                           endday = NULL,
                           landslide_sf = NULL)
{
  
  startday = as.Date(startday)
  endday = as.Date(endday)
  freq_byday <- aggregate(count~date, # aggregate frequency of landslides
                          data=landslide_sf,FUN=sum)
  freq <- merge(freq_byday, data.frame(date=seq(startday, endday, "days")), all=T)
  colnames(freq) <- c("date", "frequency")
  freq[is.na(freq)] <- 0
  
  return(freq)
}

##############################################
# function extraction temperature

temp_ex <- function(startday = NULL, 
                    endday = NULL, 
                    catchment_sf = NULL)
{
  # create timeline
  startday = as.Date(startday)
  endday = as.Date(endday)
  seq_days = data.frame(date=seq(startday, endday, "days"))
  
  
  # extract values
  days <- (length(seq_days$date))
  
  data <- ex_rainfall(
    data_path = temp_data_path,
    spatial.obj = catchment_sf,
    fun = c("mean", "min", "stdev", "max"),
    date = endday,
    nc_var = "temperature",
    days_back = days
  ) 
  
  mean <- data[data$fun == "mean", ]
  max <- data[data$fun == "max", ]
  min <- data[data$fun == "min", ]
  stdev <- data[data$fun == "stdev", ]
  
  temp_mean_0 <- mean %>% 
    select(c(date, precip)) %>% 
    st_drop_geometry
  colnames(temp_mean_0) <- c("date", "temp_mean_0")
  
  temp_max_0 <- max %>% 
    select(c(date, precip)) %>% 
    st_drop_geometry
  colnames(temp_max_0) <- c("date", "temp_max_0")
  
  temp_min_0 <- min %>% 
    select(c(date, precip)) %>% 
    st_drop_geometry
  colnames(temp_min_0) <- c("date", "temp_min_0")
  
  temp_sd_0 <- stdev %>% 
    select(c(date, precip)) %>% 
    st_drop_geometry
  colnames(temp_sd_0) <- c("date", "temp_sd_0")
  
  #merge data
  temp_0 <- merge(temp_mean_0, temp_max_0, by = "date")
  temp_0 <- merge(temp_0, temp_min_0, by = "date")
  temp_0 <- merge(temp_0, temp_sd_0, by = "date")
  
  
  return(temp_0)
}


###################################################################################
##### Precipitation


rain_data_path <- "PREC_GRIDS_MASK/"


##############
#### Precipitation data extraction function


rain_ex <- function(startday = NULL, 
                    endday = NULL, 
                    catchment_sf = NULL)
{
  # create timeline
  startday = as.Date(startday)
  endday = as.Date(endday)
  seq_days = data.frame(date=seq(startday, endday, "days"))
  
  
  # extract values
  days <- (length(seq_days$date))
  
  data <- ex_rainfall(
    data_path = rain_data_path,
    spatial.obj = catchment_sf,
    fun = c("mean", "min", "stdev", "max"),
    date = endday,
    nc_var = "precipitation",
    days_back = days
  )
  
  mean <- data[data$fun == "mean", ]
  max <- data[data$fun == "max", ]
  min <- data[data$fun == "min", ]
  stdev <- data[data$fun == "stdev", ]
  
  rain_mean_0 <- mean %>% 
    select(c(date, precip)) %>% 
    st_drop_geometry
  colnames(rain_mean_0) <- c("date", "rain_mean_0")
  
  rain_max_0 <- max %>% 
    select(c(date, precip)) %>% 
    st_drop_geometry
  colnames(rain_max_0) <- c("date", "rain_max_0")
  
  rain_min_0 <- min %>% 
    select(c(date, precip)) %>% 
    st_drop_geometry
  colnames(rain_min_0) <- c("date", "rain_min_0")
  
  rain_sd_0 <- stdev %>% 
    select(c(date, precip)) %>% 
    st_drop_geometry
  colnames(rain_sd_0) <- c("date", "rain_sd_0")
  
  #merge data
  rain_0 <- merge(rain_mean_0, rain_max_0, by = "date")
  rain_0 <- merge(rain_0, rain_min_0, by = "date")
  rain_0 <- merge(rain_0, rain_sd_0, by = "date")
  
  return(rain_0)
}





