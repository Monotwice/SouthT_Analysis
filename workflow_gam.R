library(mgcv)
library(countreg)
library(MASS)
library(DHARMa)
library(mgcViz)
library(corrplot)
library(ggcorrplot)

##############
# Function Korrelation Matrix
#Select relevant variables

varRT <- c("rain_mean_0" , 
         "rain_max_0", 
          "rain_min_0" , 
          "cmeanR_2d" ,  
          "R.max.mean_2d" , 
          "R.min.mean_2d",                  
          "cmeanR_5d ", 
           "R.max.mean_5d",                   
          "R.min.mean_5d", 
          "cmeanR_25d", 
         "R.max.mean_25d",
         "R.min.mean_25d",
         
          "temp_mean_0 ",                  
          "temp_max_0",                  
           "temp_min_0",                    
            "T.max.mean_2d",                   
            "T.min.mean_2d",                   
             "cmeant_5d",                  
             "T.max.mean_5d",                  
             "T.min.mean_5d",                  
              "cmeanT_25d",                 
              "T.max.mean_25d", 
               "T.min.mean_25d",                
                "sdnormT_25d",
         
                 "nDry_25d",
)

corr_matrix <- function(aoi = NULL, area)
{
  num <<- select_if(aoi, is.numeric)
  num[is.na(num)] <<- 0
  matrix <- ggcorrplot(cor(num), tl.cex = 10, tl.srt = 90, 
                       title = paste0("Correlation of predictors in ", area))
  return(matrix)
}

corrmatrixT <- corr_matrix(dfT, "South Tyrol (study area)")
corrmatrixT

corrmatrixC <- corr_matrix(dfC, "Center Part of South Tyrol")
corrmatrixC

########################################################
###########
n <- cor(num)
n[n < 0.8] <- NA
ggcorrplot(n)



################################################
## 

library(ggstatsplot)

# correlogram
ggstatsplot::ggcorrmat(
  data = num,
  type = "parametric", # parametric for Pearson, nonparametric for Spearman's correlation
  colors = c("darkred", "white", "steelblue"),
  tl.cex = 10, tl.srt = 90# change default colors
)

library(lares)

corr_cross(num, # name of dataset
           max_pvalue = 0.05, # display only significant correlations (at 5% level)
           top = 200 # display top 10 couples of variables (by correlation coefficient)
)

