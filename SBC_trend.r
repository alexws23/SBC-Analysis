library (poptrend)
library (tidyverse)
library (data.table)

rm(list = ls())
setwd("C:/Users/awsmilor/Git/Ward Lab/SBC-Analysis/Data")
data <- read.csv("./SBC_all_data.csv", header = TRUE) %>% 
  select(-c(X)) %>%      #Drop extraneous column
  rename(county = coun)

##### Subset the species ################################################################
spp <- "Canada Warbler"
space <- "_"
log <- "log"
smooth <- "smooth"
trend <- "trend"
pdf <- ".pdf"
ppp <- paste (spp, sep ="")
ppp_log <- paste (spp,space,log,pdf,sep="")
ppp_smooth <- paste (spp,space,smooth,pdf,sep="")
trend <- paste (spp,space,trend,sep="")

data_2 <- subset(data, Common_Name == spp)
#### from 1980 to 2021
data_2 <- subset(data_2, year > 1972)
### use this code to pull specific counties
#data_3 <- data_3 %>%
filter(county %in% c("LaSalle", "Grundy", "Livingston", "Iroquois"))
data_3 <- data_2 %>% 
  #drop_na(Count) %>% 
  filter(class == 1)

data <-data.frame(data_4$count,data_4$Year,data_4$county)
colnames(data) <- c("count","Year","county") 

############################## Add ALPHA CODE ########################################
setwd("Z:/SBC_trends")
write.csv(data, file=paste(spp, ".csv"))

## Fit a loglinear trend with fixed site effects, random time effects, and automatic selection of degrees of freedom
trFit = ptrend(Count ~ trend(year, type = "loglinear") + county, data = data_3)

## Check the estimated percent change from year 2 to 20
t_log <- change(trFit, 1974, 2025)
trend_log <- as.data.frame(t_log)
setDT(trend_log, keep.rownames = TRUE)[]
colnames(trend_log) <- c("upper_and_lower","Percent_Change_log","CI_log","start","end") 

pdf(ppp_log)
plot(trFit, ciBase = NULL, alpha = 0.05, ylab = trend, trendCol = "black", lineCol = adjustcolor("black", alpha.f = 0.05),
     shadeCol = adjustcolor("#0072B2", alpha.f = 0.4), incCol = "#009E73", decCol = "#D55E00", plotGrid = TRUE, plotLines = FALSE, secDeriv = FALSE)
dev.off()

## Fit a smooth trend with fixed site effects, random time effects, and automatic selection of degrees of freedom #######################
trFit_smooth = ptrend(GGD_1_50 ~ trend(year, type = "smooth") + latitude, data = data_3)

## Check the estimated percent change from year 2 to 20
t_smooth <- change(trFit_smooth, 100, 600)
trend_smooth <- as.data.frame(t_smooth)
setDT(trend_smooth, keep.rownames = TRUE)[]
colnames(trend_smooth) <- c("upper_and_lower","Percent_Change_smooth","CI_smooth","start","end")
combined <- data.frame(trend_smooth$upper_and_lower,trend_smooth$Percent_Change_smooth,trend_smooth$CI_smooth,trend_log$Percent_Change_log,trend_log$CI_log,trend_log$start,trend_log$end) 
colnames(combined) <- c("upper_and_lower","Percent_Change_Smooth","CI_Smoothed","Percent_Change_Log","CI_Log","start","end") 
write.csv(combined, file=paste(trend, ".csv"))

pdf(ppp_smooth)
plot(trFit_smooth, ciBase = NULL, alpha = 0.05, ylab = trend, trendCol = "black", lineCol = adjustcolor("black", alpha.f = 0.05),
     shadeCol = adjustcolor("#0072B2", alpha.f = 0.4),incCol = "#009E73", decCol = "#D55E00", plotGrid = TRUE, plotLines = FALSE, secDeriv = FALSE)
dev.off()

#ylim=c(0,1000)

