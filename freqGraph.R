library(ggplot2)
library(ggpubr)
library(tidyr)
library(plyr)
library(dplyr)
library(gtools)
library(gridExtra)
library(ggforce)
library(ggpmisc)
library(data.table)
library(devtools)
library(mni2aal)
library(lubridate)
library(stringr)

calc_age <- function(birthDate, refDate = Sys.Date()) {
  
  require(lubridate)
  
  period <- as.period(new_interval(birthDate, refDate),
                      unit = "year")
  
  period$year
}

setwd("D:/Data/仕緯專案")

older_adult_activity <- read.csv("2019 年 3 月長者簽到資料.csv")
neat_older_adult_activity <- older_adult_activity[older_adult_activity$姓名 != "" & older_adult_activity$生日 != "",]

older_birth <- as.Date(neat_older_adult_activity$生日)

older_age <- calc_age(older_birth, "2019-05-01")

neat_older_adult_activity <- cbind(neat_older_adult_activity, older_age)

normal_neat_older_adult_activity <- neat_older_adult_activity[neat_older_adult_activity$older_age >= 60,]

####

freq.f.uni_normal_neat_older_adult_activity <- normal_neat_older_adult_activity[,c(1,2,4,5,9,10,16)]

date.freq.f.uni_normal_neat_older_adult_activity <- separate(freq.f.uni_normal_neat_older_adult_activity, "簽到時間", into = c("日期", "時間"), sep = " ")
date.freq.f.uni_normal_neat_older_adult_activity <- separate(date.freq.f.uni_normal_neat_older_adult_activity, "日期", into = c("年", "月", "日"), sep = "/")

nui.date.freq.f.uni_normal_neat_older_adult_activity <- unique(date.freq.f.uni_normal_neat_older_adult_activity[,c(1,4,6,7,8,9)])

for(i in levels(freq.f.uni_normal_neat_older_adult_activity$行政區)){

 sum.people <- nrow(freq.f.uni_normal_neat_older_adult_activity[freq.f.uni_normal_neat_older_adult_activity$行政區 == i,])
 num.people <- as.data.frame(table(freq.f.uni_normal_neat_older_adult_activity[freq.f.uni_normal_neat_older_adult_activity$行政區 == i,]$長者流水號))
 
 
 hist.num.people <- hist(num.people$Freq/4, breaks = 35)
 hist.num.people$counts
 hist.num.people$breaks
}
 

as.data.frame(table(freq.f.uni_normal_neat_older_adult_activity[freq.f.uni_normal_neat_older_adult_activity$行政區 == "士林區",]$長者流水號))
