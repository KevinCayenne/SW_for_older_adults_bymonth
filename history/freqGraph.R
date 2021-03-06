library(tidyr)
library(plyr)
library(dplyr)
library(gtools)
library(data.table)
library(devtools)
library(lubridate)
library(rio)
  
#### 日期計算函數 ####
calc_age <- function(birthDate, refDate = Sys.Date()) {
  
  require(lubridate)
  
  period <- as.period(new_interval(birthDate, refDate),
                      unit = "year")
  
  period$year
}

#### 設定起始資料夾 ####

setwd("D:/Data/SWC")
 
older_adult_activity <- read.csv("2019 年 3 月長者簽到資料.csv") # 設定讀入檔案

neat_older_adult_activity <- older_adult_activity[older_adult_activity$姓名 != "" & older_adult_activity$生日 != "",]

##### Preprocessing ####

older_birth <- as.Date(neat_older_adult_activity$生日)

older_age <- calc_age(older_birth, "2019-05-01")

neat_older_adult_activity <- cbind(neat_older_adult_activity, older_age)

normal_neat_older_adult_activity <- neat_older_adult_activity[neat_older_adult_activity$older_age >= 60,]

#### freq proc ####

freq.f.uni_normal_neat_older_adult_activity <- normal_neat_older_adult_activity[,c(1,2,4,5,9,10,16)]

date.freq.f.uni_normal_neat_older_adult_activity <- separate(freq.f.uni_normal_neat_older_adult_activity, "簽到時間", into = c("日期", "時間"), sep = " ")
date.freq.f.uni_normal_neat_older_adult_activity <- separate(date.freq.f.uni_normal_neat_older_adult_activity, "日期", into = c("年", "月", "日"), sep = "/")

uni.date.freq.f.uni_normal_neat_older_adult_activity <- unique(date.freq.f.uni_normal_neat_older_adult_activity[,c(1,4,6,7,8,9)])

norep.uni.date.freq.f.uni_normal_neat_older_adult_activity <- unique(date.freq.f.uni_normal_neat_older_adult_activity[,c(1,4,6,7),])

all.hist.uni.date.freq.f.uni_normal_neat_older_adult_activity <- hist(table(norep.uni.date.freq.f.uni_normal_neat_older_adult_activity$長者流水號)/4, breaks = 6)

all.hist.uni.date.freq.f.uni_normal_neat_older_adult_activity$counts

admin.people.df <- data.frame()
for(i in levels(uni.date.freq.f.uni_normal_neat_older_adult_activity$行政區)){
  
   uni.date.freq.f.uni_normal_neat_older_adult_activity.df <- as.data.frame(table(uni.date.freq.f.uni_normal_neat_older_adult_activity[uni.date.freq.f.uni_normal_neat_older_adult_activity$行政區 == i,]$長者流水號))
   
   uni.date.freq.f.uni_normal_neat_older_adult_activity.df.hist <- hist(uni.date.freq.f.uni_normal_neat_older_adult_activity.df$Freq/4, breaks = 6)

   temp.admin <- data.frame(一周平均刷卡天數 = 1:length(uni.date.freq.f.uni_normal_neat_older_adult_activity.df.hist$counts),
                            人數 = uni.date.freq.f.uni_normal_neat_older_adult_activity.df.hist$counts, 
                            行政區 = rep(i, length(uni.date.freq.f.uni_normal_neat_older_adult_activity.df.hist$counts)))
   
   admin.people.df <- rbind(admin.people.df, temp.admin)
}
   
#### export files

export(admin.people.df, "freqGraph.xlsx")
  