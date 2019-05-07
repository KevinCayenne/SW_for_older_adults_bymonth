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

calc_age <- function(birthDate, refDate = Sys.Date()) {
  
  require(lubridate)
  
  period <- as.period(new_interval(birthDate, refDate),
                      unit = "year")
  
  period$year
}

setwd("D:/Data/仕緯專案")

older_adult_activity <- read.csv("2019 年 3 月長者簽到資料.csv")
neat_older_adult_activity <- older_adult_activity[older_adult_activity$姓名 != "" & older_adult_activity$生日 != "",]


##### Preprocessing ####

older_birth <- as.Date(neat_older_adult_activity$生日)

older_age <- calc_age(older_birth, "2019-05-01")

neat_older_adult_activity <- cbind(neat_older_adult_activity, older_age)

normal_neat_older_adult_activity <- neat_older_adult_activity[neat_older_adult_activity$older_age >= 20,]

normal_neat_older_adult_activity[normal_neat_older_adult_activity$主類型 == "" & normal_neat_older_adult_activity$子類型 == "",]

#### Second graph (Total number of participants which is repeatable) #### 

act.freq.table <- as.data.frame(table(normal_neat_older_adult_activity$子類型, normal_neat_older_adult_activity$主類型), stringsAsFactors=FALSE)
act.freq.table <- act.freq.table[act.freq.table$Freq != 0,]

colnames(act.freq.table) <- c("子類型", "主類型", "人次")
act.freq.table[1,1:2] <- c("一般簽到", "一般簽到")
row.names(act.freq.table) <- 1:nrow(act.freq.table)

#### Second graph (Total number of participants which is not repeatable) #### 

f.uni_normal_neat_older_adult_activity <- unique(normal_neat_older_adult_activity[,c(1,4,5,12,13,14,16)])

norepo.act.freq.table <- as.data.frame(table(f.uni_normal_neat_older_adult_activity$子類型, f.uni_normal_neat_older_adult_activity$主類型), stringsAsFactors=FALSE)
norepo.act.freq.table <- norepo.act.freq.table[norepo.act.freq.table$Freq != 0,]

colnames(norepo.act.freq.table) <- c("子類型", "主類型", "人數")
norepo.act.freq.table[1,1:2] <- c("一般簽到", "一般簽到")
row.names(norepo.act.freq.table) <- 1:nrow(norepo.act.freq.table)

#### merge and create csv file ####

act.freq.table <- cbind(act.freq.table, 人數 = norepo.act.freq.table$人數)

act.freq.table <- cbind(act.freq.table, 平均使用次數 = act.freq.table$人次/act.freq.table$人數)

write.csv(act.freq.table, "second_graph.csv", row.names = FALSE)

