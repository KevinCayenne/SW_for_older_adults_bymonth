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
library(rio)

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

#### Third graph (按照行政區)####

admin.normal_neat_older_adult_activity <- as.data.frame(table(normal_neat_older_adult_activity$行政區))

map.f.uni_normal_neat_older_adult_activity <- unique(normal_neat_older_adult_activity[,c(1,4,5,9,10,16)])
admin.map.f.uni_normal_neat_older_adult_activity <- unique(normal_neat_older_adult_activity[,c(9,10)])

admin.map.f.uni_normal_neat_older_adult_activity.df <- as.data.frame(table(admin.map.f.uni_normal_neat_older_adult_activity$行政區), stringsAsFactors=FALSE)
colnames(admin.map.f.uni_normal_neat_older_adult_activity.df) <- c("行政區", "據點數")

admin.uni.normal_neat_older_adult_activity <- as.data.frame(table(map.f.uni_normal_neat_older_adult_activity$行政區))

admin.normal_neat_older_adult_activity <- cbind(admin.normal_neat_older_adult_activity,
                                                人數 = admin.uni.normal_neat_older_adult_activity$Freq)

colnames(admin.normal_neat_older_adult_activity) <- c("行政區", "人次", "人數")

#### Third graph (按照據點流水號)####

org_list <- read.csv("108年度據點列表.csv")

admin.normal_neat_older_adult_activity.by_id <- as.data.frame(table(normal_neat_older_adult_activity$據點流水號))

uni.admin.normal_neat_older_adult_activity.by_id <- as.data.frame(table(map.f.uni_normal_neat_older_adult_activity$據點流水號))

admin.normal_neat_older_adult_activity.by_id <- cbind(admin.normal_neat_older_adult_activity.by_id,
                                                      人數 = uni.admin.normal_neat_older_adult_activity.by_id$Freq,
                                                      平均使用次數 = admin.normal_neat_older_adult_activity.by_id$Freq/uni.admin.normal_neat_older_adult_activity.by_id$Freq,
                                                      據點名稱 = org_list[org_list$據點流水號 %in% admin.normal_neat_older_adult_activity.by_id$Var1,]$據點名稱)

colnames(admin.normal_neat_older_adult_activity.by_id)[1:2] <- c("據點流水號", "人次")

write.csv(admin.normal_neat_older_adult_activity.by_id, "Third_graph_org.csv", row.names = FALSE)

admin.normal_neat_older_adult_activity <- cbind(admin.normal_neat_older_adult_activity,
                                                "據點數" = admin.map.f.uni_normal_neat_older_adult_activity.df$據點數)

write.csv(admin.normal_neat_older_adult_activity, "Third_graph.csv", row.names = FALSE)
export(admin.people.df, "Third_graph.xlsx")