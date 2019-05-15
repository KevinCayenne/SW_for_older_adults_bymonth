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
org_list <- read.csv("108年度據點列表.csv") # 設定讀入檔案

neat_older_adult_activity <- older_adult_activity[older_adult_activity$姓名 != "" & older_adult_activity$生日 != "",]

##### First graph (density of participants by age ranges) ####

older_birth <- as.Date(neat_older_adult_activity$生日)

older_age <- calc_age(older_birth, "2019-05-01")

neat_older_adult_activity <- cbind(neat_older_adult_activity, older_age)

normal_neat_older_adult_activity <- neat_older_adult_activity[neat_older_adult_activity$older_age >= 20,]

#### normal unique subjects ####

f.uni_normal_neat_older_adult_activity <- unique(normal_neat_older_adult_activity[,c(1,4,5,16)])

#table(f.uni_normal_neat_older_adult_activity$older_age, f.uni_normal_neat_older_adult_activity$性別)

f.uni_normal_neat_older_adult_activity.M <- f.uni_normal_neat_older_adult_activity[f.uni_normal_neat_older_adult_activity$性別 == "M",]
f.uni_normal_neat_older_adult_activity.F <- f.uni_normal_neat_older_adult_activity[f.uni_normal_neat_older_adult_activity$性別 == "F",]
f.uni_normal_neat_older_adult_activity.NA <- f.uni_normal_neat_older_adult_activity[f.uni_normal_neat_older_adult_activity$性別 == "",]

hf.uni_normal_neat_older_adult_activity.M <- hist(f.uni_normal_neat_older_adult_activity.M$older_age, breaks = 15)
hf.uni_normal_neat_older_adult_activity.F <- hist(f.uni_normal_neat_older_adult_activity.F$older_age, breaks = 15)
hf.uni_normal_neat_older_adult_activity.NA <- hist(f.uni_normal_neat_older_adult_activity.NA$older_age, breaks = 15)

##### abnormal data ####

outdata.normal_neat_older_adult_activity <- neat_older_adult_activity[neat_older_adult_activity$older_age < 20,]
f.outdata.normal_neat_older_adult_activity <- unique(outdata.normal_neat_older_adult_activity[,c(1,4,5,16)])

f.outdata.normal_neat_older_adult_activity.M <- f.outdata.normal_neat_older_adult_activity[f.outdata.normal_neat_older_adult_activity$性別 == "M",]
f.outdata.normal_neat_older_adult_activity.F <- f.outdata.normal_neat_older_adult_activity[f.outdata.normal_neat_older_adult_activity$性別 == "F",]
f.outdata.normal_neat_older_adult_activity.NA <- f.outdata.normal_neat_older_adult_activity[f.outdata.normal_neat_older_adult_activity$性別 == "",]

f.outdata.normal_neat_older_adult_activity.col <- c("不在分析範圍的值",
                                                    nrow(f.outdata.normal_neat_older_adult_activity.M),
                                                    nrow(f.outdata.normal_neat_older_adult_activity.F),
                                                    nrow(f.outdata.normal_neat_older_adult_activity.NA),
                                                    nrow(f.outdata.normal_neat_older_adult_activity.M) + nrow(f.outdata.normal_neat_older_adult_activity.F) + nrow(f.outdata.normal_neat_older_adult_activity.NA))

lengths = max(length(hf.uni_normal_neat_older_adult_activity.M$counts),
              length(hf.uni_normal_neat_older_adult_activity.F$counts),
              length(hf.uni_normal_neat_older_adult_activity.NA$counts))

length(hf.uni_normal_neat_older_adult_activity.M$counts) = lengths
length(hf.uni_normal_neat_older_adult_activity.F$counts) = lengths
length(hf.uni_normal_neat_older_adult_activity.NA$counts) = lengths

hf.uni_normal_neat_older_adult_activity.gender <- cbind(hf.uni_normal_neat_older_adult_activity.M$breaks, 
                                                        hf.uni_normal_neat_older_adult_activity.M$counts, 
                                                        hf.uni_normal_neat_older_adult_activity.F$counts, 
                                                        hf.uni_normal_neat_older_adult_activity.NA$counts)

hf.uni_normal_neat_older_adult_activity.gender[is.na(hf.uni_normal_neat_older_adult_activity.gender)] = 0

年齡間距 <- c("20-25","26-30","31-35",
              "36-40","41-45","46-50","51-55","56-60","61-65","66-70",
              "71-75","76-80","81-85","85-90","91-95","96-100","101-105",
              "106-110","111-115","116-120")

hf.uni_normal_neat_older_adult_activity.gender.df <- as.data.frame(hf.uni_normal_neat_older_adult_activity.gender, stringsAsFactors=FALSE)
hf.uni_normal_neat_older_adult_activity.gender.df <- hf.uni_normal_neat_older_adult_activity.gender.df[-c(21),]
hf.uni_normal_neat_older_adult_activity.gender.df <- cbind(年齡間距, hf.uni_normal_neat_older_adult_activity.gender.df)
hf.uni_normal_neat_older_adult_activity.gender.df <- hf.uni_normal_neat_older_adult_activity.gender.df[,-c(2)]

colnames(hf.uni_normal_neat_older_adult_activity.gender.df) <- c("年齡間距", "男", "女", "無")

區間加總 <- c()
for(i in 1:nrow(hf.uni_normal_neat_older_adult_activity.gender.df)){
  區間加總 <- c(區間加總, sum(hf.uni_normal_neat_older_adult_activity.gender.df[i,c(2:4)]))
}

fhf.uni_normal_neat_older_adult_activity.gender.df <- cbind(hf.uni_normal_neat_older_adult_activity.gender.df, 區間加總)

Total <- c()
for(i in 2:5){
  Total <- c(Total, sum(fhf.uni_normal_neat_older_adult_activity.gender.df[,i]))
}

Total <- c("Total", Total)

fhf.uni_normal_neat_older_adult_activity.gender.df$年齡間距 <- as.character(fhf.uni_normal_neat_older_adult_activity.gender.df$年齡間距)

fhf.uni_nomral_neat_older_adult_activity.gender.df <- rbind(fhf.uni_normal_neat_older_adult_activity.gender.df, 
                                                            Total,
                                                            f.outdata.normal_neat_older_adult_activity.col)

##### >= 60 gender 

oversixty_normal_neat_older_adult_activity <- neat_older_adult_activity[neat_older_adult_activity$older_age >= 60,]
f.oversixty_normal_neat_older_adult_activity <- unique(oversixty_normal_neat_older_adult_activity[,c(1,4,5,16)])

f.oversixty_normal_neat_older_adult_activity.df <- as.data.frame(table(f.oversixty_normal_neat_older_adult_activity$性別), stringsAsFactors=FALSE)

colnames(f.oversixty_normal_neat_older_adult_activity.df) <- c("性別", "人數")
f.oversixty_normal_neat_older_adult_activity.df$性別 <- c("無", "女", "男")

#### export files

#write.csv(fhf.uni_nomral_neat_older_adult_activity.gender.df, "first_gragh.csv", row.names = FALSE)
export(admin.people.df, "first_gragh.xlsx")

#write.csv(f.oversixty_normal_neat_older_adult_activity.df, "gender_over60.csv", row.names = FALSE)
export(admin.people.df, "gender_over60.xlsx")


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

#### merge and export csv file ####

act.freq.table <- cbind(act.freq.table, 人數 = norepo.act.freq.table$人數)

act.freq.table <- cbind(act.freq.table, 平均使用次數 = act.freq.table$人次/act.freq.table$人數)

# write.csv(act.freq.table, "second_graph.csv", row.names = FALSE)
export(admin.people.df, "second_graph.xlsx")

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

#### Third graph (按照據點流水號) ####

admin.normal_neat_older_adult_activity.by_id <- as.data.frame(table(normal_neat_older_adult_activity$據點流水號))

uni.admin.normal_neat_older_adult_activity.by_id <- as.data.frame(table(map.f.uni_normal_neat_older_adult_activity$據點流水號))

admin.normal_neat_older_adult_activity.by_id <- cbind(admin.normal_neat_older_adult_activity.by_id,
                                                      人數 = uni.admin.normal_neat_older_adult_activity.by_id$Freq,
                                                      平均使用次數 = admin.normal_neat_older_adult_activity.by_id$Freq/uni.admin.normal_neat_older_adult_activity.by_id$Freq,
                                                      據點名稱 = org_list[org_list$據點流水號 %in% admin.normal_neat_older_adult_activity.by_id$Var1,]$據點名稱)

colnames(admin.normal_neat_older_adult_activity.by_id)[1:2] <- c("據點流水號", "人次")

admin.normal_neat_older_adult_activity <- cbind(admin.normal_neat_older_adult_activity,
                                                "據點數" = admin.map.f.uni_normal_neat_older_adult_activity.df$據點數)

# export csv and and xlsx
# write.csv(admin.normal_neat_older_adult_activity.by_id, "Third_graph_org.csv", row.names = FALSE)
export(admin.people.df, "Third_graph_org.xlsx")

# write.csv(admin.normal_neat_older_adult_activity, "Third_graph.csv", row.names = FALSE)
export(admin.people.df, "Third_graph.xlsx")

#### freqency graph ####

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
