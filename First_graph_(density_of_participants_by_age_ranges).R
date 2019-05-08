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

####

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

write.csv(fhf.uni_nomral_neat_older_adult_activity.gender.df, "first_gragh.csv", row.names = FALSE)

##### >= 60 gender 

oversixty_normal_neat_older_adult_activity <- neat_older_adult_activity[neat_older_adult_activity$older_age >= 60,]
f.oversixty_normal_neat_older_adult_activity <- unique(oversixty_normal_neat_older_adult_activity[,c(1,4,5,16)])

f.oversixty_normal_neat_older_adult_activity.df <- as.data.frame(table(f.oversixty_normal_neat_older_adult_activity$性別), stringsAsFactors=FALSE)

colnames(f.oversixty_normal_neat_older_adult_activity.df) <- c("性別", "人數")
f.oversixty_normal_neat_older_adult_activity.df$性別 <- c("無", "女", "男")

write.csv(f.oversixty_normal_neat_older_adult_activity.df, "gender_over60.csv", row.names = FALSE)
