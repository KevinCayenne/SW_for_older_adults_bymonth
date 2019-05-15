library(tidyr)
library(plyr)
library(dplyr)
library(gtools)
library(data.table)
library(devtools)
library(lubridate)
library(rio)
  
#### ����p���� ####
calc_age <- function(birthDate, refDate = Sys.Date()) {
  
  require(lubridate)
  
  period <- as.period(new_interval(birthDate, refDate),
                      unit = "year")
  
  period$year
}

#### �]�w�_�l��Ƨ� ####

setwd("D:/Data/SWC")
 
older_adult_activity <- read.csv("2019 �~ 3 �����ñ����.csv") # �]�wŪ�J�ɮ�

neat_older_adult_activity <- older_adult_activity[older_adult_activity$�m�W != "" & older_adult_activity$�ͤ� != "",]

##### Preprocessing ####

older_birth <- as.Date(neat_older_adult_activity$�ͤ�)

older_age <- calc_age(older_birth, "2019-05-01")

neat_older_adult_activity <- cbind(neat_older_adult_activity, older_age)

normal_neat_older_adult_activity <- neat_older_adult_activity[neat_older_adult_activity$older_age >= 60,]

#### freq proc ####

freq.f.uni_normal_neat_older_adult_activity <- normal_neat_older_adult_activity[,c(1,2,4,5,9,10,16)]

date.freq.f.uni_normal_neat_older_adult_activity <- separate(freq.f.uni_normal_neat_older_adult_activity, "ñ��ɶ�", into = c("���", "�ɶ�"), sep = " ")
date.freq.f.uni_normal_neat_older_adult_activity <- separate(date.freq.f.uni_normal_neat_older_adult_activity, "���", into = c("�~", "��", "��"), sep = "/")

uni.date.freq.f.uni_normal_neat_older_adult_activity <- unique(date.freq.f.uni_normal_neat_older_adult_activity[,c(1,4,6,7,8,9)])

norep.uni.date.freq.f.uni_normal_neat_older_adult_activity <- unique(date.freq.f.uni_normal_neat_older_adult_activity[,c(1,4,6,7),])

all.hist.uni.date.freq.f.uni_normal_neat_older_adult_activity <- hist(table(norep.uni.date.freq.f.uni_normal_neat_older_adult_activity$���̬y����)/4, breaks = 6)

all.hist.uni.date.freq.f.uni_normal_neat_older_adult_activity$counts

admin.people.df <- data.frame()
for(i in levels(uni.date.freq.f.uni_normal_neat_older_adult_activity$��F��)){
  
   uni.date.freq.f.uni_normal_neat_older_adult_activity.df <- as.data.frame(table(uni.date.freq.f.uni_normal_neat_older_adult_activity[uni.date.freq.f.uni_normal_neat_older_adult_activity$��F�� == i,]$���̬y����))
   
   uni.date.freq.f.uni_normal_neat_older_adult_activity.df.hist <- hist(uni.date.freq.f.uni_normal_neat_older_adult_activity.df$Freq/4, breaks = 6)

   temp.admin <- data.frame(�@�P������d�Ѽ� = 1:length(uni.date.freq.f.uni_normal_neat_older_adult_activity.df.hist$counts),
                            �H�� = uni.date.freq.f.uni_normal_neat_older_adult_activity.df.hist$counts, 
                            ��F�� = rep(i, length(uni.date.freq.f.uni_normal_neat_older_adult_activity.df.hist$counts)))
   
   admin.people.df <- rbind(admin.people.df, temp.admin)
}
   
#### export files

export(admin.people.df, "freqGraph.xlsx")
  