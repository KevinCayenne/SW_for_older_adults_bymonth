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

normal_neat_older_adult_activity <- neat_older_adult_activity[neat_older_adult_activity$older_age >= 20,]

normal_neat_older_adult_activity[normal_neat_older_adult_activity$�D���� == "" & normal_neat_older_adult_activity$�l���� == "",]

#### Second graph (Total number of participants which is repeatable) #### 

act.freq.table <- as.data.frame(table(normal_neat_older_adult_activity$�l����, normal_neat_older_adult_activity$�D����), stringsAsFactors=FALSE)
act.freq.table <- act.freq.table[act.freq.table$Freq != 0,]

colnames(act.freq.table) <- c("�l����", "�D����", "�H��")
act.freq.table[1,1:2] <- c("�@��ñ��", "�@��ñ��")
row.names(act.freq.table) <- 1:nrow(act.freq.table)

#### Second graph (Total number of participants which is not repeatable) #### 

f.uni_normal_neat_older_adult_activity <- unique(normal_neat_older_adult_activity[,c(1,4,5,12,13,14,16)])

norepo.act.freq.table <- as.data.frame(table(f.uni_normal_neat_older_adult_activity$�l����, f.uni_normal_neat_older_adult_activity$�D����), stringsAsFactors=FALSE)
norepo.act.freq.table <- norepo.act.freq.table[norepo.act.freq.table$Freq != 0,]

colnames(norepo.act.freq.table) <- c("�l����", "�D����", "�H��")
norepo.act.freq.table[1,1:2] <- c("�@��ñ��", "�@��ñ��")
row.names(norepo.act.freq.table) <- 1:nrow(norepo.act.freq.table)

#### merge and export csv file ####

act.freq.table <- cbind(act.freq.table, �H�� = norepo.act.freq.table$�H��)

act.freq.table <- cbind(act.freq.table, �����ϥΦ��� = act.freq.table$�H��/act.freq.table$�H��)

# write.csv(act.freq.table, "second_graph.csv", row.names = FALSE)
export(admin.people.df, "second_graph.xlsx")