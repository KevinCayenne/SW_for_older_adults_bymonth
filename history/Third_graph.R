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

#### Third graph (���Ӧ�F��)####

admin.normal_neat_older_adult_activity <- as.data.frame(table(normal_neat_older_adult_activity$��F��))

map.f.uni_normal_neat_older_adult_activity <- unique(normal_neat_older_adult_activity[,c(1,4,5,9,10,16)])
admin.map.f.uni_normal_neat_older_adult_activity <- unique(normal_neat_older_adult_activity[,c(9,10)])

admin.map.f.uni_normal_neat_older_adult_activity.df <- as.data.frame(table(admin.map.f.uni_normal_neat_older_adult_activity$��F��), stringsAsFactors=FALSE)
colnames(admin.map.f.uni_normal_neat_older_adult_activity.df) <- c("��F��", "���I��")

admin.uni.normal_neat_older_adult_activity <- as.data.frame(table(map.f.uni_normal_neat_older_adult_activity$��F��))

admin.normal_neat_older_adult_activity <- cbind(admin.normal_neat_older_adult_activity,
                                                �H�� = admin.uni.normal_neat_older_adult_activity$Freq)

colnames(admin.normal_neat_older_adult_activity) <- c("��F��", "�H��", "�H��")

#### Third graph (���Ӿ��I�y����) ####

org_list <- read.csv("108�~�׾��I�C��.csv")

admin.normal_neat_older_adult_activity.by_id <- as.data.frame(table(normal_neat_older_adult_activity$���I�y����))

uni.admin.normal_neat_older_adult_activity.by_id <- as.data.frame(table(map.f.uni_normal_neat_older_adult_activity$���I�y����))

admin.normal_neat_older_adult_activity.by_id <- cbind(admin.normal_neat_older_adult_activity.by_id,
                                                      �H�� = uni.admin.normal_neat_older_adult_activity.by_id$Freq,
                                                      �����ϥΦ��� = admin.normal_neat_older_adult_activity.by_id$Freq/uni.admin.normal_neat_older_adult_activity.by_id$Freq,
                                                      ���I�W�� = org_list[org_list$���I�y���� %in% admin.normal_neat_older_adult_activity.by_id$Var1,]$���I�W��)

colnames(admin.normal_neat_older_adult_activity.by_id)[1:2] <- c("���I�y����", "�H��")

admin.normal_neat_older_adult_activity <- cbind(admin.normal_neat_older_adult_activity,
                                                "���I��" = admin.map.f.uni_normal_neat_older_adult_activity.df$���I��)

# export csv and and xlsx
# write.csv(admin.normal_neat_older_adult_activity.by_id, "Third_graph_org.csv", row.names = FALSE)
export(admin.people.df, "Third_graph_org.xlsx")

# write.csv(admin.normal_neat_older_adult_activity, "Third_graph.csv", row.names = FALSE)
export(admin.people.df, "Third_graph.xlsx")