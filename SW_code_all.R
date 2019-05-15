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
org_list <- read.csv("108�~�׾��I�C��.csv") # �]�wŪ�J�ɮ�

neat_older_adult_activity <- older_adult_activity[older_adult_activity$�m�W != "" & older_adult_activity$�ͤ� != "",]

##### First graph (density of participants by age ranges) ####

older_birth <- as.Date(neat_older_adult_activity$�ͤ�)

older_age <- calc_age(older_birth, "2019-05-01")

neat_older_adult_activity <- cbind(neat_older_adult_activity, older_age)

normal_neat_older_adult_activity <- neat_older_adult_activity[neat_older_adult_activity$older_age >= 20,]

# export normal data for analysis
export(normal_neat_older_adult_activity, "data_for_analysis.xlsx")

#### normal unique subjects ####

f.uni_normal_neat_older_adult_activity <- unique(normal_neat_older_adult_activity[,c(1,4,5,16)])

#table(f.uni_normal_neat_older_adult_activity$older_age, f.uni_normal_neat_older_adult_activity$�ʧO)

f.uni_normal_neat_older_adult_activity.M <- f.uni_normal_neat_older_adult_activity[f.uni_normal_neat_older_adult_activity$�ʧO == "M",]
f.uni_normal_neat_older_adult_activity.F <- f.uni_normal_neat_older_adult_activity[f.uni_normal_neat_older_adult_activity$�ʧO == "F",]
f.uni_normal_neat_older_adult_activity.NA <- f.uni_normal_neat_older_adult_activity[f.uni_normal_neat_older_adult_activity$�ʧO == "",]

hf.uni_normal_neat_older_adult_activity.M <- hist(f.uni_normal_neat_older_adult_activity.M$older_age, breaks = 15)
hf.uni_normal_neat_older_adult_activity.F <- hist(f.uni_normal_neat_older_adult_activity.F$older_age, breaks = 15)
hf.uni_normal_neat_older_adult_activity.NA <- hist(f.uni_normal_neat_older_adult_activity.NA$older_age, breaks = 15)

##### abnormal data ####

outdata.normal_neat_older_adult_activity <- neat_older_adult_activity[neat_older_adult_activity$older_age < 20,]
f.outdata.normal_neat_older_adult_activity <- unique(outdata.normal_neat_older_adult_activity[,c(1,4,5,16)])

f.outdata.normal_neat_older_adult_activity.M <- f.outdata.normal_neat_older_adult_activity[f.outdata.normal_neat_older_adult_activity$�ʧO == "M",]
f.outdata.normal_neat_older_adult_activity.F <- f.outdata.normal_neat_older_adult_activity[f.outdata.normal_neat_older_adult_activity$�ʧO == "F",]
f.outdata.normal_neat_older_adult_activity.NA <- f.outdata.normal_neat_older_adult_activity[f.outdata.normal_neat_older_adult_activity$�ʧO == "",]

f.outdata.normal_neat_older_adult_activity.col <- c("���b���R�d�򪺭�",
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

�~�ֶ��Z <- c("20-25","26-30","31-35",
              "36-40","41-45","46-50","51-55","56-60","61-65","66-70",
              "71-75","76-80","81-85","85-90","91-95","96-100","101-105",
              "106-110","111-115","116-120")

hf.uni_normal_neat_older_adult_activity.gender.df <- as.data.frame(hf.uni_normal_neat_older_adult_activity.gender, stringsAsFactors=FALSE)
hf.uni_normal_neat_older_adult_activity.gender.df <- hf.uni_normal_neat_older_adult_activity.gender.df[-c(21),]
hf.uni_normal_neat_older_adult_activity.gender.df <- cbind(�~�ֶ��Z, hf.uni_normal_neat_older_adult_activity.gender.df)
hf.uni_normal_neat_older_adult_activity.gender.df <- hf.uni_normal_neat_older_adult_activity.gender.df[,-c(2)]

colnames(hf.uni_normal_neat_older_adult_activity.gender.df) <- c("�~�ֶ��Z", "�k", "�k", "�L")

�϶��[�` <- c()
for(i in 1:nrow(hf.uni_normal_neat_older_adult_activity.gender.df)){
  �϶��[�` <- c(�϶��[�`, sum(hf.uni_normal_neat_older_adult_activity.gender.df[i,c(2:4)]))
}

fhf.uni_normal_neat_older_adult_activity.gender.df <- cbind(hf.uni_normal_neat_older_adult_activity.gender.df, �϶��[�`)

Total <- c()
for(i in 2:5){
  Total <- c(Total, sum(fhf.uni_normal_neat_older_adult_activity.gender.df[,i]))
}

Total <- c("Total", Total)

fhf.uni_normal_neat_older_adult_activity.gender.df$�~�ֶ��Z <- as.character(fhf.uni_normal_neat_older_adult_activity.gender.df$�~�ֶ��Z)

fhf.uni_nomral_neat_older_adult_activity.gender.df <- rbind(fhf.uni_normal_neat_older_adult_activity.gender.df, 
                                                            Total,
                                                            f.outdata.normal_neat_older_adult_activity.col)

##### >= 60 gender 

oversixty_normal_neat_older_adult_activity <- neat_older_adult_activity[neat_older_adult_activity$older_age >= 60,]
f.oversixty_normal_neat_older_adult_activity <- unique(oversixty_normal_neat_older_adult_activity[,c(1,4,5,16)])

f.oversixty_normal_neat_older_adult_activity.df <- as.data.frame(table(f.oversixty_normal_neat_older_adult_activity$�ʧO), stringsAsFactors=FALSE)

colnames(f.oversixty_normal_neat_older_adult_activity.df) <- c("�ʧO", "�H��")
f.oversixty_normal_neat_older_adult_activity.df$�ʧO <- c("�L", "�k", "�k")

#### export files

#write.csv(fhf.uni_nomral_neat_older_adult_activity.gender.df, "first_gragh.csv", row.names = FALSE)
export(fhf.uni_nomral_neat_older_adult_activity.gender.df, "first_gragh.xlsx")

#write.csv(f.oversixty_normal_neat_older_adult_activity.df, "gender_over60.csv", row.names = FALSE)
export(f.oversixty_normal_neat_older_adult_activity.df, "gender_over60.xlsx")

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
export(act.freq.table, "second_graph.xlsx")

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
export(admin.normal_neat_older_adult_activity.by_id, "Third_graph_org.xlsx")

# write.csv(admin.normal_neat_older_adult_activity, "Third_graph.csv", row.names = FALSE)
export(admin.normal_neat_older_adult_activity, "Third_graph.xlsx")

#### freqency graph ####

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