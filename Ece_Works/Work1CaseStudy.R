# First set your working directory
setwd("~/myBDAgroup/case_1/")
# Download from GitHub (do it only once)
download.file("https://mef-bda503.github.io/files/osym_data_2017.RData", 
              "osym_data_2017.RData")
# Install tidyverse if not already installed
if (!("tidyverse" %in% installed.packages())) {
  install.packages("tidyverse", repos = "https://cran.r-project.org")
}
# Load tidyverse package
library(tidyverse)
# Load the data
load("C:\Users\MEFLAB409\Downloads\osym_data_2017.RData")

#########
install.packages("tidyverse")
install("ggplot2")
options(dplyr.width=Inf)
library(tidyverse)
library(dplyr)
osym_data_2017<-load("C:/Users/MEFLAB409/Downloads/osym_data_2017.RData")
glimpse(osym_data_2017)
osym_data_2017 %>% distinct(university_name)
ist.uni<-osym_data_2017 %>% filter(city == "ISTANBUL")  ## UTF-8 problemi
Sys.setlocale("LC_COLLATE", "English")
Sys.setlocale("LC_CTYPE", "English")
Sys.getlocale()
ist.uni<-osym_data_2017 %>% filter(grepl("STANBUL",city))
##bolu.uni <- osym_data_2017%>% filter(city=="BOLU")
ist.uni %>% distinct(university_name)
ist.uni.faculties <- ist.uni %>% group_by(faculty_name)
ist.uni.faculties.programs <- ist.uni.faculties%>%group_by(program_name)
ist.uni.faculties.programs %>% group_by(program_name)%>%summarise(max(max_score)) # en yüksek puana sahipser
ist.uni.faculties.programs %>% group_by(program_name)%>%summarise(max(max_score))%>%top_n(20)

ist.uni.faculties.programs.top20<-ist.uni.faculties.programs %>% group_by(program_name)
%>%summarise(max_max_score=max(max_score))%>%top_n(20)
ist.uni.faculties.programs.top20 %>% select(max_max_score)

ggplot(ist.uni.faculties.programs.top20, aes(x=program_name,y=max_max_score)) + geom_point(shape=1) ## ilk grafik.

ggplot(ist.uni.faculties.programs.top20, aes(x=program_name,y=max_max_score)) + geom_point(shape=1) + theme(axis.text = element_text(angle = 15)) ## yazilar idüzelt.

ggplot(ist.uni.faculties.programs.top20%>%top_n(10), aes(x=program_name,y=max_max_score)) + geom_point(shape=1) + theme(axis.text = element_text(angle = 15)) ##top 10 u aldim.

ggplot(ist.uni.faculties.programs.top20%>%top_n(10), aes(x=program_name,y=max_max_score)) + geom_bar(stat="identity") + theme(axis.text = element_text(angle = 15))

##MEF
mef.uni<-ist.uni %>% filter(grepl("MEF",university_name))
##top 10 bölum
mef.uni.top_10 <- mef.uni %>% group_by(program_name) %>% summarise(max_max_score=max(max_score)) %>% top_n(10)

ggplot(mef.uni.top_10,aes(x=program_name,y=max_fac_score)) + geom_bar(stat="identity")
#OK
ggplot(mef.uni.top_10,aes(x=program_name,y=max_fac_score)) + geom_bar(stat="identity") + theme(axis.text = element_text(angle = 15))

ggplot(mef.uni.top_10,aes(x=program_name,y=max_fac_score)) + geom_bar(stat="identity") + theme(axis.text = element_text(angle = 15)) + xlab("Programs") + ylab("Max_Score") + ggtitle("MEF UNIVERSITY TOP 10 PROGRAM")

ggplot(mef.uni.top_10,aes(x=program_name,y=max_fac_score)) + geom_bar(stat="identity") + theme_minimal() + theme(axis.text = element_text(angle = 15)) + xlab("Programs") + ylab("Max_Score") + ggtitle("MEF UNIVERSITY TOP 10 PROGRAM")

ggplot(mef.uni.top_10,aes(x=program_name,y=max_fac_score)) + geom_bar(stat="identity",fill="yellow") + theme_minimal() + theme(axis.text = element_text(angle = 15)) + xlab("Programs") + ylab("Max_Score") + ggtitle("MEF UNIVERSITY TOP 10 PROGRAM")