# First set your working directory
setwd("C:/Users/ecetp/Downloads/MEF/Bda 503 - R")
# set charset setting in turkish.
Sys.setlocale(locale="Turkish_Turkey.1254")

# Download from GitHub (do it only once)
download.file("https://mef-bda503.github.io/files/osym_data_2017.RData", 
              "osym_data_2017.RData")

# pti is a vector containing packages that we would like to use
pti <- c("tidyverse","readxl","writexl","lubridate")
# let's check against the already installed packages vector
# and install only missing packages
pti <- pti[!(pti %in% installed.packages())]
# install the required packages
install.packages(pti,repos="https://cran.r-project.org")

# Install tidyverse if not already installed
if (!("tidyverse" %in% installed.packages())) {
  install.packages("tidyverse", repos = "https://cran.r-project.org")
}
install.packages("stringr")
library(stringr)
# Load tidyverse package
library(tidyverse)
# Load the data
load("osym_data_2017.RData")
#see what is data
glimpse(osym_data_2017)

# call libraries.
library(tidyverse)
library(dplyr)
library(ggplot2)

#check university names
osym_data_2017 %>% distinct(university_name)

#just Istanbul uni's.
ist.uni<-osym_data_2017 %>% filter(city == "ISTANBUL") 
ist.uni<-osym_data_2017 %>% filter(grepl("STANBUL",city))  ##karakter sorunu çözülmezse bu sekilde de olur.

#MEF uni's.
mef.uni<-ist.uni %>% filter(grepl("MEF",university_name))

# MEF uni top 10 program according to max_Score
mef.uni.top_10 <- mef.uni %>% group_by(program_name) %>% summarise(max_max_score=max(max_score)) %>% top_n(10)

# MEF Grafik
ggplot(mef.uni.top_10,aes(x=program_name,y=max_max_score)) + geom_bar(stat="identity",fill="yellow") + theme_minimal() + theme(axis.text = element_text(angle = 15)) + xlab("Programmes") + ylab("Max_Score") + ggtitle("MEF UNIVERSITY TOP 10 PROGRAM")

# bu bölümlerin en yüksek oldugu ünive
#.sirala
mef.uni.top_10%>%arrange(max_max_score)
#en yüksek puanli bölümü al
mef.uni.top.1 <-mef.uni.top_10%>%arrange(max_max_score)%>%top_n(1)%>%select(program_name)
#bu bölüme sahip istanbuldaki üniler  ## bunu parametreyle yapamadim ... :( filter'a parametre verme.
ist.uni.hukuk <- ist.uni %>% filter(program_name=="Hukuk (Tam Burslu)")

#graph it ! 
orderedhukuk<-ist.uni.hukuk%>%arrange(max_score)
ggplot(orderedhukuk,aes(x=university_name,y=max_score)) + geom_bar(stat="identity",fill="yellow") + coord_flip() + theme_minimal() 
+ xlab("Max Score") + ylab("University") + ggtitle("Hukuk (Tam Burslu) Program In Universities")






