urlfile<-'https://raw.githubusercontent.com/vincentarelbundock/Rdatasets/master/csv/car/Vocab.csv'
dsin<-read.csv(url(urlfile))

library(dplyr)
glimpse(dsin)

dsin %>% distinct(year)
dsin %>% distinct(year) %>% arrange(year) ## order distinct years in dataset.
dsin %>% distinct(sex)  ## Female, Male
dsin %>% arrange(desc((year)))   ##order by year
female.2004<-dsin %>% filter(year==2004 & sex=="Female") ## 2004 female 
count(female.2004) ##801
male.2004<-dsin %>% filter(year==2004 & sex=="Male") ## 2004 male
male.2004  ##637

dsin %>% group_by(year,sex)

g<-dsin %>% group_by(year) %>% summarise(count=n()) ## yillara göre satir count
g2<-dsin %>% group_by(year) %>% summarise(count=n(),voc_sum=sum(vocabulary)) ## yillara göre vocab toplam
g3<-dsin %>% group_by(year) %>% summarise(cnt=n(),voc_sum=sum(vocabulary),avg=(voc_sum/cnt))  ## yillara göre ortalama
g3<-dsin %>% group_by(year) %>% summarise(cnt=n(),voc_sum=sum(vocabulary),avg=round(voc_sum/cnt,2)) ## round

ggplot(data = dsin, aes(x = sex, y = education)) + 
  +     geom_point()

ggplot(data = g3, aes(x = year, y = avg)) + geom_line() ## yillara göre vocab grafigi.

## correlation

ggplot(g3, aes(year)) + 
  +   geom_point(aes(y = Avg_Vocab_Test, colour = "red")) + 
  +   geom_point(aes(y = Avg_Educ_In_Year, colour = "black"))

ggplot(g3, aes(year)) + 
  geom_point(aes(y = Avg_Vocab_Test, colour = "Average Vocabulary Test Score")) + 
  geom_point(aes(y = Avg_Educ_In_Year, colour = "Average Education in Year"))+scale_colour_manual(values=c("red", "blue")) +theme_minimal() 

#with title

> ggplot(g3, aes(year)) + 
  +   geom_point(aes(y = Avg_Vocab_Test, colour = "Average Vocabulary Test Score")) +geom_point(aes(y = Avg_Educ_In_Year, colour = "Average Education in Year"))+scale_colour_manual(values=c("red", "blue")) 
+theme_minimal() + ggtitle("Is there any correlation?")

## with x-y labels

ggplot(g3, aes(year)) + 
  geom_point(aes(y = Avg_Vocab_Test, colour = "Average Vocabulary Test Score")) +geom_point(aes(y = Avg_Educ_In_Year, colour = "Average Education in Year"))+scale_colour_manual(values=c("red", "blue")) +theme_minimal() + xlab("Year") + ylab("Average") +ggtitle("Is there any correlation?")

xlab("Names Of Speakers") + ylab("Number of Talks") +
  ggtitle("Top 10 TED Speakers")
###
a<-dsin %>% group_by(year,sex)%>%summarise(edu_sum=sum(education),voc_sum=sum(vocabulary)) ## yil ve cinsiyete göre sum
a<-dsin %>% group_by(year,sex)%>%summarise(count=n(),edu_sum=sum(education),voc_sum=sum(vocabulary))  ## countlu.
a<-dsin %>% group_by(year,sex)%>%summarise(cnt=n(),edu_sum=sum(education),voc_sum=sum(vocabulary),avg_edu=round(edu_sum/cnt,2), avg_vocab=round(voc_sum/cnt,2))
ggplot(data=a, aes(x=year,y=sex,fill=avg_edu)) + geom_bar(stat="identity") + theme(axis.text = element_text(angle = 45))
a%>%filter(sex=="Female")  ## sadece kadinlari alalim.
ggplot(data=a%>%filter(sex=="Female"), aes(x=year,y=avg_vocab)) + geom_bar(stat="identity") + theme(axis.text = element_text(angle = 15)) ##kadin grafigi
ggplot(data=a%>%filter(sex=="Male"), aes(x=year,y=avg_vocab)) + geom_bar(stat="identity") + theme(axis.text = element_text(angle = 45)) ##erkek grafigi
ggplot(data=a, aes(x=year,y=sex,fill=avg_vocab)) + geom_bar(stat="identity") + theme(axis.text = element_text(angle = 45)) + coord_trans()
ggplot(data=a, aes(x=sex,y=year,color=avg_vocab))+ coord_flip() + geom_point() ##ikisi br arada
ggplot(data=a, aes(x=sex,y=year,color=avg_vocab))+ coord_flip() + geom_boxplot() ## manasiz bir grafik.
ggplot(data=a, aes(x=sex,y=year,color=avg_vocab,size=5))+ coord_flip() + geom_point() ## güzel.
ggplot(data=a, aes(x=sex,y=year,color=avg_vocab,size=5,shape=sex))+ coord_flip() + geom_point() ## sekilli.


ggplot(data=a, aes(x=sex,y=year,color=avg_vocab,size=5))+ coord_flip() + geom_point() + scale_color_continuous(low="blue",high="red",guide="colourbar")

ggplot(data=a, aes(x=sex,y=year,color=avg_vocab,size=5))+ coord_flip() + geom_point() +theme_classic()+ scale_color_continuous(low="blue",high="red",guide="colourbar") 