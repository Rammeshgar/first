
#first week!

library(tidyverse)
library(ggrepel)
getwd()
my_data2<-
read.csv("C:/Users/admin/OneDrive/Documents/Rcodes/coursera.csv")

names(my_data2)
dim(my_data2)
table(my_data2$year)
summary(my_data2$all_bills)

fig115<-filter(my_data2, congress==115)
fig115<-select(fig115, "seniority", "all_pass", "dem")

ggplot(fig115, aes(seniority, all_pass))+
  geom_point()

fig115 %>% 
  filter("congress"==115) %>% 
  select("seniority","all_pass","dem")

fig115$dem

ggplot(fig115, aes(seniority, all_pass, color= dem))+
  geom_jitter()+
  labs(x= "Seniority", y= "Bills passed", 
       title= "Seniority and Bills passed in the 115th congress")

party<- recode(fig115$dem, '1'="Democrat", '0'="Republican")

fig115<- add_column(fig115,party)

fig115$party

ggplot(fig115, aes(seniority, all_pass, color= party))+
  geom_jitter()+
  labs(x= "Seniority", y= "Bills passed", 
       title= "Seniority and Bills passed in the 115th congress")+
  scale_color_manual(values = c("blue","red"))+
  facet_wrap(~party)

#second week!(bar plots)

my_data2 %>% 
  filter(congress==115) %>% 
  ggplot(aes(dem))+
  geom_bar()

table(filter(my_data2, congress==115)$dem)

my_data2 %>% 
  filter(congress==115) %>% 
  ggplot(aes(st_name))+
  geom_bar()

my_data2 %>% 
  filter(congress==115) %>% 
  ggplot(aes(y=st_name))+
  geom_bar()

party<- recode(my_data2$dem, '1'="Democrat", '0'="Republican")

my_data2<- add_column(my_data2,party)

my_data2 %>% 
  filter(congress==115) %>% 
  ggplot(aes(party, fill= party))+
  geom_bar()+
  labs(x= "Party", y= "Number of Members")+
  scale_fill_manual(values = c("cyan4","red4"))+
  guides(fill=F)

########

apple<- rep("apple",6)
orange<- rep("orange",3)
banana<- rep("banana",1)

fruit__bowl<- tibble("fruits"=c(apple,orange,banana))

fruit_bowl_summary<- fruit__bowl %>% 
  group_by(fruits) %>% 
  summarise(count=n())

#calculate proportion
fruit_bowl_summary$proportion<-fruit_bowl_summary$count/sum(fruit_bowl_summary$count)

fruit_bowl_summary

ggplot(fruit_bowl_summary, aes(fruits,proportion, fill=fruits))+
  geom_bar(stat="identity")+
  scale_fill_manual(values = c("red2","yellow","orange"))+
  guides(fill=F)+
  labs(x= "Fruits",y= "Proportion of Fruits")

########
getwd()
my_data3<-
read.csv("c:/Users/admin/OneDrive/Documents/Rcodes/advent of code/cces_sample_coursera.csv")

dem_rep<- recode(my_data3$pid7,'1'="Democrat",'2'="Democrat",'3'="Democrat",
                 '4'="Independent",'5'="Republican",'6'="Republican",'7'="Republican")
table(dem_rep)

my_data3 <- add_column(my_data3, dem_rep)

ggplot(my_data3, aes(region, fill=dem_rep))+
  geom_bar(position = "dodge")+
  labs(x="Region", y="Count")

#third week!(annotation)

kid<- c("Nick","Jessica","Justica","Bradi","Kelly","Enrique")
time_spent<- c(40,35,25,20,10,5)
high_score<- c(100,75,85,50,25,30)

tetris<- tibble(kid,time_spent,high_score)

ggplot(tetris,aes(time_spent, high_score))+
  #geom_point()+
  geom_text(aes(label=kid))


ggplot(tetris,aes(time_spent, high_score))+
  geom_point()+
  geom_text(aes(label=kid), nudge_y = 5)

########

my_data2 %>% 
  filter(congress==115) %>% 
  ggplot(aes(dwnom1, all_pass, label= thomas_name))+
  geom_point()+
  geom_text(data = filter(my_data2,congress==115 & all_pass>8))


my_data2 %>% 
  filter(congress==115) %>% 
  ggplot(aes(dwnom1, all_pass))+
  geom_point()+
  geom_text_repel(data = filter(my_data2,congress==115 & all_pass>8), 
                  mapping = aes(dwnom1, all_pass, label= thomas_name))+
  annotate("rect", xmin =.05, xmax = .4, ymin= 13, ymax= 15, alpha=.2, fill="red4")+
  annotate("text",x=.6, y=14, label="most passed", color="blue2")
