# get data
data<-read.csv("Violence_Against_Women_Girls_Data.csv")

library(tidyverse)

# data exploration

apply(data,2,class)
unique(data$Country)
unique(data$Survey.Year)

#extract year of the survey
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
} #https://stackoverflow.com/questions/7963898/extracting-the-last-n-characters-from-a-string-in-r
data<-data%>%mutate(Survey.Year = as.factor(substrRight(Survey.Year, 2)))
unique(data$Survey.Year)

# change "... " with ""
data<-data%>%mutate(Question = substring(Question, 4))

unique(data$Question)                    


data_top_yr<-data%>%group_by(Question,Survey.Year,Gender,Country)%>%
  summarise(Value = mean(as.numeric(Value),na.rm=T))%>%mutate(Question =
                                                                as.factor(Question))

# trelliscope panel            

library(gridExtra)
library(trelliscopejs)

data_top_yr%>%ggplot(aes(x=factor(Survey.Year),y=Value,
                         
                         fill = Gender))+
  geom_bar(stat="identity") +
  scale_fill_manual(values = c("#68228B",  "#FFA500")) +
  theme_get() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none")+
  labs(x='Year') +
  labs(y='% of people who agree with the question') + 
  facet_grid(Gender~Question)+
  ggtitle("A husband is justified in hitting or beating his wife") +
  facet_trelliscope(~ Country, as_plotly = T, height = 720, width = 1420,
                    path = 'C:/Users/пк/Desktop/R_Git_Hub/violane_women_version_control_R')

# check the contribution of each country
# each country in the sample was questioned only once
data_country_year<-data%>%group_by(Country,Survey.Year)%>%
  summarise(Value = max(Value))
test<-as.data.frame(table(data_country_year$Country))
unique(test$Freq)


# a few ggplots

burns_food_marital<-data%>%
  filter(Question == " if she burns the food")%>%
  filter(Demographics.Question == "Marital status")%>%
  group_by(Gender, Demographics.Response)%>%
  summarise(Value = mean(Value,na.rm=T))%>%
  mutate(Gender = as.factor(Gender),
         Demographics.Response = as.factor(Demographics.Response))

burns_food_educ<-data%>%
  filter(Question == " if she burns the food")%>%
  filter(Demographics.Question == "Education")%>%
  group_by(Gender, Demographics.Response)%>%
  summarise(Value = mean(Value,na.rm=T))%>%
  mutate(Gender = as.factor(Gender),
         Demographics.Response = as.factor(Demographics.Response))

burns_food_age<-data%>%
  filter(Question == " if she burns the food")%>%
  filter(Demographics.Question == "Age")%>%
  group_by(Gender, Demographics.Response)%>%
  summarise(Value = mean(Value,na.rm=T))%>%
  mutate(Gender = as.factor(Gender),
         Demographics.Response = as.factor(Demographics.Response))

burns_food_resid<-data%>%
  filter(Question == " if she burns the food")%>%
  filter(Demographics.Question == "Residence")%>%
  group_by(Gender, Demographics.Response)%>%
  summarise(Value = mean(Value,na.rm=T))%>%
  mutate(Gender = as.factor(Gender),
         Demographics.Response = as.factor(Demographics.Response))

burns_food_empl<-data%>%
  filter(Question == " if she burns the food")%>%
  filter(Demographics.Question == "Employment")%>%
  group_by(Gender, Demographics.Response)%>%
  summarise(Value = mean(Value,na.rm=T))%>%
  mutate(Gender = as.factor(Gender),
         Demographics.Response = as.factor(Demographics.Response))




# burns food
ggplot(burns_food_marital, aes(x = Demographics.Response, y = Value, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#8B4789",  "#FFA500")) +
  labs(x = "Marital status", y = "% of people surveyed agree with the question", fill = "Gender") +
  ggtitle("A husband is justified in hitting or beating his wife if she burns the food")

ggplot(burns_food_educ, aes(x = Demographics.Response, y = Value, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#8B4789",  "#FFA500")) +
  labs(x = "Education", y = "% of people surveyed agree with the question", fill = "Gender")+
  ggtitle("A husband is justified in hitting or beating his wife if she burns the food")

ggplot(burns_food_age, aes(x = Demographics.Response, y = Value, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#8B4789",  "#FFA500")) +
  labs(x = "Age", y = "% of people surveyed agree with the question", fill = "Gender")+
  ggtitle("A husband is justified in hitting or beating his wife if she burns the food")

ggplot(burns_food_resid, aes(x = Demographics.Response, y = Value, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#8B4789",  "#FFA500")) +
  labs(x = "Residence", y = "% of people surveyed agree with the question", fill = "Gender")+
  ggtitle("A husband is justified in hitting or beating his wife if she burns the food")

ggplot(burns_food_empl, aes(x = Demographics.Response, y = Value, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#8B4789",  "#FFA500")) +
  labs(x = "Employment", y = "% of people surveyed agree with the question", fill = "Gender")+
  ggtitle("A husband is justified in hitting or beating his wife if she burns the food")







