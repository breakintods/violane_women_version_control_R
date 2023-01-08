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
                         color = Gender,
                         fill = Gender))+
  geom_bar(stat="identity") +
  theme_get() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none")+
  labs(x='Year') +
  labs(y='% of people who agree with the question') + 
  facet_grid(Gender~Question)+
  ggtitle("A husband is justified in hitting or beating his wife") +
  facet_trelliscope(~ Country, as_plotly = T, height = 720, width = 1420,
                    path = 'C:/Users/пк/Desktop/R_Git_Hub/violane_women_version_control_R')

# transform the data from long to wide form

library("reshape2")
data_wide<-dcast(data, Demographics.Question, 
                 value.var="Demographics.Response")
test<-data%>%filter(RecordID%in%"1",
                    Demographics.Question%in%"Marital status")


