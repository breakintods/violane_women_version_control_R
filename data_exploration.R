# get data
data<-read.csv("Violence_Against_Women_Girls_Data.csv")


# load libraries

if (!require(pacman)) {
  install.packages("pacman")
  require(pacman)
}
pacman::p_load(
  tidyverse, gridExtra, trelliscopejs, maps, stringr, grid
)

# data exploration

apply(data,2,class)
unique(data$Survey.Year)

#extract year of the survey
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
} #https://stackoverflow.com/questions/7963898/extracting-the-last-n-characters-from-a-string-in-r
data<-data%>%mutate(Survey.Year = as.factor(substrRight(Survey.Year, 2)))
unique(data$Survey.Year)

# change "...  " with ""
data<-data%>%mutate(Question = substring(Question, 5))

unique(data$Question)                    


data_top_yr<-data%>%group_by(Question,Survey.Year,Gender,Country)%>%
  summarise(Value = mean(as.numeric(Value),na.rm=T))%>%mutate(Question =
                                                                as.factor(Question))

# trelliscope panel - non-filtered!          

data_top_yr%>%ggplot(aes(x=factor(Survey.Year),y=Value,
                         
                         fill = Gender))+
  geom_bar(stat="identity") +
  scale_fill_manual(values = c("#A2B5CD",  "#B452CD")) +
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
length(unique(data_country_year$Country))

# create a map of covered countries

# Create a data frame with the countries in your survey
survey_countries <- as.data.frame(
  data_country_year[,1]
)


survey_countries$Country_2 <- survey_countries$Country

# Merge your survey data with a map data frame to get the polygon data for each country
world_map <- map_data("world")


world_map_merged <- left_join(world_map, survey_countries, by = c("region" = "Country"))


# Plot the world map with countries colored based on whether they are in your survey
ggplot(world_map_merged, aes(long, lat, group = group)) + 
  geom_polygon(aes(fill = !is.na(Country_2))) + 
  coord_fixed(1.3) + 
  scale_fill_manual(values = c("gray", "brown")) + 
  ggtitle("Coverage of the Demographic and Health Survey 2000-2018")+
  theme_void()+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"))

# a few plots

# 1) burns food

burns_food_marital<-data%>%
  filter(Question == "if she burns the food")%>%
  filter(Demographics.Question == "Marital status")%>%
  group_by(Gender, Demographics.Response)%>%
  summarise(Value = mean(Value,na.rm=T))%>%
  mutate(Gender = as.factor(Gender),
         Demographics.Response = as.factor(Demographics.Response))

burns_food_educ<-data%>%
  filter(Question == "if she burns the food")%>%
  filter(Demographics.Question == "Education")%>%
  group_by(Gender, Demographics.Response)%>%
  summarise(Value = mean(Value,na.rm=T))%>%
  mutate(Gender = as.factor(Gender),
         Demographics.Response = as.factor(Demographics.Response))

burns_food_age<-data%>%
  filter(Question == "if she burns the food")%>%
  filter(Demographics.Question == "Age")%>%
  group_by(Gender, Demographics.Response)%>%
  summarise(Value = mean(Value,na.rm=T))%>%
  mutate(Gender = as.factor(Gender),
         Demographics.Response = as.factor(Demographics.Response))

burns_food_resid<-data%>%
  filter(Question == "if she burns the food")%>%
  filter(Demographics.Question == "Residence")%>%
  group_by(Gender, Demographics.Response)%>%
  summarise(Value = mean(Value,na.rm=T))%>%
  mutate(Gender = as.factor(Gender),
         Demographics.Response = as.factor(Demographics.Response))

burns_food_empl<-data%>%
  filter(Question == "if she burns the food")%>%
  filter(Demographics.Question == "Employment")%>%
  group_by(Gender, Demographics.Response)%>%
  summarise(Value = mean(Value,na.rm=T))%>%
  mutate(Gender = as.factor(Gender),
         Demographics.Response = as.factor(Demographics.Response))


burns_food_ttl <- textGrob("A husband is justified in hitting or beating his wife if she burns the food", gp = gpar(fontsize = 20))

grid.arrange(
ggplot(burns_food_marital, aes(x = Demographics.Response, y = Value, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#A2B5CD",  "#B452CD")) +
  labs(x = "Marital status", y = "% of people surveyed agree with the question", fill = "Gender")+
  scale_y_continuous(limits = c(0, 35))+
  theme_classic()+ 
  theme(legend.title = element_blank(),
        axis.title.y = element_text(size = 14)),

ggplot(burns_food_educ, aes(x = Demographics.Response, y = Value, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#A2B5CD",  "#B452CD")) +
  labs(x = "Education", y = "", fill = "Gender")+
  scale_y_continuous(limits = c(0, 35))+
  theme_classic()+ 
  theme(legend.title = element_blank()),

ggplot(burns_food_age, aes(x = Demographics.Response, y = Value, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#A2B5CD",  "#B452CD")) +
  labs(x = "Age", y = "", fill = "Gender")+
  scale_y_continuous(limits = c(0, 35))+
  theme_classic()+ 
  theme(legend.title = element_blank()),

ggplot(burns_food_resid, aes(x = Demographics.Response, y = Value, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#A2B5CD",  "#B452CD")) +
  labs(x = "Residence", y = "% of people surveyed agree with the question", fill = "Gender")+
  scale_y_continuous(limits = c(0, 35))+
  theme_classic()+ 
  theme(legend.title = element_blank(),
        axis.title.y = element_text(size = 14)),

ggplot(burns_food_empl, aes(x = Demographics.Response, y = Value, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#A2B5CD",  "#B452CD")) +
  labs(x = "Employment", y = "", fill = "Gender")+
  scale_y_continuous(limits = c(0, 35))+
  theme_classic()+ 
  theme(legend.title = element_blank()),
ncol = 3, nrow = 2, top = burns_food_ttl)


# 2) if she neglects the children

neglects_children_marital<-data%>%
  filter(Question == "if she neglects the children")%>%
  filter(Demographics.Question == "Marital status")%>%
  group_by(Gender, Demographics.Response)%>%
  summarise(Value = mean(Value,na.rm=T))%>%
  mutate(Gender = as.factor(Gender),
         Demographics.Response = as.factor(Demographics.Response))

neglects_children_educ<-data%>%
  filter(Question == "if she neglects the children")%>%
  filter(Demographics.Question == "Education")%>%
  group_by(Gender, Demographics.Response)%>%
  summarise(Value = mean(Value,na.rm=T))%>%
  mutate(Gender = as.factor(Gender),
         Demographics.Response = as.factor(Demographics.Response))

neglects_children_age<-data%>%
  filter(Question == "if she neglects the children")%>%
  filter(Demographics.Question == "Age")%>%
  group_by(Gender, Demographics.Response)%>%
  summarise(Value = mean(Value,na.rm=T))%>%
  mutate(Gender = as.factor(Gender),
         Demographics.Response = as.factor(Demographics.Response))

neglects_children_resid<-data%>%
  filter(Question == "if she neglects the children")%>%
  filter(Demographics.Question == "Residence")%>%
  group_by(Gender, Demographics.Response)%>%
  summarise(Value = mean(Value,na.rm=T))%>%
  mutate(Gender = as.factor(Gender),
         Demographics.Response = as.factor(Demographics.Response))

neglects_children_empl<-data%>%
  filter(Question == "if she neglects the children")%>%
  filter(Demographics.Question == "Employment")%>%
  group_by(Gender, Demographics.Response)%>%
  summarise(Value = mean(Value,na.rm=T))%>%
  mutate(Gender = as.factor(Gender),
         Demographics.Response = as.factor(Demographics.Response))


neglects_children_ttl <- textGrob("A husband is justified in hitting or beating his wife if she neglects the children", gp = gpar(fontsize = 20))

grid.arrange(
  ggplot(neglects_children_marital, aes(x = Demographics.Response, y = Value, fill = Gender)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("#A2B5CD",  "#B452CD")) +
    labs(x = "Marital status", y = "% of people surveyed agree with the question", fill = "Gender")+
    scale_y_continuous(limits = c(0, 35))+
    theme_classic()+ 
    theme(legend.title = element_blank(),
          axis.title.y = element_text(size = 14)),
  
  ggplot(neglects_children_educ, aes(x = Demographics.Response, y = Value, fill = Gender)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("#A2B5CD",  "#B452CD")) +
    labs(x = "Education", y = "", fill = "Gender")+
    scale_y_continuous(limits = c(0, 35))+
    theme_classic()+ 
    theme(legend.title = element_blank()),
  
  ggplot(neglects_children_age, aes(x = Demographics.Response, y = Value, fill = Gender)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("#A2B5CD",  "#B452CD")) +
    labs(x = "Age", y = "", fill = "Gender")+
    scale_y_continuous(limits = c(0, 35))+
    theme_classic()+ 
    theme(legend.title = element_blank()),
  
  ggplot(neglects_children_resid, aes(x = Demographics.Response, y = Value, fill = Gender)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("#A2B5CD",  "#B452CD")) +
    labs(x = "Residence", y = "% of people surveyed agree with the question", fill = "Gender")+
    scale_y_continuous(limits = c(0, 35))+
    theme_classic()+ 
    theme(legend.title = element_blank(),
          axis.title.y = element_text(size = 14)),
  
  ggplot(neglects_children_empl, aes(x = Demographics.Response, y = Value, fill = Gender)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("#A2B5CD",  "#B452CD")) +
    labs(x = "Employment", y = "", fill = "Gender")+
    scale_y_continuous(limits = c(0, 35))+
    theme_classic()+ 
    theme(legend.title = element_blank()),
  ncol = 3, nrow = 2, top = neglects_children_ttl)

# 3) if she goes out without telling him

goes_out_marital<-data%>%
  filter(Question == "if she goes out without telling him")%>%
  filter(Demographics.Question == "Marital status")%>%
  group_by(Gender, Demographics.Response)%>%
  summarise(Value = mean(Value,na.rm=T))%>%
  mutate(Gender = as.factor(Gender),
         Demographics.Response = as.factor(Demographics.Response))

goes_out_educ<-data%>%
  filter(Question == "if she goes out without telling him")%>%
  filter(Demographics.Question == "Education")%>%
  group_by(Gender, Demographics.Response)%>%
  summarise(Value = mean(Value,na.rm=T))%>%
  mutate(Gender = as.factor(Gender),
         Demographics.Response = as.factor(Demographics.Response))

goes_out_age<-data%>%
  filter(Question == "if she goes out without telling him")%>%
  filter(Demographics.Question == "Age")%>%
  group_by(Gender, Demographics.Response)%>%
  summarise(Value = mean(Value,na.rm=T))%>%
  mutate(Gender = as.factor(Gender),
         Demographics.Response = as.factor(Demographics.Response))

goes_out_resid<-data%>%
  filter(Question == "if she goes out without telling him")%>%
  filter(Demographics.Question == "Residence")%>%
  group_by(Gender, Demographics.Response)%>%
  summarise(Value = mean(Value,na.rm=T))%>%
  mutate(Gender = as.factor(Gender),
         Demographics.Response = as.factor(Demographics.Response))

goes_out_empl<-data%>%
  filter(Question == "if she goes out without telling him")%>%
  filter(Demographics.Question == "Employment")%>%
  group_by(Gender, Demographics.Response)%>%
  summarise(Value = mean(Value,na.rm=T))%>%
  mutate(Gender = as.factor(Gender),
         Demographics.Response = as.factor(Demographics.Response))


goes_out_ttl <- textGrob("A husband is justified in hitting or beating his wife if she goes out without telling him", gp = gpar(fontsize = 20))

grid.arrange(
  ggplot(goes_out_marital, aes(x = Demographics.Response, y = Value, fill = Gender)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("#A2B5CD",  "#B452CD")) +
    labs(x = "Marital status", y = "% of people surveyed agree with the question", fill = "Gender")+
    scale_y_continuous(limits = c(0, 35))+
    theme_classic()+ 
    theme(legend.title = element_blank(),
          axis.title.y = element_text(size = 14)),
  
  ggplot(goes_out_educ, aes(x = Demographics.Response, y = Value, fill = Gender)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("#A2B5CD",  "#B452CD")) +
    labs(x = "Education", y = "", fill = "Gender")+
    scale_y_continuous(limits = c(0, 35))+
    theme_classic()+ 
    theme(legend.title = element_blank()),
  
  ggplot(goes_out_age, aes(x = Demographics.Response, y = Value, fill = Gender)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("#A2B5CD",  "#B452CD")) +
    labs(x = "Age", y = "", fill = "Gender")+
    scale_y_continuous(limits = c(0, 35))+
    theme_classic()+ 
    theme(legend.title = element_blank()),
  
  ggplot(goes_out_resid, aes(x = Demographics.Response, y = Value, fill = Gender)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("#A2B5CD",  "#B452CD")) +
    labs(x = "Residence", y = "% of people surveyed agree with the question", fill = "Gender")+
    scale_y_continuous(limits = c(0, 35))+
    theme_classic()+ 
    theme(legend.title = element_blank(),
          axis.title.y = element_text(size = 14)),
  
  ggplot(goes_out_empl, aes(x = Demographics.Response, y = Value, fill = Gender)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("#A2B5CD",  "#B452CD")) +
    labs(x = "Employment", y = "", fill = "Gender")+
    scale_y_continuous(limits = c(0, 35))+
    theme_classic()+ 
    theme(legend.title = element_blank()),
  ncol = 3, nrow = 2, top = goes_out_ttl)



# 4) if she refuses to have sex with him

refuses_sex_marital<-data%>%
  filter(Question == "if she refuses to have sex with him")%>%
  filter(Demographics.Question == "Marital status")%>%
  group_by(Gender, Demographics.Response)%>%
  summarise(Value = mean(Value,na.rm=T))%>%
  mutate(Gender = as.factor(Gender),
         Demographics.Response = as.factor(Demographics.Response))

refuses_sex_educ<-data%>%
  filter(Question == "if she refuses to have sex with him")%>%
  filter(Demographics.Question == "Education")%>%
  group_by(Gender, Demographics.Response)%>%
  summarise(Value = mean(Value,na.rm=T))%>%
  mutate(Gender = as.factor(Gender),
         Demographics.Response = as.factor(Demographics.Response))

refuses_sex_age<-data%>%
  filter(Question == "if she refuses to have sex with him")%>%
  filter(Demographics.Question == "Age")%>%
  group_by(Gender, Demographics.Response)%>%
  summarise(Value = mean(Value,na.rm=T))%>%
  mutate(Gender = as.factor(Gender),
         Demographics.Response = as.factor(Demographics.Response))

refuses_sex_resid<-data%>%
  filter(Question == "if she refuses to have sex with him")%>%
  filter(Demographics.Question == "Residence")%>%
  group_by(Gender, Demographics.Response)%>%
  summarise(Value = mean(Value,na.rm=T))%>%
  mutate(Gender = as.factor(Gender),
         Demographics.Response = as.factor(Demographics.Response))

refuses_sex_empl<-data%>%
  filter(Question == "if she refuses to have sex with him")%>%
  filter(Demographics.Question == "Employment")%>%
  group_by(Gender, Demographics.Response)%>%
  summarise(Value = mean(Value,na.rm=T))%>%
  mutate(Gender = as.factor(Gender),
         Demographics.Response = as.factor(Demographics.Response))


refuses_sex_ttl <- textGrob("A husband is justified in hitting or beating his wife if she refuses to have sex with him", gp = gpar(fontsize = 20))

grid.arrange(
  ggplot(refuses_sex_marital, aes(x = Demographics.Response, y = Value, fill = Gender)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("#A2B5CD",  "#B452CD")) +
    labs(x = "Marital status", y = "% of people surveyed agree with the question", fill = "Gender")+
    scale_y_continuous(limits = c(0, 35))+
    theme_classic()+ 
    theme(legend.title = element_blank(),
          axis.title.y = element_text(size = 14)),
  
  ggplot(refuses_sex_educ, aes(x = Demographics.Response, y = Value, fill = Gender)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("#A2B5CD",  "#B452CD")) +
    labs(x = "Education", y = "", fill = "Gender")+
    scale_y_continuous(limits = c(0, 35))+
    theme_classic()+ 
    theme(legend.title = element_blank()),
  
  ggplot(refuses_sex_age, aes(x = Demographics.Response, y = Value, fill = Gender)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("#A2B5CD",  "#B452CD")) +
    labs(x = "Age", y = "", fill = "Gender")+
    scale_y_continuous(limits = c(0, 35))+
    theme_classic()+ 
    theme(legend.title = element_blank()),
  
  ggplot(refuses_sex_resid, aes(x = Demographics.Response, y = Value, fill = Gender)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("#A2B5CD",  "#B452CD")) +
    labs(x = "Residence", y = "% of people surveyed agree with the question", fill = "Gender")+
    scale_y_continuous(limits = c(0, 35))+
    theme_classic()+ 
    theme(legend.title = element_blank(),
          axis.title.y = element_text(size = 14)),
  
  ggplot(refuses_sex_empl, aes(x = Demographics.Response, y = Value, fill = Gender)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("#A2B5CD",  "#B452CD")) +
    labs(x = "Employment", y = "", fill = "Gender")+
    scale_y_continuous(limits = c(0, 35))+
    theme_classic()+ 
    theme(legend.title = element_blank()),
  ncol = 3, nrow = 2, top = refuses_sex_ttl)

# 5) argues with him

argues_marital<-data%>%
  filter(Question == "if she argues with him")%>%
  filter(Demographics.Question == "Marital status")%>%
  group_by(Gender, Demographics.Response)%>%
  summarise(Value = mean(Value,na.rm=T))%>%
  mutate(Gender = as.factor(Gender),
         Demographics.Response = as.factor(Demographics.Response))

argues_educ<-data%>%
  filter(Question == "if she argues with him")%>%
  filter(Demographics.Question == "Education")%>%
  group_by(Gender, Demographics.Response)%>%
  summarise(Value = mean(Value,na.rm=T))%>%
  mutate(Gender = as.factor(Gender),
         Demographics.Response = as.factor(Demographics.Response))

argues_age<-data%>%
  filter(Question == "if she argues with him")%>%
  filter(Demographics.Question == "Age")%>%
  group_by(Gender, Demographics.Response)%>%
  summarise(Value = mean(Value,na.rm=T))%>%
  mutate(Gender = as.factor(Gender),
         Demographics.Response = as.factor(Demographics.Response))

argues_resid<-data%>%
  filter(Question == "if she argues with him")%>%
  filter(Demographics.Question == "Residence")%>%
  group_by(Gender, Demographics.Response)%>%
  summarise(Value = mean(Value,na.rm=T))%>%
  mutate(Gender = as.factor(Gender),
         Demographics.Response = as.factor(Demographics.Response))

argues_empl<-data%>%
  filter(Question == "if she argues with him")%>%
  filter(Demographics.Question == "Employment")%>%
  group_by(Gender, Demographics.Response)%>%
  summarise(Value = mean(Value,na.rm=T))%>%
  mutate(Gender = as.factor(Gender),
         Demographics.Response = as.factor(Demographics.Response))


argues_ttl <- textGrob("A husband is justified in hitting or beating his wife if she argues with him", gp = gpar(fontsize = 20))

grid.arrange(
  ggplot(argues_marital, aes(x = Demographics.Response, y = Value, fill = Gender)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("#A2B5CD",  "#B452CD")) +
    labs(x = "Marital status", y = "% of people surveyed agree with the question", fill = "Gender")+
    scale_y_continuous(limits = c(0, 35))+
    theme_classic()+ 
    theme(legend.title = element_blank(),
          axis.title.y = element_text(size = 14)),
  
  ggplot(argues_educ, aes(x = Demographics.Response, y = Value, fill = Gender)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("#A2B5CD",  "#B452CD")) +
    labs(x = "Education", y = "", fill = "Gender")+
    scale_y_continuous(limits = c(0, 35))+
    theme_classic()+ 
    theme(legend.title = element_blank()),
  
  ggplot(argues_age, aes(x = Demographics.Response, y = Value, fill = Gender)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("#A2B5CD",  "#B452CD")) +
    labs(x = "Age", y = "", fill = "Gender")+
    scale_y_continuous(limits = c(0, 35))+
    theme_classic()+ 
    theme(legend.title = element_blank()),
  
  ggplot(argues_resid, aes(x = Demographics.Response, y = Value, fill = Gender)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("#A2B5CD",  "#B452CD")) +
    labs(x = "Residence", y = "% of people surveyed agree with the question", fill = "Gender")+
    scale_y_continuous(limits = c(0, 35))+
    theme_classic()+ 
    theme(legend.title = element_blank(),
          axis.title.y = element_text(size = 14)),
  
  ggplot(argues_empl, aes(x = Demographics.Response, y = Value, fill = Gender)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("#A2B5CD",  "#B452CD")) +
    labs(x = "Employment", y = "", fill = "Gender")+
    scale_y_continuous(limits = c(0, 35))+
    theme_classic()+ 
    theme(legend.title = element_blank()),
  ncol = 3, nrow = 2, top = argues_ttl)

# 6) for at least one specific reason

specific_reason_marital<-data%>%
  filter(Question == "for at least one specific reason")%>%
  filter(Demographics.Question == "Marital status")%>%
  group_by(Gender, Demographics.Response)%>%
  summarise(Value = mean(Value,na.rm=T))%>%
  mutate(Gender = as.factor(Gender),
         Demographics.Response = as.factor(Demographics.Response))

specific_reason_educ<-data%>%
  filter(Question == "for at least one specific reason")%>%
  filter(Demographics.Question == "Education")%>%
  group_by(Gender, Demographics.Response)%>%
  summarise(Value = mean(Value,na.rm=T))%>%
  mutate(Gender = as.factor(Gender),
         Demographics.Response = as.factor(Demographics.Response))

specific_reason_age<-data%>%
  filter(Question == "for at least one specific reason")%>%
  filter(Demographics.Question == "Age")%>%
  group_by(Gender, Demographics.Response)%>%
  summarise(Value = mean(Value,na.rm=T))%>%
  mutate(Gender = as.factor(Gender),
         Demographics.Response = as.factor(Demographics.Response))

specific_reason_resid<-data%>%
  filter(Question == "for at least one specific reason")%>%
  filter(Demographics.Question == "Residence")%>%
  group_by(Gender, Demographics.Response)%>%
  summarise(Value = mean(Value,na.rm=T))%>%
  mutate(Gender = as.factor(Gender),
         Demographics.Response = as.factor(Demographics.Response))

specific_reason_empl<-data%>%
  filter(Question == "for at least one specific reason")%>%
  filter(Demographics.Question == "Employment")%>%
  group_by(Gender, Demographics.Response)%>%
  summarise(Value = mean(Value,na.rm=T))%>%
  mutate(Gender = as.factor(Gender),
         Demographics.Response = as.factor(Demographics.Response))


specific_reason_ttl <- textGrob("A husband is justified in hitting or beating his wife for at least one specific reason", gp = gpar(fontsize = 20))

grid.arrange(
  ggplot(specific_reason_marital, aes(x = Demographics.Response, y = Value, fill = Gender)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("#A2B5CD",  "#B452CD")) +
    labs(x = "Marital status", y = "% of people surveyed agree with the question", fill = "Gender")+
    scale_y_continuous(limits = c(0, 35))+
    theme_classic()+ 
    theme(legend.title = element_blank(),
          axis.title.y = element_text(size = 14)),
  
  ggplot(specific_reason_educ, aes(x = Demographics.Response, y = Value, fill = Gender)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("#A2B5CD",  "#B452CD")) +
    labs(x = "Education", y = "", fill = "Gender")+
    scale_y_continuous(limits = c(0, 35))+
    theme_classic()+ 
    theme(legend.title = element_blank()),
  
  ggplot(specific_reason_age, aes(x = Demographics.Response, y = Value, fill = Gender)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("#A2B5CD",  "#B452CD")) +
    labs(x = "Age", y = "", fill = "Gender")+
    scale_y_continuous(limits = c(0, 35))+
    theme_classic()+ 
    theme(legend.title = element_blank()),
  
  ggplot(specific_reason_resid, aes(x = Demographics.Response, y = Value, fill = Gender)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("#A2B5CD",  "#B452CD")) +
    labs(x = "Residence", y = "% of people surveyed agree with the question", fill = "Gender")+
    scale_y_continuous(limits = c(0, 35))+
    theme_classic()+ 
    theme(legend.title = element_blank(),
          axis.title.y = element_text(size = 14)),
  
  ggplot(specific_reason_empl, aes(x = Demographics.Response, y = Value, fill = Gender)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("#A2B5CD",  "#B452CD")) +
    labs(x = "Employment", y = "", fill = "Gender")+
    scale_y_continuous(limits = c(0, 35))+
    theme_classic()+ 
    theme(legend.title = element_blank()),
  ncol = 3, nrow = 2, top = specific_reason_ttl)
