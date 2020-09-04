library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tidyverse)
covid <- read_csv("https://covid.ourworldindata.org/data/ecdc/total_cases.csv") %>% 
  select(-2) %>% 
  pivot_longer(-date, names_to = "Country", values_to = "Cases" ) %>% 
  filter(Country == "Kenya")
cleanedcovid <- covid[is.na(covid$Cases) == F, ] 

# Line graph
ggplot(data = cleanedcovid , aes(x = date , y = Cases )) +
  geom_line(color = "red") +
  labs(title = "Covid in Kenya", x ="", y = "Confirmed cases",
       subtitle = "Data source: MoH", caption = "Plot by: Ezra") +
  theme_test()

# Boxplot

ggplot() +
  geom_boxplot(data =ToothGrowth, aes(x = supp, y =len)) +
  labs(x ="Intervention ", y = "Length gain", title = "Intervention vs placebo") +
  theme_test()

ggplot(ToothGrowth, aes(x = supp, y =len)) +
  geom_jitter(width = .2) +
  labs(x ="Intervention ", y = "Length gain") +
  theme_test()

ggplot(ToothGrowth, aes(x = supp, y =len)) +
  geom_violin() +
  labs(x ="Intervention ", y = "Length gain") +
  theme_test()

t.test(len~supp, data = ToothGrowth)

# COl and bars

ggplot(ToothGrowth, aes(x = supp, y =len)) +
  geom_col(width = .4, fill = "green") +
  labs(x ="Intervention ", y = "Length gain") +
  theme_test()

ggplot(ToothGrowth, aes(x = supp)) +
  geom_bar(width = .4, fill = "green") +
  labs(x ="Intervention ", y = "Length gain") +
  theme_test()



dat <- read_csv("https://covid.ourworldindata.org/data/ecdc/total_cases.csv") %>% 
  select("date", "Kenya", "Uganda", "Rwanda") %>% 
  pivot_longer(-date, names_to = "Country", values_to = "Cases" )

cleaneddat <- dat[is.na(dat$Cases) == F, ] 


ggplot(data = cleaneddat , aes(x = date , y = Cases, color = Country )) +
  geom_line() +
  theme_bw()

dat1 <- dat %>% 
  filter(date == "2020-06-12")


ggplot(dat1, aes(x = Country, y = Cases)) +
  geom_col(width = .4, fill = "turquoise") +
  geom_text(aes(label = Cases), vjust = -0.2, size =3, color = "red") +
  theme_bw()

#GGPLOT using COVID DATA
  ##selecting  total cases+deaths and Kenya
library(tidyverse)
cov<- read_csv("https://covid.ourworldindata.org/data/ecdc/full_data.csv")%>%
  select(-c(new_cases, new_deaths))%>%
  filter(location=="Kenya")
 
#trends in cases and mortality in Kenya
  ##trends in total cases
ggplot(data=cov,aes(x=date,y=total_cases))+
         geom_line(colour="red")+
         labs(title="Trends in total cases",subtitle="Our world in data",caption ="etoo")+
         theme_bw()
   
  ##trends in total cases and deaths
ggplot(data=cov)+
  geom_line(aes(x=date,y=total_cases),colour="red")+
  geom_line(aes(x=date,y=total_deaths),colour="blue") +
  labs(title="Trends in covid cases$deaths", subtitle="source:WHO",x="",y="Outcomes")+
  theme_grey()

##alternative(select then pivot)
cov0<-pivot_longer(data=cov,-c(date,location),names_to = "Metric",values_to = "Counts")
ggplot(data=cov0,aes(x=date,y=Counts,colour=Metric))+
  geom_line()+
  labs(title="Trends in covid cases$deaths", subtitle="source:WHO",x="",y="Outcomes")+
  theme_grey()

#comparing trends in Kenya and Italy
   ##selecting Kenya and Italy
cov1<- read_csv("https://covid.ourworldindata.org/data/ecdc/total_cases.csv") %>% 
  select(date, Kenya,Italy) 

   ##comparing
ggplot(cov1)+
  geom_line(aes(x=date,y=Kenya),colour="red")+
  geom_line(aes(x=date,y=Italy),colour="blue")+
  labs(title="Trends in Kenya and Italy",x="",y="Cases")

  ##converting to longer version$comparing
cov2 <- pivot_longer(cov1,-date,names_to="Country",values_to="Cases")
  ggplot(data=cov2,aes(x=date,y=Cases,colour=Country))+
  geom_line()+
    labs(title="Trends in Kenya and Italy")
  
##Kenya trends
    library(tidyverse)
cov3<-read_csv("https://covid.ourworldindata.org/data/ecdc/full_data.csv")%>%
  pivot_longer(-c(date,location),names_to="Metric",values_to="Counts")%>%
  filter(location=="Kenya")
  ggplot(data=cov3,aes(x=date,y=Counts,colour=Metric))+
  geom_line()+labs(title="Covid trends in Kenya")+
  theme_classic()

###17th June
##Refresher
Dat1<- read_csv("https://covid.ourworldindata.org/data/ecdc/full_data.csv")
head(Dat1)
select(Dat1,total_cases,total_deaths)
filter(Dat1,location=="Brazil")
filter(Dat1,total_deaths>10000)
Dat1[location=="Brazil", c("new_deaths","total_cases")]

###part two
###comparing UK,US,CHINA, and ITALY
####total cases and deaths
cv<-read_csv("https://covid.ourworldindata.org/data/ecdc/full_data.csv")%>%
  select(-c(new_cases,new_deaths))%>%
  filter(location==c("China","Italy","United States","United Kingdom"))%>%
  pivot_longer(-c(date,location),names_to="Metric",values_to="Counts")
  ggplot(data=cv,aes(x=date,y=Counts,colour=Metric))+
    geom_line()
  
  ####trends in total_cases
  cv1<-read_csv("https://covid.ourworldindata.org/data/ecdc/full_data.csv")%>%
    select(-c(new_cases,new_deaths,total_deaths))%>%
    filter(location==c("China","Italy","United States","United Kingdom"))
  ggplot(data=cv1,aes(x=date,y=total_cases,colour=location))+
    geom_line()+
    labs(title="Trends in total cases")
  
####trends in total deaths
  cv2<-read_csv("https://covid.ourworldindata.org/data/ecdc/full_data.csv")%>%
    select(-c(new_cases,new_deaths,total_cases))%>%
    filter(location==c("China","Italy","United States","United Kingdom"))
  ggplot(data=cv2,aes(x=date,y=total_deaths,colour=location))+
    geom_line()+
    labs(title="Trends in total deaths")
  
  ###finding the means of total deaths
  sum<-read_csv("https://covid.ourworldindata.org/data/ecdc/full_data.csv")%>%
    select(-c(new_cases,new_deaths,total_cases))%>%
    filter(location==c("China","Italy","United States","United Kingdom"))%>%
    group_by(location)%>%
    summarise(meandeaths=mean(total_deaths))
  
  ###boxplots
  ##Dataset(ToothGrowth)
ggplot(data=ToothGrowth,aes(x=supp,y=len))+
  geom_boxplot()+
  labs(title = "Comparing interventions vs control",x="intervention",y="length")
t.test(len~supp,data=ToothGrowth)
    
###regression analysis
####Linear regression
ToothGrowth
ggplot(data=ToothGrowth,aes(x=supp,y=len))+
  geom_boxplot()
summary(lm(len~supp + dose, data = ToothGrowth))

library(sjPlot)

tab_model(lm(len~supp + dose, data = ToothGrowth))

# Logistic regression
setwd("C:/Users/etoo/Downloads/Systematic review/CMDs/Work-ups/R lessons/Datasets")

titanicdata <- read_csv("titanic_data.csv") %>% 
  mutate(Survived = factor(Survived, levels = c(0, 1), labels = c("Died", "Survived")),
         Pclass = factor(Pclass),
         Sex = factor(Sex),
         Embarked = factor(Embarked))

head(titanicdata)
ggplot(data = titanicdata, aes(x = Pclass, fill = Survived)) +
  geom_bar()

ggplot(data = titanicdata, aes(x = Sex, fill = Survived)) +
  geom_bar()

ggplot(data = titanicdata, aes(x = Age, fill = Survived)) +
  geom_histogram(alpha = .4)

summary(glm(Survived~Pclass, data = titanicdata, family = "binomial"))
tab_model(glm(Survived~Pclass + Sex + Age + Embarked, data = titanicdata, family = "binomial"))

Pcancer<-read_csv("Prostate_Cancer.csv")
ggplot(data=Pcancer,aes(x=area,y=diagnosis_result))+
  geom_col()
tab_model(glm(diagnosis_result~area,data=Pcancer,family="binomial"))

# Poisson regression

awards <- read_csv("poisson_sim.csv")
head(awards)

summary(glm(num_awards~prog, data = awards, family = "poisson"))

tab_model(glm(num_awards~ factor(prog) + math , data = awards, family = "poisson"))
tab_model(glm(num_awards~ math , data = awards, family = "poisson"))



# Analysis of variance
head(iris)
ggplot(iris, aes(x = Species, y = Sepal.Width)) +
  geom_boxplot()

mod1 <- aov(Sepal.Width~Species, data = iris)

summary(mod1)

TukeyHSD(mod1)

#####11th July logistic regression of titanic data
##
library(tidyverse)
library(sjPlot)
setwd("C:/Users/etoo/Downloads/Systematic review/CMDs/Work-ups/R lessons/Datasets")
read.csv("titanic_data.csv", header=TRUE)
titanicdata<-read_csv("titanic_data.csv")%>%
  mutate(Survuved=factor(Survied, levels=c(0,1),labels = c("Died","Survived"))
  ggplot(data=titanicdata, aes(x=Pclass, fill=Survived))+
  geom_bar()
  tab_model(glm(Survived~Pclass+Age+Fare+Embarked+Sex, data = titanicdata,family = "binomial"))
  
  ggplot(data=titanicdata, aes(x=Sex, fill=Survived))+
    geom_bar()
  
  ggplot(data=titanicdata, aes(x=Age, fill=Survived))+
    geom_histogram()  
  
  ggplot(data=titanicdata, aes(x=Embarked, fill=Survived))+
    geom_bar()
  
  ggplot(data=titanicdata, aes(x=Fare, fill=Survived))+
    geom_histogram()
  ##poison regression
  poissondata<-read_csv("poisson_sim.csv")%>%
    mutate(prog=factor(prog))
  ggplot(data=poissondata,aes(x=num_awards,fill=prog))+
    geom_bar()
  tab_model(glm(num_awards~math+prog, data=poissondata, family = "poisson"))
  
  sr<-read_csv("Dataset.csv")%>%
    mutate(Study_design=factor(Study_design,levels = c("Cross-sectional","Case-control","Cohort"),labels = c(0,1,2)))

  review<-read_csv("Dataset.csv")
  table(is.na(review$Age))
    tr<-mean(review$Age, na.rm=TRUE)
    table(is.na(tr$Age))
    
    ###Manuscript odds ratios

setwd("C:/Users/etoo/Downloads/Systematic review/CMDs/Work-ups/Excel files")
  reg<-read.csv("Book1.csv", header=TRUE) %>%
  mutate(HIV.status=factor(HIV.status, levels=c(0,1),labels=c("Negative","Positive")),
         outcome=factor(outcome, levels= c(0,1), labels=c("Depressed","Not depressed")))
         
ggplot(data = reg, aes(x = HIV.status, y = prop)) +
  geom_col()

tab_model(glm(outcome~HIV.status, data=reg,family = "binomial"))
