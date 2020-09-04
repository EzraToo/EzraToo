library(tidyverse)
library(sjPlot)
project1<-read_csv("femaleMiceWeights.csv")
ass1<-project1[12,2]
ass2<-project1$Bodyweight 
ass3<- project1 [13:24,2]%>%
  unlist()
mean(ass3)

setwd("C:/Users/etoo/Downloads/Systematic review/CMDs/Work-ups/R lessons/R scripts/Project1")
project2<-read.csv("msleep_ggplot2.csv",header = TRUE)
class(project2)
pj2a<-filter(project2,order=="Primates") %>%
  select(sleep_total)%>%
  unlist()
mean(pj2a)
nrow(pj2a)

pj2b<-filter(project2,order=="Primates")%>%
  select(sleep_total)
summarise(pj2b)


project3<-InsectSprays
ggplot(data=project3,aes(x=spray,y=count))+
  geom_boxplot()+
  labs(title="Comparing the effectiveness of different types of insect spray")+
  theme_bw()


project4<-nym.2002
ggplot(data=project4,aes(x=gender,y=time))+
  geom_boxplot()
t.test(time~gender,data=project4)

total_male<-nym.2002%>%
  filter(gender=="Male")
ggplot(data=total_male,aes(x=time))+
  geom_histogram()+
  labs(title="Male histogram")

total_female<-nym.2002%>%
  filter(gender=="Female")
ggplot(data=total_female,aes(x=time))+
  geom_histogram()+
  labs(title="Female histogram")

project5<-read_csv("femaleControlsPopulation.csv") %>% 
  unlist
pj5a<-mean(unlist(project5))
set.seed(1)
pj5b<-mean(sample(project5,5))
abs(pj5b-pj5a)
set.seed(5)
pj5c<-mean(sample(project5,5))
abs(pj5c-pj5a)

#####for loop
set.seed(1)
n <- 1000
averages5 <- vector("numeric",n)
for(i in 1:n){
  X <- sample(x,5)
  averages5[i] <- mean(X)
}
hist(averages5) 
mean( abs( averages5 - mean(x) ) > 1)

set.seed(1)
n <- 10000
averages5 <- vector("numeric",n)
for(i in 1:n){
  X <- sample(x,5)
  averages5[i] <- mean(X)
}
hist(averages5) 
mean( abs( averages5 - mean(x) ) > 1)

set.seed(1)
n <- 1000
averages50 <- vector("numeric",n)
for(i in 1:n){
  X <- sample(x,50)
  averages50[i] <- mean(X)
}
hist(averages50) 
mean( abs( averages50 - mean(x) ) > 1)


project6<-gapminder
pj6a<- filter(project6,year==1952) %>% 
  select("country",lifeExp)

# make averages5
set.seed(1)
n <- 1000
averages5 <- vector("numeric",n)
for(i in 1:n){
  X <- sample(x,5)
  averages5[i] <- mean(X)
}
hist(averages5)

# make averages50
set.seed(1)
n <- 1000
averages50 <- vector("numeric",n)
for(i in 1:n){
  X <- sample(x,50)
  averages50[i] <- mean(X)
}
hist(averages50)
mean( averages50 < 25 & averages50 > 23)
pnorm( (25-23.9) / 0.43)  - pnorm( (23-23.9) / 0.43)

mice<-read_csv("mice_pheno.csv")
cmice<-na.omit(mice) %>%
  filter(Sex=="M",Diet=="chow")
vector1<-cmice$Bodyweight
mean(vector1)
sd(vector1)

set.seed(1)
mean(sample(vector1,25))
sd(sample(vector1,25))

c_mice<-na.omit(mice) %>%
  filter(Sex=="M",Diet=="hf")
vector2<-c_mice$Bodyweight

mean(vector2)
sd(vector2)

set.seed(1)
mean(sample(vector2,25))
sd(sample(vector1,25))

x<-mean(vector2) - mean(vector1)
y<-mean(sample(vector2,25)) - mean(sample(vector1,25))
abs(x-y)

mice<-read_csv("mice_pheno.csv")
cmice<-na.omit(mice) %>%
  filter(Sex=="F",Diet=="chow")
vector3<-cmice$Bodyweight
mean(vector3)
sd(vector3)

set.seed(2)
mean(sample(vector3,25))

c_mice<-na.omit(mice) %>%
  filter(Sex=="F",Diet=="hf")
vector4<-c_mice$Bodyweight

mean(vector4)

set.seed(2)
mean(sample(vector4,25))

x<-mean(vector4) - mean(vector3)
y<-mean(sample(vector4,25)) - mean(sample(vector3,25))
abs(x-y)
abs(mean((vector4) - mean(vector3)) - mean((sample(vector4,25)) - mean(sample(vector3,25))))

##percentage proportions with varying sds from the average
micedata<-read_csv("mice_pheno.csv") %>%
  na.omit()
pnorm(1)-pnorm(-1)
pnorm(2)-pnorm(-2)
pnorm(3)-pnorm(-3)

micd<-micedata<-read_csv("mice_pheno.csv") %>%
  na.omit() %>% 
  filter(Sex=="M", Diet=="chow")

y<-micd$Bodyweight

z <- ( y - mean(y) ) / sd(y)
mean( abs(z) <=1 )

x<- (y-mean(y)) / sd(y)
mean(abs(x) <=2)

k<- (y-mean(y)) / sd(y)
mean(abs(k) <=3)

qqnorm(z)
abline(0,1)

mypar(2,2)
y <- filter(micedata, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / sd(y)
qqnorm(z);abline(0,1)
y <- filter(micedata, Sex=="F" & Diet=="chow") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / sd(y)
qqnorm(z);abline(0,1)
y <- filter(micedata, Sex=="M" & Diet=="hf") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / sd(y)
qqnorm(z);abline(0,1)
y <- filter(micedata, Sex=="F" & Diet=="hf") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / sd(y)
qqnorm(z);abline(0,1)

##correlation
project8<-read_csv("assoctest.csv")
prj8a<-table(project8$allele,project8$case)
  chisq.test(prj8a)
  fisher.test(prj8a)
  
  ##Scatter plot
project9<-nym.2002
prj9a<-filter(project9, gender=="Male")
x<-prj9a$age
y<-prj9a$time
  xy<-plot(x,y,main="correlation btw age and time",xlab = "age",ylab = "time to finish",
       frame=TRUE)
    abline(lm(y~x))
    lines(lowess(x,y))
    cor.test(x,y,method = "pearson")
    cor(x,y,method=("pearson"))
    
prj9b<-filter(project9, gender=="Female")
x<-prj9b$age
y<-prj9b$time
xy<-plot(x,y,main="correlation btw age and time",xlab = "age",ylab = "time to finish",
         frame=TRUE)
abline(lm(y~x))
lines(lowess(x,y))
cor.test(x,y,method = "pearson")
cor(x,y,method=("pearson"))

project10<-nym.2002 
time<-sort(project10$time)
min(time)
median(time)
min(time)/median(time)
max(time)/median(time)
tab1<-select(project10$time)

###plots of ratios vs log ratios
plot(time/median(time), ylim=c(1/4,4))
abline(h=c(1/2,1,2))

plot(log2(time/median(time)),ylim=c(-2,2))
abline(h=-1:1)

###comparing mead/median/mad with$without outliers
project11<-ChickWeight
plot(project11$Time,project11$weight,col=project11$Diet)
chick = reshape(project11, idvar=c("Chick","Diet"), timevar="Time",
                direction="wide")
*pivot_wider(project11,-c(weight,Time),)
chick=na.omit(chick)
length(chick$weight.4)

s<-mean(unlist(chick$weight.4))
ss<-mean(c(unlist(chick$weight.4),3000))
ss/s

s1<-median(unlist(chick$weight.4))
ss1<-median(c(unlist(chick$weight.4),3000))
ss1/s1

s2<-sd(unlist(chick$weight.4))
ss2<-sd(c(unlist(chick$weight.4),3000))
ss2/s2

s3<-mad(unlist(chick$weight.4))
ss3<-mad(c(unlist(chick$weight.4),3000))
ss3/s3

plot(chick$weight.4,chick$weight.21)
cor<-cor(chick$weight.4,chick$weight.21)

chick1=c(unlist(chick$weight.4),3000)
chick2=c(unlist(chick$weight.21),3000)
plot(chick1,chick2)
cor1<-cor(chick1,chick2)
cor1/cor

plot(chick$weight.4,chick$weight.21,method="spearman")
spearman<-cor(chick$weight.4,chick$weight.21, method="spearman")

plot(chick1,chick2, method="spearman")
spearman1<-cor(chick1,chick2, method="spearman")
spearman1/spearman

###man-whiteney wilconxon test
project12<-filter(chick,Diet==4)
 y<-project12$weight.4
 
project13<-filter(chick,Diet==1)
x<-c(project13$weight.4,200)
t.test(x,y)$p.value
wilcox.test(x,y)$p.value

x<-project13$weight.4
y<-project12$weight.4
mypar(1,3)
boxplot(x,y)
boxplot(x,y+10)
boxplot(x,y+100)

wilcox.test(c(1,2,3),c(4,5,6))$p.value
wilcox.test(c(1,2,3),c(400,500,600))$p.value
t.test(x,y+10)$statistic - t.test(x,y+100)$statistic

wilcox.test(c(1,2,3))$p.value - wilcox.test(c(4,5,6))$p.value
wilcox.test(c(400,500,600))$p.value


x<-father.son$fheight
hist(x,breaks = seq(floor(min(x)),ceiling(max(x))), main="Height of fathers",
     xlab= "Height in inches")

load("skew.RData")
par(mfrow = c(3,3))
for (i in 1:9) {
  qqnorm(dat [,i])
  qqline(dat [,i])
}
par(mfrow=c(1,1))
hist(dat [,4])

project14<- read.table("babies.txt", header = TRUE)
bwt.nonsmoke <- filter(project14, smoke==0) %>% dplyr::select(bwt) %>% unlist 
bwt.smoke <- filter(project14, smoke==1) %>% dplyr::select(bwt) %>% unlist

mean(bwt.nonsmoke) - mean(bwt.smoke)
sd(bwt.nonsmoke)
sd(bwt.smoke)

set.seed(1)
snonsmokers<-sample(bwt.nonsmoke,5)
ssmokers<-sample(bwt.smoke,5)

tval<- t.test(snonsmokers,ssmokers)
pval <- 1-(pnorm(abs(tval))-pnorm(-abs(tval)))
2*pnorm(-abs(tval))

(2*N-2(tval))

N <- 25
set.seed(1)
dat.ns <- sample(bwt.nonsmoke, N) 
dat.s <- sample(bwt.smoke, N) 
qt(0.995,48)*sqrt( sd( dat.ns)^2/N + sd( dat.s)^2/N )
##note that if you define dat.s before dat.ns, you get a different answer
##due to sampling randomness
##tolerance is set to accept both answers


#######Power calculation
##For Loop to rerun the code in project 14 10,000 times
N<-5
set.seed(1)
rejects <- replicate(10000,{
  dat.ns <- sample(bwt.nonsmoke , N)
  dat.s <- sample(bwt.smoke , N)
  t.test(dat.s, dat.ns)$p.value < 0.05
})
mean(rejects)

##generating random sample then computing t.statistic
set.seed(1)
N <- 5
X <- rnorm(N)
sqrt(N)*mean(X)/sd(X)

set.seed(1)
b<-replicate(1000,{
  X <- rnorm(N)
  sqrt(N)*mean(X)/sd(X)
})
mean(b>2)

####lesson two
project15<-father.son %>%
  filter(fheight>71.1,fheight<71.5) %>%
  dplyr::select(sheight) %>%
  round(digits=0) %>%
  unlist

  mean(project15)
  
  ###vectors & matrices
  X = matrix(1:1000,100,10) 
X[25,3]

x=matrix(1:10)
cbind(x,2*x,3*x,4*x,5*x)
sum(x[7, ])

a <- matrix(1:12, nrow=4)
b <- matrix(1:15, nrow=3)
a*b
x<-a%*%b
x[3,2]
