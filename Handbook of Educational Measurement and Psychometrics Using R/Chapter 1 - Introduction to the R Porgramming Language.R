# Chapter 1: Introduction to the R Porgramming Language -------------------

library(devtools)
install_github("cddesja/hemp")
library("hemp")

data(package="hemp")

data(rse)

head(rse,3)

rse[rse$country=="TW"& rse$age<35,]

?reshape

rse_sub<-subset(rse,select = Q1:Q10)
rse_cov<-cov(rse_sub)
round(rse_cov,2)

rse_cor<-cor(rse_sub)
round(rse_cor,2)

cor.test(rse$Q1,rse$Q2)

rse_gender<-subset(rse,gender==1|gender==2)
a<-t.test(age~gender,data=rse_gender)

mod1<-lm(Q1~Q2+Q3,data=rse)

names(mod1)

summary(mod1)

pred_vales<-predict(mod1)
res_values<-resid(mod1)

plot(mod1)

par(mfrow=c(2,2),mar=c(2,4.1,2,2))
plot(mod1)

par(mfrow=c(1,1),mar=c(5, 4, 4, 2) + 0.1)
plot(vocab~mathmtcs,data=interest,col=gender)
lines(lowess(interest$mathmtcs,interest$vocab))

verbal<-subset(interest,select=c(vocab,reading,sentcomp))
pairs(verbal)

hist(interest$vocab)
boxplot(interest$vocab)

boxplot(interest$vocab~interest$gender)

stem(interest$vocab)

xyplot(vocab~reading,data = interest)

xyplot(vocab~reading | gender,data = interest)

xyplot(vocab~reading, group=gender,data = interest)

citation()

toBibtex(citation("mirt"))

install.packages(c("boot","difR"))
install.packages(c("boot","difR","equate","faoutlier","GPArotation","lattice","lavaan","lme4","mirt","psych","semPlot","shiny","devtools"))
