
# Chapter 2: Classical Test Theory ----------------------------------------

library("hemp")

gender_nominal<-ifelse(interest$gender==1,"female","male")
age_nominal<-cut(interest$age,breaks=seq(10,70,by=10))
age_ordinal<-ordered(age_nominal)

table(gender_nominal)
table(age_nominal)
table(age_ordinal)

ftable(gender_nominal,age_ordinal)
ftable(age_ordinal,gender_nominal)

age_table<-table(age_ordinal)
prop.table(age_table)

prop.table(ftable(gender_nominal,age_ordinal))

library(lattice)
barchart(age_ordinal)
dotplot(age_table)

age_gender_table<-ftable(gender_nominal,age_ordinal)
age_gender_df<-data.frame(age_gender_table)

dotplot(age_ordinal~Freq|gender_nominal,age_gender_df,xlab="Freq",ylab="Age")

summary(interest$vocab)

qqnorm(interest$vocab,ylab="vocab")
qqline(interest$vocab)

num_miss(SAPA)

split_half(SAPA,type = "alternate")

set.seed(1)
split_half(SAPA,type="random")

split_half(SAPA,type="alternate",sb=TRUE)

test_length(SAPA,r=.95,r_type="split")
test_length(SAPA,r=.95,r_type = 0.8623436)
test_length(SAPA,r=.95,r_type = 0.758)

coef_alpha(SAPA)

library(boot)

alpha_fun<-function(data,row){
  coef_alpha(data[row,])
}

alpha_boot<-boot(SAPA,alpha_fun,R=1e4)

alpha_boot

plot(alpha_boot)

boot.ci(alpha_boot,type=c("norm","basic","perc","bca"))

cvr(20,17)

cor(interest[,c("vocab","reading","sentcomp")])

mod_old<-lm(teacher~socdom,interest)
mod_new<-lm(teacher~socdom+reading,interest)

summary(mod_new)$r.squared-summary(mod_old)$r.squared

anova(mod_old,mod_new)

item_diff<-colMeans(SAPA,na.rm=TRUE)
round(item_diff,3)

total_score<-rowSums(SAPA,na.rm=TRUE)

table(total_score,useNA = "always")

item_discr<-cor(SAPA,total_score,use = "pairwise.complete.obs")

item_discr

idi(SAPA,SAPA$reason.4,perc_cut = .27)

iri(SAPA)

ivi(item=SAPA$reason.4,crit = SAPA$reason.17)

?multiplechoice

distract(multiplechoice)

idi(multiplechoice,multiplechoice$item5,0.27)