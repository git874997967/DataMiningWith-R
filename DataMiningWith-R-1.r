library(DMwR)
library(VIM)
library(tidyverse)
library(InformationValue)
library(car)
library(fscaret)
library(rpart)
library(rpart.plot)
####study in chap 2
mtcars
table(mtcars$gear)
prop.table(mtcars$gear)
##gl  generate levels
gender = gl(labels = c("F", "M"), 3, 4)
###rnorm fuction to genreate random numbers based on certain distribution
set.seed(4321)
rnorm(4, mean = 10, sd = 3)
mtcars[mtcars$cyl == 6,]
subset(mtcars, cyl == 6)
### load the data
head(algae)
str(algae)
sum(is.na(algae))
aggr(algae)
par(mfrow = c(1, 1))
hist(algae$mxPH, prob = T, ylim = 0:1)
lines(density(algae$mxPH, na.rm = T))
rug(jitter(algae$mxPH))
qq.plot(algae$mxPH)
mxPh=identify(algae$mxPH)
algae[mxPh,]
hist(log(algae$oPO4))
boxplot(algae$oPO4)
## basic  boxplot and hist plot is ok to analysis the numeric vars

###use the clicked.lines=identify to check out outstanding node 
## that is awesome

plot(algae$NH4)
clicked.lines = identify(algae$NH4)
algae[clicked.lines, ]
###
library(lattice)
bwplot(size~a1,data=algae)
WOE(algae$size,algae$season)
 library(Hmisc)
bwplot(size~a1,data=algae,panel = panel.bpplot,datadensity=T)

###one way to treat na value is delete them at all  
###with complete 

apply(algae,1,function(x){sum(is.na(x))})

algae[199,]
algae=centralImputation(algae)
aggr(algae)
####
WOE(algae$season,algae$size)

str(algae)
cor(algae[,4:18],use='complete.obs')
 corrplot(algae[,4:18])
 symnum(cor(algae[,4:18]))
 ###   add new features and othere analysis after generate the corplot(matrix)
 histogram(~mxPH| size* speed,data=algae)
histogram(~mpg|cyl,data=mtcars)
mtcars$cyl
 sum(is.na(algae$oPO4))
sum(is.na(algae$PO4))
lm.model=lm(log(oPO4)~log(PO4)+PO4+oPO4,data=algae)
summary(lm.model)
colName=colnames(algae)
colName=colName[-c(9,11:18)]
col='log(a1+1)'
for(name in colName){
  print(name)
  col=paste(col,name,sep='+')
}
# dummAl=dummyVars(algae)
lm.a1=lm(log(a1+1)~size+speed+Cl+log(NO3+1)+log(PO4+1),data=algae[,1:12])
lm.a2=lm(log(a1+1)+season+size+speed+mxPH+mnO2+Cl+NO3+NH4+PO4,data=algae[,1:12])
summary(lm.a1)
empty=lm(log(a1+1)~1,data=algae[,1:12])
full=lm(log(a1+1)~.,data=algae[,1:12])
lm.step=step(full,scope = list(lower=full,upper=empty),direction = 'backward')
anova(lm.step)
####use rpart desion tree
rpart.model=rpart(a1~.,data=algae[1:12],cp=0.0001)
rpart.plot(rpart.model)
rpart.model$cptable
rpart.model
prettyTree(rpart.model)
plot(rpart.model)
text(rpart.model)
summary(rpart.model$cptable)
 prune(rpart.model,cp=0.08)
rpartXse(a1~.,algae[1:12,],se=1,cp=0.008)
snip.rpart(rpart.model)
#clean.algae
 
