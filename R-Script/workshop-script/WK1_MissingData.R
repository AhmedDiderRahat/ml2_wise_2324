#Workshop 1: Imputing missing values

#Uncomment the following if you need to install the following packages 

#install.packages("mice")
#install.packages("VIM")
#install.packages("NHANES")
library(mice)
library(VIM)


#Exercise 1 
#(a) 
data(tao) #in VIM package

nrow(tao) # 736
ncol(tao) # 8

?tao
table(tao$Year)
#there are only two years and we do not need the numeric value of Year in the regression model, 
# so convert it to a factor
tao$Year<-as.factor(tao$Year) 
plot(tao$Air.Temp,tao$Sea.Surface.Temp,col=tao$Year)

startMar<-par()$mar #store the current margin settings
par(mar=c(0,0,0,0)+0.1)
md.pattern(tao, rotate.names=TRUE) 
summary(tao)
aggr(tao)


#(b) Questions regarding the output to the above commands. 
## My Note: There are no missing values for: Year, Lat/Long, UWind, VWind
## Humidity has 93 missing values. Sea.Surface, Air.Temp, and Humidity have more than 1 missing values.



#(c)
any.missing<-!complete.cases(tao)
par(mar=startMar)
marginplot(tao[,c("Air.Temp", "Humidity")])
marginplot(tao[,c("Air.Temp","Sea.Surface.Temp")])
#Notice that the missing values appear not to be random

#(d)
#You will use the same longish model specification several times in Exercise 2, so let's give the "formula" a short name.
tao.model<-formula(Sea.Surface.Temp~Year+Longitude+Latitude+Air.Temp+ Humidity+UWind+VWind)
# here, the model is trained with all the values which doesn't contains any NaN.
#fit a linear regression model to the "fully known" observations
lm.knowns<-lm(tao.model,data=tao)
summary(lm.knowns)



#Exercise 2
#univariate imputation

#(a) Mean imputation 
mean.replace<-function(x) 
{
  idx<-which(is.na(x))  #returns the row numbers with a missing value in x
  known.mean<-mean(x,na.rm=T) 
  x[idx]<-known.mean
  
  return(x)
} 

#check using a test variable
tt<-c(1:10,NA,NA,99)
tt
mean.replace(tt)

par(mar=startMar) #reset the margins changed in previous Ex
hist(tao$Air.Temp)
#impute the Air.Temp using mean replacement
mrep.Air.Temp <- mean.replace(tao$Air.Temp)
hist(mrep.Air.Temp)

tao.mrep<-tao
tao.mrep$Air.Temp<-mrep.Air.Temp
tao.mrep$Sea.Surface.Temp<-mean.replace(tao.mrep$Sea.Surface.Temp) ## my adding
tao.mrep$Humidity <- mean.replace(tao.mrep$Humidity)
with(tao.mrep,plot(Air.Temp,Sea.Surface.Temp,col=Year))
###NB  with(data.frame,command(...))  runs command(...) knowing that variable names are to be found in data.frame
with(tao.mrep, plot(Air.Temp,Sea.Surface.Temp,col=1+any.missing)) #colour the imputed vales red. 
## my adding




lm.mrep<-lm(tao.model,data=tao.mrep)
summary(lm.mrep)

#(b)

mean.sd.replace<-function(x) 
{
  idx<-which(is.na(x))
  known.mean<-mean(x,na.rm=T) 
  known.sd<-sd(x,na.rm=T) # my adding
  x[idx]<-rnorm(length(idx),known.mean,known.sd)
  
  return(x)
}

tt<-c(1:10,NA,NA,95)
tt
mean.sd.replace(tt)
round(mean.sd.replace(tt),2)


hist(tao$Air.Temp)
#impute the Air.Temp using mean/variance simulation
msdrep.Air.Temp <- mean.sd.replace(tao$Air.Temp)
hist(msdrep.Air.Temp)

tao.msdrep<-tao
tao.msdrep$Air.Temp<-???
tao.msdrep$Sea.Surface.Temp<-???
tao.msdrep$Humidity<-???
with(tao.??,plot(Air.Temp,Sea.Surface.Temp,col=Year))
with(???,plot(??,???,col=1+any.missing))



lm.msdrep<-lm(tao.model,data=???)
summary(???)


#(c)

dir.rand.samp<-function(x) 
{  ##direct random sampling of x
  idx<-which(is.na(x))
  x[idx]<-sample(x[-idx],length(idx),replace=???)
  
  return(x)
}   

#check
tt
dir.rand.samp(tt)
#and again
dir.rand.samp(tt)


tao.drs<-tao
tao.drs$Air.Temp<-dir.rand.samp(tao$Air.Temp)
tao.drs$Sea.Surface.Temp<-dir.rand.samp(tao$Sea.Surface.Temp)
tao.drs$Humidity<-dir.rand.samp(tao$Humidity)
hist(tao.drs$Air.Temp)
plot(tao.drs$Air.Temp,tao.drs$Sea.Surface.Temp,col=tao.drs$Year)

with(tao.drs,plot(???))
with(???,plot(???))


lm.drs<-lm(tao.model,data=tao.drs)
summary(lm.drs)


#compare the coefficients from all four univariate methods
cbind(lm.knowns$coefficients,lm.mrep$coefficients,lm.msdrep$coefficients,lm.drs$coefficients)


#Exercise 3 Multivariate imputation using Gibbs sampling
#(a)
GibbsData <- mice(tao,m=5,maxit=50,meth='pmm',seed=600)

#(b)
Gibbsdata1<-complete(GibbsData,1)
#plot with missing values in red
with(Gibbsdata1,plot(Air.Temp,Sea.Surface.Temp,col=1+any.missing))

lm.Gibbs1<-lm(tao.model,data=Gibbsdata1)
summary(lm.Gibbs1)
round(cbind(lm.knowns$coefficients,lm.mrep$coefficients,lm.msdrep$coefficients,lm.drs$coefficients,lm.Gibbs1$coefficients),4)

#(c)
#run lm on all 5 complete data sets
lm.Gibbs.all<-with(GibbsData,lm(Sea.Surface.Temp~Year+Longitude+Latitude+Air.Temp+ Humidity+UWind+VWind))
#the resulst of each on 
lm.Gibbs.all$analyses 

#summary(lm.obj) for each of the 5 lms.
lapply(lm.Gibbs.all$analyses,summary)
round(sapply(lm.Gibbs.all$analyses,coefficients),4)
 
#(d)
summary(pool(lm.Gibbs.all))

# final model
lm.Gibbs.all.final<-with(GibbsData,lm(Sea.Surface.Temp~Year+Longitude+Latitude+Air.Temp+Humidity+VWind))
summary(pool(lm.Gibbs.all.final))

#Exercise 4: Imputing missing data for the Diabetes data set
#(a)
library(NHANES)
Diabetes2<-data.frame(NHANES[,c("Diabetes","Gender","Race1","BMI","Age","Pulse","BPSysAve","BPDiaAve","HealthGen","DaysPhysHlthBad","DaysMentHlthBad","LittleInterest","Depressed")])
table(Diabetes2$Diabetes)

#convert Diabetes vaiable to be a binary outcome for logistic regression
Diabetes2$Diabetes<-as.numeric(Diabetes2$Diabetes)-1

par(mar=c(0,0,0,0)+0.1)
md.pattern(Diabetes2,rotate.names = TRUE) 
#reset the margins
par(mar=startMar)

summary(Diabetes2)
aggr(Diabetes2)

any.missing<-!complete.cases(Diabetes2)

#(b)Multivariate imputation
#this takes a while!
GibbsDiabetes <- mice(???,m=5,maxit=10,meth='pmm',seed=700)

##obtain the first complete data set
Gibbs1Diabetes<-???;  
Gibbs1Diabetes<-complete(GibbsDiabetes,1)

#output a cat!
md.pattern(Gibbs1Diabetes) 



#(c) glm() with family="binomial" specifies a logistic regression
#first the logistic regression on the original known data, throwing away the missings
logreg.known<-glm(Diabetes~.,data=???,family="binomial")
summary(logreg.known)
logreg.Gibbs1<-glm(Diabetes~.,???)
summary(???)
logreg.full<-with(GibbsDiabetes,glm(Diabetes~Gender+Race1+BMI+Age+Pulse+BPSysAve+BPDiaAve+HealthGen+
                                      DaysPhysHlthBad+DaysMentHlthBad+LittleInterest+Depressed,family="binomial"))
summary(pool(???))


#remove the variable Depressed as it seems not to be significant at 5% level  
logreg.final<-with(GibbsDiabetes,glm(Diabetes~Gender+Race1+BMI+Age+Pulse+BPSysAve+BPDiaAve+HealthGen+
                                       DaysPhysHlthBad+DaysMentHlthBad,family="binomial"))
summary(pool(????))


#predictions from the logistic regression model
pr.logreg<-predict(logreg.Gibbs1,type="response")>0.5
table(pr.logreg,Diabetes2$Diabetes)


#e)
#you might have to install these packages which were used in ML1
#install.packages("rpart")
#install.packages("rpart.plot")

library(rpart)
library(rpart.plot)
tree.known<-rpart(Diabetes~.,data=Diabetes2)
rpart.plot(tree.known)

tree.Gibbs1<-rpart(Diabetes~.,data=Gibbs1Diabetes)
rpart.plot(tree.Gibbs1)



Gibbs.full<-with(GibbsDiabetes,rpart(Diabetes~Gender+Race1+BMI+Age))

pool(Gibbs.full) ###Damn!  (produces an error)

for(i in 1:5){
  tree.Gibbsfull<-rpart(Diabetes~Gender+Race1+BMI+Age+Pulse+BPSysAve+BPDiaAve+HealthGen+DaysPhysHlthBad+DaysMentHlthBad+LittleInterest+Depressed,
                        data=complete(GibbsDiabetes,i))
  rpart.plot(tree.Gibbsfull,main=i,roundint=FALSE)
}
