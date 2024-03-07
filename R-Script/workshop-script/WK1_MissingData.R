###############################################################################################
# Workshop: 01 (Dealing with Missing Data)                                                    #
# Email: adrahat.bht@gmail.com                                                                #
###############################################################################################


# Uncomment the following if you need to install the following packages 

#install.packages("mice")
#install.packages("VIM")
#install.packages("NHANES")
library(mice)
library(VIM)


###############################################################################################

# Exercise 1- The tropical atmosphere ocean data

# Exercise 1 (a)

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

sum(apply(tao, 1, function(x) sum(is.na(x)) > 1))

# Exercise 1 (b)
# My Note: There are no missing values for: Year, Lat/Long, UWind, VWind
# Humidity has 93 missing values. 
# There are 4 observation which have more than 1 missing values.


# Exercise 1 (c)
any.missing<-!complete.cases(tao)
par(mar=startMar)
marginplot(tao[,c("Air.Temp", "Humidity")])
marginplot(tao[,c("Air.Temp","Sea.Surface.Temp")])
#Notice that the missing values appear not to be random

# Exercise 1 (d)
#You will use the same longish model specification several times in Exercise 2, so 
# let's give the "formula" a short name.
tao.model <- formula(Sea.Surface.Temp ~ Year + 
                     Longitude + Latitude + 
                     Air.Temp + Humidity + 
                     UWind + VWind)

# here, the model is trained with all the values which doesn't contains any NaN.
#fit a linear regression model to the "fully known" observations
lm.knowns <- lm(tao.model, data=tao)
summary(lm.knowns)



###############################################################################################

# Exercise 2-Univariate imputation


# Exercise 2 (a) Mean imputation 
mean.replace <- function(x) 
{
  idx <- which(is.na(x))  #returns the row numbers with a missing value in x
  known.mean <- mean(x, na.rm=T) 
  x[idx] <- known.mean
  
  return(x)
}

#check using a test variable
tt<-c(1:10, NA, NA, 99)
tt
mean.replace(tt)

par(mar = startMar) #reset the margins changed in previous Ex

hist(tao$Air.Temp)
#impute the Air.Temp using mean replacement
mrep.Air.Temp <- mean.replace(tao$Air.Temp)
hist(mrep.Air.Temp)

tao.mrep <- tao
tao.mrep$Air.Temp <- mrep.Air.Temp

tao.mrep$Sea.Surface.Temp <- mean.replace(tao.mrep$Sea.Surface.Temp) ## my adding

tao.mrep$Humidity <- mean.replace(tao.mrep$Humidity)

with(tao.mrep, plot(Air.Temp, Sea.Surface.Temp, col = Year))

# NB  with(data.frame,command(...))  runs command(...) knowing that variable
# names are to be found in data.frame
with(tao.mrep, plot(Air.Temp, Sea.Surface.Temp, col = 1 + any.missing)) 

# color the imputed vales red. 
# my adding

lm.mrep <- lm(tao.model, data = tao.mrep)
summary(lm.mrep)


# Exercise 2 (b)
mean.sd.replace <- function(x) 
{
  idx <- which(is.na(x))
  known.mean <- mean(x, na.rm = T) 
  known.sd <- sd(x, na.rm = T) # my adding
  x[idx] <- rnorm(length(idx), known.mean, known.sd)
  
  return(x)
}

tt<-c(1:10, NA, NA, 95)
tt
mean.sd.replace(tt)
round(mean.sd.replace(tt), 2)

hist(tao$Air.Temp)

#impute the Air.Temp using mean/variance simulation
msdrep.Air.Temp <- mean.sd.replace(tao$Air.Temp)
hist(msdrep.Air.Temp)

tao.msdrep <- tao

tao.msdrep$Air.Temp <- mean.sd.replace(tao.msdrep$Air.Temp)

tao.msdrep$Sea.Surface.Temp <- mean.sd.replace(tao.msdrep$Sea.Surface.Temp)

tao.msdrep$Humidity <- mean.sd.replace(tao.msdrep$Humidity) 

with(tao.msdrep, plot(Air.Temp, Sea.Surface.Temp, col=Year))

with(tao.msdrep, plot(Air.Temp, Sea.Surface.Temp, col= 1 + any.missing))

lm.msdrep <- lm(tao.model, data = tao.msdrep)
summary(lm.msdrep)


# Exercise 2 (c)

##direct random sampling of x

dir.rand.samp <- function(x) 
{ 
  idx <- which(is.na(x))
  x[idx] <- sample(x[-idx], length(idx), replace = T)

  return(x)
}

#check
tt
dir.rand.samp(tt)
#and again
dir.rand.samp(tt)


tao.drs <- tao
tao.drs$Air.Temp <- dir.rand.samp(tao$Air.Temp)
tao.drs$Sea.Surface.Temp <- dir.rand.samp(tao$Sea.Surface.Temp)
tao.drs$Humidity <- dir.rand.samp(tao$Humidity)
hist(tao.drs$Air.Temp)
plot(tao.drs$Air.Temp, tao.drs$Sea.Surface.Temp, col=tao.drs$Year)

with(tao.drs, plot(Air.Temp, Sea.Surface.Temp, col= Year))

with(tao.drs, plot(Air.Temp, Sea.Surface.Temp, col= 1 + any.missing))

lm.drs <- lm(tao.model, data = tao.drs)
summary(lm.drs)


#compare the coefficients from all four univariate methods
cbind(lm.knowns$coefficients,
      lm.mrep$coefficients,
      lm.msdrep$coefficients,
      lm.drs$coefficients)



###############################################################################################

# Exercise 3-Multivariate imputation using Gibbs sampling
# Exercise 3 (a)

GibbsData <- mice(tao, 
                  m = 5, 
                  maxit = 50, 
                  meth = 'pmm', 
                  seed = 600)


# Exercise 3 (b)
Gibbsdata1 <- complete(GibbsData, 1)
#plot with missing values in red

with(Gibbsdata1, plot(Air.Temp, Sea.Surface.Temp, col = 1+any.missing))

lm.Gibbs1 <- lm(tao.model, data = Gibbsdata1)
summary(lm.Gibbs1)
round(cbind(lm.knowns$coefficients,
            lm.mrep$coefficients,
            lm.msdrep$coefficients,
            lm.drs$coefficients,
            lm.Gibbs1$coefficients), 4)


# Exercise 3 (c)
# run lm on all 5 complete data sets
lm.Gibbs.all <- with(GibbsData,
                     lm(Sea.Surface.Temp ~ Year + Longitude + Latitude + 
                          Air.Temp + Humidity + UWind + VWind))


# the results of each on 
lm.Gibbs.all$analyses

#summary(lm.obj) for each of the 5 lms.
lapply(lm.Gibbs.all$analyses, summary)

round(sapply(lm.Gibbs.all$analyses, coefficients), 4)
 
# Exercise 3 (d)
summary(pool(lm.Gibbs.all))

# final model
lm.Gibbs.all.final <- with(GibbsData,
                           lm(Sea.Surface.Temp ~ Year + Longitude + Latitude +
                                Air.Temp + Humidity + VWind))

summary(pool(lm.Gibbs.all.final))



###############################################################################################

# Exercise 4-Imputing missing data for the Diabetes data set

# Exercise 4 (a)
library(NHANES)

colsname <- c("Diabetes", "Gender", "Race1", "BMI", "Age", "Pulse", "BPSysAve",
              "BPDiaAve", "HealthGen", "DaysPhysHlthBad", "DaysMentHlthBad",
              "LittleInterest", "Depressed")

Diabetes2 <- data.frame(NHANES[, colsname])

table(Diabetes2$Diabetes)

#convert Diabetes variable to be a binary outcome for logistic regression
Diabetes2$Diabetes <- as.numeric(Diabetes2$Diabetes) - 1

par(mar=c(0,0,0,0)+0.1)
md.pattern(Diabetes2, rotate.names = TRUE)

#reset the margins
par(mar=startMar)

summary(Diabetes2)
aggr(Diabetes2)

any.missing <- !complete.cases(Diabetes2)

# Exercise 4 (b) - Multivariate imputation
# this takes a while!
GibbsDiabetes <- mice(Diabetes2, m=5, maxit=10, meth='pmm', seed=700)

# obtain the first complete data set
Gibbs1Diabetes <- complete(GibbsDiabetes, 1)

#output a cat!
md.pattern(Gibbs1Diabetes)


# Exercise 4 (c) - glm() with family="binomial" specifies a logistic regression
# first the logistic regression on the original known data, throwing away the missing

# with known data sets
logreg.known <- glm(Diabetes~., data = Diabetes2, family = "binomial")

summary(logreg.known)

# with imputed data sets

logreg.Gibbs1 <- glm(Diabetes~., data = Gibbs1Diabetes, family = "binomial")
summary(logreg.Gibbs1)

logreg.full <- with(GibbsDiabetes, 
                    glm(Diabetes ~ Gender + Race1 + BMI + Age + Pulse + BPSysAve + 
                          BPDiaAve + HealthGen + DaysPhysHlthBad + DaysMentHlthBad + 
                          LittleInterest + Depressed, family = "binomial"))

summary(pool(logreg.full))


# remove the variable Depressed as it seems not to be significant at 5% level  
logreg.final <- with(GibbsDiabetes,
                     glm(Diabetes ~ Gender + Race1 + BMI + Age + Pulse + BPSysAve + 
                           BPDiaAve + HealthGen + DaysPhysHlthBad + DaysMentHlthBad,
                         family = "binomial"))
summary(pool(logreg.final))


# predictions from the logistic regression model
pr.logreg <- predict(logreg.Gibbs1, type="response") > 0.5
table(pr.logreg, Diabetes2$Diabetes)


# Exercise 4 (e)

# you might have to install these packages which were used in ML1
# install.packages("rpart")
# install.packages("rpart.plot")

library(rpart)
library(rpart.plot)

tree.known <- rpart(Diabetes~., data = Diabetes2)
rpart.plot(tree.known)

tree.Gibbs1 <- rpart(Diabetes~., data = Gibbs1Diabetes)
rpart.plot(tree.Gibbs1)


Gibbs.full <- with(GibbsDiabetes, rpart(Diabetes ~ Gender + Race1 + BMI + Age))

pool(Gibbs.full) ###Damn!  (produces an error)

for(i in 1:5){
  tree.Gibbsfull <- rpart(Diabetes ~ Gender + Race1 + BMI + Age + Pulse + BPSysAve +
                          BPDiaAve + HealthGen + DaysPhysHlthBad + DaysMentHlthBad +
                          LittleInterest + Depressed,
                        data = complete(GibbsDiabetes, i))
  rpart.plot(tree.Gibbsfull, main = i, roundint = FALSE)
}
