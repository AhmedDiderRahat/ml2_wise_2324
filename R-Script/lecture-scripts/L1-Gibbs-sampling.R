# clear env-variable
rm(list = ls(all.names = TRUE))

# install the mice package
install.packages("mice")

# load the mice package
library("mice")

data("airquality")

# pattern of the data
md.pattern(airquality)


# Imputation Using Gibbs sampling
tempData <- mice(airquality, m = 5, maxit = 50, meth = 'norm', seed = 500)

tempData$imp$Ozone

# To obtain a full data set with the 1st imputation.
completedData <- complete(tempData, 1)

md.pattern(completedData)


# fitting model using imputed data
modelFit1 <- with(tempData, lm(Temp ~ Ozone + Solar.R + Wind))

summary(pool(modelFit1))
