## Script No. 2 ##
# Makes adjustments

source('~/Desktop/false belief/Code/falsebelief.R')
library(Hmisc)

#Standardize data
## Ravens scores vary with age:
cor.test(EverythingClean$Ravens, EverythingClean$Age)

#Deal w/ 8 missing values
# We decided not to impute missing values
#Ravens.Imputed <- impute(EverythingClean$Ravens, mean)
#EverythingClean <- cbind(EverythingClean, Ravens.Imputed)
#
EverythingClean <- EverythingClean[complete.cases(EverythingClean$Ravens),]

## Fit model:
lm1<- lm(Ravens ~ Age, data= EverythingClean)
## Now residuals are not correlated with age:
cor.test(lm1$residuals, EverythingClean$Age)

#Add to Ravens.Age to Everything df
Ravens.Age <- lm1$residuals
EverythingClean <- cbind(EverythingClean, Ravens.Age)

## SCQ scores vary with gender:
cor.test(EverythingClean$SCQ, as.numeric(EverythingClean$Gender))

#Deal w/ 32 missing values
SCQ.Imputed <- impute(EverythingClean$SCQ, mean)
EverythingClean <- cbind(EverythingClean, SCQ.Imputed)

## Fit model:
lm1<- lm(SCQ.Imputed ~ as.numeric(Gender), data= EverythingClean)
## Now residuals are not correlated with gender:
cor.test(lm1$residuals, as.numeric(EverythingClean$Gender))

#Add to SCQ.Gender to Everything df
SCQ.Gender <- lm1$residuals
EverythingClean <- cbind(EverythingClean, SCQ.Gender)


## MFB scores vary with age:
cor.test(as.numeric(EverythingClean$MFB.Both), EverythingClean$Age)

## Fit model:
lm1<- lm(as.numeric(MFB.Both) ~ Age, data= EverythingClean)
## Now residuals are not correlated with age:
cor.test(lm1$residuals, EverythingClean$Age)

#Add to Ravens.Age to Everything df
MFB.Age <- lm1$residuals
EverythingClean <- cbind(EverythingClean, MFB.Age)
