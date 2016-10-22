## Script No. 3 ##
# exploring data

source('~/Desktop/false belief/Code/standardize.R')


#Pick out some interesting stuff
colnames(EverythingClean)
Interesting <- subset(EverythingClean, select = c(
          AOI,
          Diagnosis,
          TimeSpentGms, 
          TimeSpentFms,
          Fixations,
          SRS.Raw,
          MOT.T,
          Ravens.Age,
          COG.Raw,
          MFB.1,
          MFB.2,
          MFB.Both
            ))

Interesting$Diagnosis <- as.numeric(Interesting$Diagnosis)
Interesting$MFB.Both <- as.numeric(Interesting$MFB.Both)
Interesting$AOI <- as.numeric(Interesting$AOI)
Interesting$TimeSpentGms <- as.numeric(Interesting$TimeSpentGms)
Interesting$TimeSpentFms <- as.numeric(Interesting$TimeSpentFms)
Interesting$MFB.1 <- as.numeric(Interesting$MFB.1)
Interesting$MFB.2 <- as.numeric(Interesting$MFB.2)

# Standardize before taking correlations
Interesting$MFB.Both <- scale(Interesting$MFB.Both, center = TRUE)
Interesting$SRS.Raw <- scale(Interesting$SRS.Raw, center = TRUE)
Interesting$MOT.T <- scale(Interesting$MOT.T, center = TRUE)
Interesting$Ravens.Age <- scale(Interesting$Ravens.Age, center = TRUE)
Interesting$TimeSpentGms <- scale(Interesting$TimeSpentGms, center = TRUE)
Interesting$TimeSpentFms <- scale(Interesting$TimeSpentFms, center = TRUE)
Interesting$Fixations <- scale(Interesting$Fixations, center = TRUE)
Interesting$COG.Raw <- scale(Interesting$COG.Raw, center = TRUE)
Interesting$AWR.Raw <- scale(Interesting$AWR.Raw, center = TRUE)
Interesting$HitTime <- scale(Interesting$HitTime, center = TRUE)
Interesting$MFB.1 <- scale(Interesting$MFB.1, center = TRUE)
Interesting$MFB.2 <- scale(Interesting$MFB.2, center = TRUE)


#Remove observations with missing data
Interesting <- Interesting[complete.cases(Interesting),]


#This tells us how many factors we should expect to pull out
# (Remember it's just an estimate, there are tons of other ways to estimate this
# and multiple sources suggest trying a few and picking the most conservative)
fa1 <- fa.parallel(Interesting[3:12])

#plot everything
pairs.panels(Interesting)
cor.plot(Interesting[3:9])

#Ball
cor.plot(Interesting[3:9][Interesting$AOI %in% c(1,3),], main = "Correlations (Ball), all subjects")
cor.plot(Interesting[3:8][Interesting$AOI %in% c(1,3) & Interesting$MFB.Both < 0,], main = "Correlations (Ball) MFB=fail")
cor.plot(Interesting[3:8][Interesting$AOI %in% c(1,3) & Interesting$MFB.Both > 0,], main = "Correlations (Ball) MFB=pass")
#Eyes
cor.plot(Interesting[3:9][Interesting$AOI %in% c(2,4),], main = "Correlations (Eyes), all subjects")
cor.plot(Interesting[3:8][Interesting$AOI %in% c(2,4) & Interesting$MFB.Both < 0,], main = "Correlations (Eyes) MFB=pass")
cor.plot(Interesting[3:8][Interesting$AOI %in% c(2,4) & Interesting$MFB.Both > 0,], main = "Correlations (Eyes) MFB=fail")

correlations <- cor(Interesting[2:12])
KMO(correlations)


Balls <- Interesting[Interesting$AOI %in% c(1,3),]
Balls$AOI <- NULL
Eyes <- Interesting[Interesting$AOI %in% c(2,4),]
Eyes$AOI <- NULL
Balls1 <- Interesting[Interesting$AOI ==1,]
Balls1$AOI <- NULL
Eyes1 <- Interesting[Interesting$AOI ==2,]
Eyes1$AOI <- NULL
Balls2 <- Interesting[Interesting$AOI ==3,]
Balls2$AOI <- NULL
Eyes2 <- Interesting[Interesting$AOI ==4,]
Eyes2$AOI <- NULL

omega(correlations, nfactors = 3, sl = FALSE, title = "Heirarchical Factor Model")

omega(Balls, nfactors = 3, sl = FALSE, title = "Heirarchical Factor Model (Ball)")
omega(Eyes, nfactors = 3, sl = FALSE, title = "Heirarchical Factor Model (Eyes)")

omega(Balls1, nfactors = 3, sl = FALSE, title = "Heirarchical Factor Model (Ball) MFB test 1")
omega(Balls2, nfactors = 3, sl = FALSE, title = "Heirarchical Factor Model (Ball) MFB test 2")
omega(Eyes1, nfactors = 3, sl = FALSE, title = "Heirarchical Factor Model (Eyes) MFB test 1")
omega(Eyes2, nfactors = 3, sl = FALSE, title = "Heirarchical Factor Model (Eyes) MFB test 2")

