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
          COG.Raw,
          Ravens.Age,
          MFB.1,
          MFB.2,
          MFB.Both
            ))

Interesting$Diagnosis <- as.numeric(Interesting$Diagnosis)
Interesting$Diagnosis[Interesting$Diagnosis == "1"] <- 0
Interesting$Diagnosis[Interesting$Diagnosis == "2"] <- 1
Interesting$MFB.Both <- as.numeric(Interesting$MFB.Both)
Interesting$MFB.Both[Interesting$MFB.Both == "1"] <- 0
Interesting$MFB.Both[Interesting$MFB.Both == "2"] <- 1
Interesting$TimeSpentGms <- as.numeric(Interesting$TimeSpentGms)
Interesting$TimeSpentFms <- as.numeric(Interesting$TimeSpentFms)
Interesting$MFB.1 <- as.numeric(Interesting$MFB.1)
Interesting$MFB.1[Interesting$MFB.1 == "1"] <- 0
Interesting$MFB.1[Interesting$MFB.1 == "2"] <- 1
Interesting$MFB.2 <- as.numeric(Interesting$MFB.2)
Interesting$MFB.2[Interesting$MFB.2 == "1"] <- 0
Interesting$MFB.2[Interesting$MFB.2 == "2"] <- 1

# Standardize before taking correlations
Interesting$SRS.Raw <- scale(Interesting$SRS.Raw, center = TRUE)
Interesting$MOT.T <- scale(Interesting$MOT.T, center = TRUE)
Interesting$Ravens.Age <- scale(Interesting$Ravens.Age, center = TRUE)
Interesting$TimeSpentGms <- scale(Interesting$TimeSpentGms, center = TRUE)
Interesting$TimeSpentFms <- scale(Interesting$TimeSpentFms, center = TRUE)
Interesting$Fixations <- scale(Interesting$Fixations, center = TRUE)
Interesting$COG.Raw <- scale(Interesting$COG.Raw, center = TRUE)

#Remove observations with missing data
Interesting <- Interesting[complete.cases(Interesting),]

#This tells us how many factors we should expect to pull out
# (Remember it's just an estimate, there are tons of other ways to estimate this
# and multiple sources suggest trying a few and picking the most conservative)
png('plots/EFA/ParallelAnalysis.png')
fa1 <- fa.parallel(Interesting[3:12])
dev.off()

#plot everything
#pairs.panels(Interesting)
png('plots/EFA/CorrelationsAll.png')
cor.plot(Interesting[2:12])
dev.off()

Ball <- Interesting[Interesting$AOI %in% c("MFB1Ball","MFB2Ball"),]
Eyes <- Interesting[Interesting$AOI %in% c("MFB1Eyes","MFB2Eyes"),]
Ball1 <- Ball[Ball$AOI == "MFB1Ball",]
Eyes1 <- Eyes[Eyes$AOI == "MFB1Eyes",]
Ball2 <- Ball[Ball$AOI == "MFB2Ball",]
Eyes2 <- Eyes[Eyes$AOI == "MFB2Eyes",]

#Ball
png('plots/EFA/CorrelationsBall.png')
cor.plot(Ball[3:11], main = "Correlations (Ball), all subjects")
dev.off()
png('plots/EFA/CorrelationsBallMFB1.png')
cor.plot(Ball1[3:11], main = "Correlations (Ball) MFB test 1")
dev.off()
png('plots/EFA/CorrelationsBallMFB2.png')
cor.plot(Ball2[3:11], main = "Correlations (Ball) MFB test 2")
dev.off()
png('plots/EFA/CorrelationsBallMFBFail.png')
cor.plot(Ball[2:10][Ball$MFB.Both == 0,], main = "Correlations (Ball), MFB = fail")
dev.off()
png('plots/EFA/CorrelationsBallMFBPass.png')
cor.plot(Ball[2:10][Ball$MFB.Both == 1,], main = "Correlations (Ball), MFB = pass")
dev.off()
#Eyes
png('plots/EFA/CorrelationsEyes.png')
cor.plot(Eyes[3:11], main = "Correlations (Eyes), all subjects")
dev.off()
png('plots/EFA/CorrelationsEyesMFB1.png')
cor.plot(Eyes1[3:11], main = "Correlations (Eyes) MFB test 1")
dev.off()
png('plots/EFA/CorrelationsEyesMFB2.png')
cor.plot(Eyes2[3:11], main = "Correlations (Eyes) MFB test 2")
dev.off()
png('plots/EFA/CorrelationsEyesMFBFail.png')
cor.plot(Eyes[3:10][Eyes$MFB.Both == 0,], main = "Correlations (Eyes), MFB = fail")
dev.off()
png('plots/EFA/CorrelationsEyesMFBPass.png')
cor.plot(Eyes[3:10][Eyes$MFB.Both == 1,], main = "Correlations (Eyes), MFB = pass")
dev.off()

#sampling adequacy
correlations <- cor(Interesting[2:12])
KMO(correlations)

#subsets
Ball$AOI <- NULL
Eyes$AOI <- NULL
Ball1$AOI <- NULL
Eyes1$AOI <- NULL
Ball2$AOI <- NULL
Eyes2$AOI <- NULL

png('plots/EFA/HeirarchicalFactorModelAll.png')
omega(correlations, nfactors = 3, sl = FALSE, title = "Heirarchical Factor Model")
dev.off()

png('plots/EFA/HeirarchicalFactorModelBall.png')
omega(Ball, nfactors = 3, sl = FALSE, title = "Heirarchical Factor Model (Ball)")
dev.off()
png('plots/EFA/HeirarchicalFactorModelEyes.png')
omega(Eyes, nfactors = 3, sl = FALSE, title = "Heirarchical Factor Model (Eyes)")
dev.off()

png('plots/EFA/HeirarchicalFactorModelBall1.png')
omega(Ball1, nfactors = 3, sl = FALSE, title = "Heirarchical Factor Model (Ball) MFB test 1")
dev.off()
png('plots/EFA/HeirarchicalFactorModelBall2.png')
omega(Ball2, nfactors = 3, sl = FALSE, title = "Heirarchical Factor Model (Ball) MFB test 2")
dev.off()
png('plots/EFA/HeirarchicalFactorModelEyes1.png')
omega(Eyes1, nfactors = 3, sl = FALSE, title = "Heirarchical Factor Model (Eyes) MFB test 1")
dev.off()
png('plots/EFA/HeirarchicalFactorModelEyes2.png')
omega(Eyes2, nfactors = 3, sl = FALSE, title = "Heirarchical Factor Model (Eyes) MFB test 2")
dev.off()

