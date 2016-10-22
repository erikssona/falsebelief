library(psych)

# Read CSV into R
allEg <- read.csv(file="Data/allEg.csv", header=TRUE, sep=",", stringsAsFactors = TRUE)
ToM2Ball <- read.csv(file="Data/Ball2egxToM.csv", header=TRUE, sep=",", stringsAsFactors = TRUE)
ToM2Eyes <- read.csv(file="Data/Eyes2EgXToM.csv", header=TRUE, sep=",", stringsAsFactors = TRUE)
Sheet1 <- read.csv(file="Data/Sheet1.csv", header=TRUE, sep=",", stringsAsFactors = TRUE)
ToMEyeGaze <- read.csv(file="Data/ToMEyeGaze.csv", header=TRUE, sep=",", stringsAsFactors = TRUE)
ToM1Ball <- read.csv(file="Data/ToMxBallEG1.csv", header=TRUE, sep=",", stringsAsFactors = TRUE)
ToM1Eyes <- read.csv(file="Data/TOMxEyeEG.csv", header=TRUE, sep=",", stringsAsFactors = TRUE)
Ravens <- read.csv(file="Data/RavensScores.csv", header=TRUE, sep=",", stringsAsFactors = TRUE)
Sheet1$Participant <- as.factor(gsub('-', '', Sheet1$Participant))
ToMEyeGaze$Respondent <- as.factor(ToMEyeGaze$Respondent)
ToM1Eyes$Respondent <- as.factor(ToM1Eyes$Respondent)
ToM1Ball$Respondent <- as.factor(ToM1Ball$Respondent)
ToM2Ball$Respondent <- as.factor(ToM2Ball$Respondent)

#Fix cognitive assessments data
Assessments <- Sheet1[-which(!(Sheet1$Participant %in% ToM1Ball$Respondent)),]
Assessments$Participant <- factor(Assessments$Participant)
names(Assessments)[names(Assessments) == "Participant"] <- "Respondent"
names(Ravens)[names(Ravens) == "ID"] <- "Respondent"
TestScores <- na.omit(cbind(Assessments[match(Ravens$Respondent, Assessments$Respondent),], Ravens))

#Fix eye gaze data
allEg <- allEg[-(which(allEg$Respondent == "974714b")),]
allEg <- allEg[-(which(allEg$Respondent == "974705")),]
allEg <- allEg[-(which(allEg$Respondent == "974718")),]
allEg <- unique(allEg)
EyeGaze <- allEg[-(which(allEg$Respondent == "974706" & allEg$Gender == "Female")),]
EyeGaze$Respondent <- factor(EyeGaze$Respondent)
EyeGaze <- EyeGaze[order(EyeGaze$Respondent),]
ToMEyeGaze <- ToMEyeGaze[order(ToMEyeGaze$Respondent),]
ToMEyeGaze <- cbind(ToMEyeGaze, EyeGaze$Gender)
ToMEyeGaze <- ToMEyeGaze[order(ToMEyeGaze$AOI.Name),]


ToM2Eyes <- ToMEyeGaze[which(ToMEyeGaze$AOI.Name == "ToM2 Eyes"),]
ToM2Eyes <- cbind(ToM2Eyes[match(ToM1Eyes$Respondent, ToM2Eyes$Respondent),], ToM1Eyes$Gender)

assessx4 <- rbind(Assessments, Assessments, Assessments, Assessments)
assessx4 <- assessx4[order(assessx4$Respondent),]
Everything <- cbind(ToMEyeGaze[order(ToMEyeGaze$Respondent),], assessx4)
EverythingClean <- Everything[-(which(Everything$Respondent == "974726")),]
EverythingClean <- EverythingClean[-(which(EverythingClean$Respondent == "974729")),]
names(EverythingClean)[names(EverythingClean) == "EyeGaze$Gender"] <- "Gender"
EverythingClean <- EverythingClean[order(EverythingClean$AOI.Name),]

ToM1Ball <- cbind(ToM1Ball[match(ToM1Ball$Respondent, Assessments$Respondent),], Assessments)
ToM1Eyes <- cbind(ToM1Eyes[match(ToM1Eyes$Respondent, Assessments$Respondent),], Assessments)
ToM2Ball <- cbind(ToM2Ball[match(ToM2Ball$Respondent, Assessments$Respondent),], Assessments)
ToM2Eyes <- cbind(ToM2Eyes[match(ToM2Eyes$Respondent, Assessments$Respondent),], Assessments)

ToM1BallClean <- ToM1Ball[-(which(ToM1Ball$Respondent == "974726")),]
ToM1BallClean <- ToM1BallClean[-(which(ToM1BallClean$Respondent == "974729")),]
ToM1EyesClean <- ToM1Eyes[-(which(ToM1Eyes$Respondent == "974726")),]
ToM1EyesClean <- ToM1EyesClean[-(which(ToM1EyesClean$Respondent == "974729")),]
ToM2BallClean <- ToM2Ball[-(which(ToM2Ball$Respondent == "974726")),]
ToM2BallClean <- ToM2BallClean[-(which(ToM2BallClean$Respondent == "974729")),]
ToM2EyesClean <- ToM2Eyes[-(which(ToM2Eyes$Respondent == "974726")),]
ToM2EyesClean <- ToM2EyesClean[-(which(ToM2EyesClean$Respondent == "974729")),]

AssessmentsClean <- Assessments[-(which(Assessments$Respondent == "974726")),]
AssessmentsClean <- AssessmentsClean[-(which(Assessments$Respondent == "974729")),]

#Combine Eyes and Balls,
#ToM1 and ToM2
AllEyes = cbind(ToM1EyesClean[match(ToM1EyesClean$Respondent, ToM2EyesClean$Respondent),], ToM2EyesClean)
AllBall = cbind(ToM1BallClean[match(ToM1BallClean$Respondent, ToM2BallClean$Respondent),], ToM2BallClean)
AllToM1 = cbind(ToM1EyesClean[match(ToM1EyesClean$Respondent, ToM1BallClean$Respondent),], ToM1BallClean)
AllToM2 = cbind(ToM2EyesClean[match(ToM2EyesClean$Respondent, ToM2BallClean$Respondent),], ToM2BallClean)

#Don't litter
remove(Sheet1)
remove(allEg)
remove(EyeGaze)
remove(assessx4)
remove(Assessments)
remove(Everything)
remove(ToM1Eyes)
remove(ToM2Eyes)
remove(ToM1Ball)
remove(ToM2Ball)

#Standardize data
EverythingClean$Ravens.Total <- scale(EverythingClean$Ravens.Total)
EverythingClean$Hit.time.G..ms.[EverythingClean$Hit.time.G..ms. == 0] <- NA
EverythingClean$Time.spent.G..ms.[EverythingClean$Time.spent.G..ms. == 0] <- NA
EverythingClean$Total.Raw.Score[EverythingClean$Total.Raw.Score == 0] <- NA
EverythingClean$SCQ[EverythingClean$SCQ==0] <- NA


#plot age
age = ToM1BallClean$Age
png('plots/age.png')
plot(density(age))
dev.off()

# Make some plots showing the distribution of hit time for each AOI
# Hit Time-G (ms): Arrival time, time of first sample within the AOI (for this respondent)
Hit.time = ToM1BallClean$Hit.time.G..ms.
Hit.time[Hit.time==0] <- NA
Hit.time <- na.omit(Hit.time)
png('plots/HitTimeToM1Ball.png')
plot(density(Hit.time), ylab = 'ToM1Ball', main = 'Hit Time-G (ms)')
dev.off()
Hit.time = ToM1EyesClean$Hit.time.G..ms.
Hit.time[Hit.time==0] <- NA
Hit.time <- na.omit(Hit.time)
png('plots/HitTimeToM1Eyes.png')
plot(density(Hit.time), ylab = 'ToM1Eyes', main = 'Hit Time-G (ms)')
dev.off()
Hit.time = ToM2BallClean$Hit.time.G..ms.
Hit.time[Hit.time==0] <- NA
Hit.time <- na.omit(Hit.time)
png('plots/HitTimeToM2Ball.png')
plot(density(Hit.time), ylab = 'ToM2Ball', main = 'Hit Time-G (ms)')
dev.off()
Hit.time = ToM2EyesClean$Hit.time.G..ms.
Hit.time[Hit.time==0] <- NA
Hit.time <- na.omit(Hit.time)
png('plots/HitTimeToM2Eyes.png')
plot(density(Hit.time), ylab = 'ToM2Eyes', main = 'Hit Time-G (ms)')
dev.off()

# Make some plots showing the distribution of ttff for each AOI
# TTFF_F (ms): Time to First Fixation. Timestamp of the first fixation recorded inside this AOI for this respondent.
TTFF.F..ms. = ToM1BallClean$TTFF.F..ms.
TTFF.F..ms.[TTFF.F..ms.==0] <- NA
TTFF.F..ms. <- na.omit(TTFF.F..ms.)
png("plots/TTFFToM1Ball.png")
plot(density(TTFF.F..ms.), ylab = 'ToM1Ball', main = 'TTFF-F (ms)')
dev.off()
TTFF.F..ms. = ToM1EyesClean$TTFF.F..ms.
TTFF.F..ms.[TTFF.F..ms.==0] <- NA
TTFF.F..ms. <- na.omit(TTFF.F..ms.)
png("plots/TTFFToM1Eyes.png")
plot(density(TTFF.F..ms.), ylab = 'ToM1Eyes', main = 'TTFF-F (ms)')
dev.off()
TTFF.F..ms. = ToM2BallClean$TTFF.F..ms.
TTFF.F..ms.[TTFF.F..ms.==0] <- NA
TTFF.F..ms. <- na.omit(TTFF.F..ms.)
png("plots/TTFFToM2Ball.png")
plot(density(TTFF.F..ms.), ylab = 'ToM2Ball', main = 'TTFF-F (ms)')
dev.off()
TTFF.F..ms. = ToM2EyesClean$TTFF.F..ms.
TTFF.F..ms.[TTFF.F..ms.==0] <- NA
TTFF.F..ms. <- na.omit(TTFF.F..ms.)
png("plots/TTFFToM2Eyes.png")
plot(density(TTFF.F..ms.), ylab = 'ToM2Eyes', main = 'TTFF-F (ms)')
dev.off()

# Make some plots showing the distribution of Time Spent for each AOI
# Time Spent-G (ms): Time spent in the AOI based on raw gaze data points (not fixation based)
Time.spent.G..ms. = ToM1BallClean$Time.spent.G..ms.
Time.spent.G..ms.[Time.spent.G..ms.==0] <- NA
Time.spent.G..ms. <- na.omit(Time.spent.G..ms.)
png('plots/TimeSpentToM1Ball.png')
plot(density(Time.spent.G..ms.), ylab = 'ToM1Ball', main = 'Time Spent-G (ms)')
dev.off()
Time.spent.G..ms. = ToM1EyesClean$Time.spent.G..ms.
Time.spent.G..ms.[Time.spent.G..ms.==0] <- NA
Time.spent.G..ms. <- na.omit(Time.spent.G..ms.)
png('plots/TimeSpentToM1Eyes.png')
plot(density(Time.spent.G..ms.), ylab = 'ToM1Eyes', main = 'Time Spent-G (ms)')
dev.off()
Time.spent.G..ms. = ToM2BallClean$Time.spent.G..ms.
Time.spent.G..ms.[Time.spent.G..ms.==0] <- NA
Time.spent.G..ms. <- na.omit(Time.spent.G..ms.)
png('plots/TimeSpentToM2Ball.png')
plot(density(Time.spent.G..ms.), ylab = 'ToM2Ball', main = 'Time Spent-G (ms)')
dev.off()
Time.spent.G..ms. = ToM2EyesClean$Time.spent.G..ms.
Time.spent.G..ms.[Time.spent.G..ms.==0] <- NA
Time.spent.G..ms. <- na.omit(Time.spent.G..ms.)
png('plots/TimeSpentToM2Eyes.png')
plot(density(Time.spent.G..ms.), ylab = 'ToM2Eyes', main = 'Time Spent-G (ms)')
dev.off()

# Make some plots showing the distribution of Fixation Count for each AOI
# Fixation Count = Number of fixations recorded inside this AOI for this responent
Fixations.Count = ToM1BallClean$Fixations.Count
Fixations.Count[Fixations.Count==0] <- NA
Fixations.Count <- na.omit(Fixations.Count)
png('plots/FixesToM1Ball.png')
plot(density(Fixations.Count), ylab = 'ToM1Ball', main = 'Fixation Count')
dev.off()
Fixations.Count = ToM1EyesClean$Fixations.Count
Fixations.Count[Fixations.Count==0] <- NA
Fixations.Count <- na.omit(Fixations.Count)
png('plots/FixesToM1Eyes.png')
plot(density(Fixations.Count), ylab = 'ToM1Eyes', main = 'Fixation Count')
dev.off()
Fixations.Count = ToM2BallClean$Fixations.Count
Fixations.Count[Fixations.Count==0] <- NA
Fixations.Count <- na.omit(Fixations.Count)
png('plots/FixesToM2Ball.png')
plot(density(Fixations.Count), ylab = 'ToM2Ball', main = 'Fixation Count')
dev.off()
Fixations.Count = ToM2EyesClean$Fixations.Count
Fixations.Count[Fixations.Count==0] <- NA
Fixations.Count <- na.omit(Fixations.Count)
png('plots/FixesToM2Eyes.png')
plot(density(Fixations.Count), ylab = 'ToM2Eyes', main = 'Fixation Count')
dev.off()


#Pick out some interesting stuff
Interesting <- subset(EverythingClean, select = c(Age, AOI.Name,
                                                  Hit.time.G..ms., Time.spent.G..ms.,
                                                  Total.Raw.Score,
                                                  SCQ, Ravens.Total, ToM.Both))
Interesting$Age <- as.numeric(Interesting$Age)
Interesting$ToM.Both <- as.numeric(Interesting$ToM.Both)
Interesting$AOI.Name <- as.numeric(Interesting$AOI.Name)
Interesting$Hit.time.G..ms. <- as.numeric(Interesting$Hit.time.G..ms.)
Interesting$Time.spent.G..ms. <- as.numeric(Interesting$Time.spent.G..ms.)

#Replace 0's with NA's for stats purposes
Interesting$Hit.time.G..ms.[Interesting$Hit.time.G..ms. == 0] <- NA
Interesting$Time.spent.G..ms.[Interesting$Time.spent.G..ms. == 0] <- NA
# I took out TTFF and Fixations because they were strongly linked to Hit time and Time Spent,
# respectively. It seemed redundant.
#Interesting$TTFF.F..ms.[Interesting$TTFF.F..ms. == 0] <- NA
#Interesting$Fixations.Count[Interesting$Fixations.Count == 0] <- NA
Interesting$Total.Raw.Score[Interesting$Total.Raw.Score == 0] <- NA
Interesting$SCQ[Interesting$SCQ == 0] <- NA

#Plot correlations
png('plots/Corrs/Correlations.png')
cor.plot(Interesting)
dev.off()
png('plots/Corrs/CorrsBall.png')
cor.plot(Interesting[Interesting$AOI %in% c(1,3),], main = "All Ball")
dev.off()
png('plots/Corrs/CorrsEyes.png')
cor.plot(Interesting[Interesting$AOI.Name %in% c(2,4),], main = "All Eyes")
dev.off()
png('plots/Corrs/CorrsToM1Ball.png')
cor.plot(Interesting[Interesting$AOI==1,], main = "Correlations, ToM 1 Ball")
dev.off()
png('plots/Corrs/CorrsToM1Eyes.png')
cor.plot(Interesting[Interesting$AOI.Name==2,], main = "Correlations, ToM 1 Eyes")
dev.off()
png('plots/Corrs/CorrsToM2Ball.png')
cor.plot(Interesting[Interesting$AOI==3,], main = "Correlations, ToM 2 Ball")
dev.off()
png('plots/Corrs/CorrsToM2Eyes.png')
cor.plot(Interesting[Interesting$AOI.Name==4,], main = "Correlations, ToM 2 Eyes")
dev.off()
png('plots/Corrs/CorrsBallno.png')
cor.plot(Interesting[Interesting$AOI.Name %in% c(1,3) & Interesting$ToM.Both == 1,], main = "Correlations, Ball")
dev.off()
png('plots/Corrs/CorrsEyesno.png')
cor.plot(Interesting[Interesting$AOI.Name %in% c(2,4) & Interesting$ToM.Both == 1,], main = "Correlations, Eyes")
dev.off()
png('plots/Corrs/CorrsBallYes.png')
cor.plot(Interesting[Interesting$AOI.Name %in% c(1,3) & Interesting$ToM.Both == 2,], main = "Correlations, Ball")
dev.off()
png('plots/Corrs/CorrsEyesYes.png')
cor.plot(Interesting[Interesting$AOI.Name %in% c(2,4) & Interesting$ToM.Both == 2,], main = "Correlations, Eyes")
dev.off()

SRS <- subset(AssessmentsClean, select = c(Respondent, Total.Raw.Score, T.Score,
                                          AWR.Raw, COG.Raw, COM.Raw, MOT.Raw, RRB.Raw,
                                      AWR.T.Score, COG.T.Score, COM.T.Score, MOT.T.Score, RRB.T.Score,
                                      SCQ, Ravens.Total ))
SRS$Respondent <- as.numeric(SRS$Respondent)
SRSadj <- subset(SRS, select = c(Respondent, Total.Raw.Score, T.Score, AWR.Raw, COG.Raw, COM.Raw, MOT.Raw,
                                 RRB.Raw, SCQ))

#PCA on Assessment Scores
pca1 = prcomp(na.omit(SRS[2:14]))
png('plots/scree1.png')
screeplot(pca1)
dev.off()
pca1 <- pca(SRS[2:14], nfactors=3)
png('plots/pca1.png')
plot(pca1)
dev.off()

pca2 = prcomp(na.omit(SRSadj[2:9]))
png('plots/scree2.png')
screeplot(pca2)
dev.off()
pca2 <- pca(SRSadj[2:9], nfactors=3)
png('plots/pca2.png')
plot(pca2)
dev.off()

#Factor analysis
fa3 <- fa.poly(SRS[2:14], nfactors = 3, n.obs=nrow(na.omit(SRS[2:14])), fm = 'pa', rotate='oblimin')
fa2 <- fa.poly(SRS[2:14], nfactors = 2, n.obs=nrow(na.omit(SRS[2:14])), fm = 'pa', rotate='oblimin')

Space <- subset(EverythingClean, select=c(Respondent, AOI.Name, Hit.time.G..ms., Time.spent.G..ms., Age, Gender, Total.Raw.Score, SCQ, ToM.Both))

# Plots to examine distribution
png('plots/HitTimeXAOI.png')
boxplot(Space$Hit.time.G..ms.~Space$AOI.Name, main="Hit time by AOI")
dev.off()
png('plots/TimeSpentXAOI.png')
boxplot(Space$Time.spent.G..ms.~Space$AOI.Name, main="Time spent by AOI")
dev.off()
png('plots/HitTimeXToMXAOI.png')
boxplot(Space$Hit.time.G..ms.~Space$ToM.Both*Space$AOI.Name, main="Hit time by ToM*AOI")
dev.off()
png('plots/TimeSpentXToMXAOI.png')
boxplot(Space$Time.spent.G..ms.~Space$ToM.Both*Space$AOI.Name, main="Time spent by ToM*AOI")
dev.off()

png('plots/ToMX/Diagnosis.png')
plot(EverythingClean$Diagnosis, EverythingClean$ToM.Both, xlab = "Diagnosis", ylab = "ToM", main = "ToM per Diagnosis")
dev.off()
cor
png('plots/ToMX/Age.png')
boxplot(EverythingClean$Age~EverythingClean$ToM.Both, xlab = "Age", ylab = "ToM", main = "Age per ToM")
dev.off()

png('plots/ToMX/Gender.png')
plot(EverythingClean$Gender, EverythingClean$ToM.Both, xlab = "Gender", ylab = "ToM", main = "ToM per Gender")
dev.off()

png('plots/ToMX/GenderDiag.png')
plot(EverythingClean$Gender, EverythingClean$Diagnosis, xlab = "Gender", ylab = "Diagnosis", main = "Diagnosis per Gender")
dev.off()

png('plots/ToMX/HitTime.png')
boxplot(EverythingClean$Hit.time.G..ms.~EverythingClean$ToM.Both, ylab = "Hit Time", xlab = "ToM", main = "Hit Time per ToM")
dev.off()

png('plots/ToMX/TimeSpent.png')
boxplot(EverythingClean$Time.spent.G..ms.~EverythingClean$ToM.Both, ylab = "Time Spent", xlab = "ToM", main = "Time Spent per ToM")
dev.off()

png('plots/ToMX/SCQ.png')
boxplot(EverythingClean$SCQ~EverythingClean$ToM.Both, ylab = "SCQ", xlab = "ToM", main = "SCQ per ToM")
dev.off()

png('plots/ToMX/SRS.png')
boxplot(EverythingClean$Total.Raw.Score~EverythingClean$ToM.Both, ylab = "SRS", xlab = "ToM", main = "SRS per ToM")
dev.off()

png('plots/ToMX/Ravens.png')
boxplot(EverythingClean$Ravens.Total~EverythingClean$ToM.Both, ylab = "Ravens", xlab = "ToM", main = "Ravens per ToM")
dev.off()


model <- lm(EverythingClean$Ravens.Total~EverythingClean$Age+EverythingClean$ToM.Both)

EverythingBall <- EverythingClean[EverythingClean$AOI.Name %in% c("ToM 1 Ball", "ToM2 Ball"),]
EverythingEyes <- EverythingClean[EverythingClean$AOI.Name %in% c("ToM1 Eyes", "ToM2 Eyes"),]

EverythingNo <- EverythingClean[EverythingClean$ToM.Both=="no",]
EverythingNoBall <- EverythingNo[EverythingNo$AOI.Name %in% c("ToM 1 Ball", "ToM2 Ball"),]
EverythingNoEyes <- EverythingNo[EverythingNo$AOI.Name %in% c("ToM1 Eyes", "ToM2 Eyes"),]
EverythingYes <- EverythingClean[EverythingClean$ToM.Both=="Yes",]
EverythingYesBall <- EverythingYes[EverythingYes$AOI.Name %in% c("ToM 1 Ball", "ToM2 Ball"),]
EverythingYesEyes <- EverythingYes[EverythingYes$AOI.Name %in% c("ToM1 Eyes", "ToM2 Eyes"),]
EverythingToM1 <- EverythingClean[EverythingClean$AOI.Name %in% c("ToM 1 Ball","ToM1 Eyes"),]
EverythingToM1No <- EverythingToM1[EverythingToM1$ToM.Both=="no",]
EverythingToM1Yes <- EverythingToM1[EverythingToM1$ToM.Both=="Yes",]
EverythingToM2 <- EverythingClean[EverythingClean$AOI.Name %in% c("ToM2 Ball", "ToM2 Eyes"),]
EverythingToM2No <- EverythingToM2[EverythingToM2$ToM.Both=="no",]
EverythingToM2Yes <- EverythingToM2[EverythingToM2$ToM.Both=="Yes",]
EverythingToM1Ball <- EverythingToM1[EverythingToM1$AOI.Name=="ToM 1 Ball",]
EverythingToM1BallNo <- EverythingToM1Ball[EverythingToM1Ball$ToM.Both=="no",]
EverythingToM1BallYes <- EverythingToM1Ball[EverythingToM1Ball$ToM.Both=="Yes",]
EverythingToM1Eyes <- EverythingToM1[EverythingToM1$AOI.Name=="ToM1 Eyes",]
EverythingToM1EyesNo <- EverythingToM1Eyes[EverythingToM1Eyes$ToM.Both=="no",]
EverythingToM1EyesYes <- EverythingToM1Eyes[EverythingToM1Eyes$ToM.Both=="Yes",]

EverythingToM2Ball <- EverythingToM2[EverythingToM2$AOI.Name=="ToM2 Ball",]
EverythingToM2BallNo <- EverythingToM2Ball[EverythingToM2Ball$ToM.Both=="no",]
EverythingToM2BallYes <- EverythingToM2Ball[EverythingToM2Ball$ToM.Both=="Yes",]
EverythingToM2Eyes <- EverythingToM2[EverythingToM2$AOI.Name=="ToM2 Eyes",]
EverythingToM2EyesNo <- EverythingToM2Eyes[EverythingToM2Eyes$ToM.Both=="no",]
EverythingToM2EyesYes <- EverythingToM2Eyes[EverythingToM2Eyes$ToM.Both=="Yes",]


plot(EverythingClean$Hit.time.G..ms., EverythingClean$Time.spent.G..ms., xlab = "Hit time", ylab = "Time Spent", main = "All", xlim=c(100000, 180000), ylim=c(0, 5000))
abline(lm(EverythingClean$Time.spent.G..ms.~EverythingClean$Hit.time.G..ms.), col="red")
plot(EverythingNo$Hit.time.G..ms., EverythingNo$Time.spent.G..ms., xlab = "Hit time", ylab = "Time Spent", main = "No", xlim=c(100000, 180000), ylim=c(0, 5000))
plot(EverythingYes$Hit.time.G..ms., EverythingYes$Time.spent.G..ms., xlab = "Hit time", ylab = "Time Spent", main = "Yes", xlim=c(100000, 180000), ylim=c(0, 5000))

plot(EverythingBall$Hit.time.G..ms., EverythingBall$Time.spent.G..ms., xlab = "Hit time", ylab = "Time Spent", main = "All Ball", xlim=c(100000, 180000), ylim=c(0, 5000))
abline(lm(EverythingBall$Time.spent.G..ms.~EverythingBall$Hit.time.G..ms.), col="red")
plot(EverythingNoBall$Hit.time.G..ms., EverythingNoBall$Time.spent.G..ms., xlab = "Hit time", ylab = "Time Spent", main = "No Ball", xlim=c(100000, 180000), ylim=c(0, 5000))
abline(lm(EverythingNoBall$Time.spent.G..ms.~EverythingNoBall$Hit.time.G..ms.), col="red")
plot(EverythingYesBall$Hit.time.G..ms., EverythingYesBall$Time.spent.G..ms., xlab = "Hit time", ylab = "Time Spent", main = "Yes Ball", xlim=c(100000, 180000), ylim=c(0, 5000))
abline(lm(EverythingYesBall$Time.spent.G..ms.~EverythingYesBall$Hit.time.G..ms.), col="red")

plot(EverythingEyes$Hit.time.G..ms., EverythingEyes$Time.spent.G..ms., xlab = "Hit time", ylab = "Time Spent", main = "All Eyes", xlim=c(100000, 180000), ylim=c(0, 5000))
abline(lm(EverythingEyes$Time.spent.G..ms.~EverythingEyes$Hit.time.G..ms.), col="red")

plot(EverythingNoEyes$Hit.time.G..ms., EverythingNoEyes$Time.spent.G..ms., xlab = "Hit time", ylab = "Time Spent", main = "No Eyes", xlim=c(100000, 180000), ylim=c(0, 5000))
abline(lm(EverythingNoEyes$Time.spent.G..ms.~EverythingNoEyes$Hit.time.G..ms.), col="red")

plot(EverythingYesEyes$Hit.time.G..ms., EverythingYesEyes$Time.spent.G..ms., xlab = "Hit time", ylab = "Time Spent", main = "Yes Eyes", xlim=c(100000, 180000), ylim=c(0, 5000))

plot(EverythingToM1$Hit.time.G..ms., EverythingToM1$Time.spent.G..ms., xlab = "Hit time", ylab = "Time Spent", main = "All ToM1", xlim=c(100000, 180000), ylim=c(0, 5000))
abline(lm(EverythingToM1$Time.spent.G..ms.~EverythingToM1$Hit.time.G..ms.), col="red")


plot(EverythingToM1No$Hit.time.G..ms., EverythingToM1No$Time.spent.G..ms., xlab = "Hit time", ylab = "Time Spent", main = "ToM1 No", xlim=c(100000, 180000), ylim=c(0, 5000))
plot(EverythingToM1Yes$Hit.time.G..ms., EverythingToM1Yes$Time.spent.G..ms., xlab = "Hit time", ylab = "Time Spent", main = "ToM1 Yes", xlim=c(100000, 180000), ylim=c(0, 5000))

plot(EverythingToM1Eyes$Hit.time.G..ms., EverythingToM1Eyes$Time.spent.G..ms., xlab = "Hit time", ylab = "Time Spent", main = "ToM1 Eyes", xlim=c(100000, 180000), ylim=c(0, 5000))
abline(lm(EverythingToM1Eyes$Time.spent.G..ms.~EverythingToM1Eyes$Hit.time.G..ms.), col="red")
plot(EverythingToM2Eyes$Hit.time.G..ms., EverythingToM2Eyes$Time.spent.G..ms., xlab = "Hit time", ylab = "Time Spent", main = "ToM2 Eyes", xlim=c(100000, 180000), ylim=c(0, 5000))
abline(lm(EverythingToM2Eyes$Time.spent.G..ms.~EverythingToM2Eyes$Hit.time.G..ms.), col="red")

plot(EverythingToM1EyesNo$Hit.time.G..ms., EverythingToM1EyesNo$Time.spent.G..ms., xlab = "Hit time", ylab = "Time Spent", main = "ToM1 Eyes No", xlim=c(100000, 180000), ylim=c(0, 5000))
plot(EverythingToM1EyesYes$Hit.time.G..ms., EverythingToM1EyesYes$Time.spent.G..ms., xlab = "Hit time", ylab = "Time Spent", main = "ToM1 Eyes Yes", xlim=c(100000, 180000), ylim=c(0, 5000))

plot(EverythingToM1Ball$Hit.time.G..ms., EverythingToM1Ball$Time.spent.G..ms., xlab = "Hit time", ylab = "Time Spent", main = "ToM1 Ball", xlim=c(100000, 180000), ylim=c(0, 5000))
abline(lm(EverythingToM1Ball$Time.spent.G..ms.~EverythingToM1Ball$Hit.time.G..ms.), col="red")
plot(EverythingToM2Ball$Hit.time.G..ms., EverythingToM2Ball$Time.spent.G..ms., xlab = "Hit time", ylab = "Time Spent", main = "ToM2 Ball", xlim=c(100000, 180000), ylim=c(0, 5000))
abline(lm(EverythingToM2Ball$Time.spent.G..ms.~EverythingToM2Ball$Hit.time.G..ms.), col="red")

plot(EverythingToM1BallNo$Hit.time.G..ms., EverythingToM1BallNo$Time.spent.G..ms., xlab = "Hit time", ylab = "Time Spent", main = "ToM1 Ball No", xlim=c(100000, 180000), ylim=c(0, 5000))
plot(EverythingToM1BallYes$Hit.time.G..ms., EverythingToM1BallYes$Time.spent.G..ms., xlab = "Hit time", ylab = "Time Spent", main = "ToM1 Ball Yes", xlim=c(100000, 180000), ylim=c(0, 5000))

plot(EverythingToM2$Hit.time.G..ms., EverythingToM2$Time.spent.G..ms., xlab = "Hit time", ylab = "Time Spent", main = "All ToM2", xlim=c(100000, 180000), ylim=c(0, 5000))
abline(lm(EverythingToM2$Time.spent.G..ms.~EverythingToM2$Hit.time.G..ms.), col="red")

plot(EverythingToM2No$Hit.time.G..ms., EverythingToM2No$Time.spent.G..ms., xlab = "Hit time", ylab = "Time Spent", main = "ToM2 No", xlim=c(100000, 180000), ylim=c(0, 5000))
plot(EverythingToM2Yes$Hit.time.G..ms., EverythingToM2Yes$Time.spent.G..ms., xlab = "Hit time", ylab = "Time Spent", main = "ToM2 Yes", xlim=c(100000, 180000), ylim=c(0, 5000))

EverythingToM2$Hit.time.G..ms.[EverythingToM2$Hit.time.G..ms.==0] <- NA
plot(na.omit(EverythingToM2$Hit.time.G..ms.~EverythingToM2$ToM.Both))

# Make some plots showing the distribution of hit time for each AOI
# Hit Time-G (ms): Arrival time, time of first sample within the AOI (for this ID)
Hit.time = MFB1BallClean$HitTime
Hit.time[Hit.time==0] <- NA
Hit.time <- na.omit(Hit.time)
png('plots/HitTimeMFB1Ball.png')
plot(density(Hit.time), ylab = 'MFB1Ball', main = 'Hit Time-G (ms)')
dev.off()
Hit.time = MFB1EyesClean$HitTime
Hit.time[Hit.time==0] <- NA
Hit.time <- na.omit(Hit.time)
png('plots/HitTimeMFB1Eyes.png')
plot(density(Hit.time), ylab = 'MFB1Eyes', main = 'Hit Time-G (ms)')
dev.off()
Hit.time = MFB2BallClean$HitTime
Hit.time[Hit.time==0] <- NA
Hit.time <- na.omit(Hit.time)
png('plots/HitTimeMFB2Ball.png')
plot(density(Hit.time), ylab = 'MFB2Ball', main = 'Hit Time-G (ms)')
dev.off()
Hit.time = MFB2EyesClean$HitTime
Hit.time[Hit.time==0] <- NA
Hit.time <- na.omit(Hit.time)
png('plots/HitTimeMFB2Eyes.png')
plot(density(Hit.time), ylab = 'MFB2Eyes', main = 'Hit Time-G (ms)')
dev.off()

# Make some plots showing the distribution of ttff for each AOI
# TTFF_F (ms): Time to First Fixation. Timestamp of the first fixation recorded inside this AOI for this ID.
TTFF = MFB1BallClean$TTFF
TTFF[TTFF==0] <- NA
TTFF <- na.omit(TTFF)
png("plots/TTFFMFB1Ball.png")
plot(density(TTFF), ylab = 'MFB1Ball', main = 'TTFF-F (ms)')
dev.off()
TTFF = MFB1EyesClean$TTFF
TTFF[TTFF==0] <- NA
TTFF <- na.omit(TTFF)
png("plots/TTFFMFB1Eyes.png")
plot(density(TTFF), ylab = 'MFB1Eyes', main = 'TTFF-F (ms)')
dev.off()
TTFF = MFB2BallClean$TTFF
TTFF[TTFF==0] <- NA
TTFF <- na.omit(TTFF)
png("plots/TTFFMFB2Ball.png")
plot(density(TTFF), ylab = 'MFB2Ball', main = 'TTFF-F (ms)')
dev.off()
TTFF = MFB2EyesClean$TTFF
TTFF[TTFF==0] <- NA
TTFF <- na.omit(TTFF)
png("plots/TTFFMFB2Eyes.png")
plot(density(TTFF), ylab = 'MFB2Eyes', main = 'TTFF-F (ms)')
dev.off()

# Make some plots showing the distribution of Time Spent for each AOI
# Time Spent-G (ms): Time spent in the AOI based on raw gaze data points (not fixation based)
TimeSpentGms = MFB1BallClean$TimeSpentGms
TimeSpentGms[TimeSpentGms==0] <- NA
TimeSpentGms <- na.omit(TimeSpentGms)
png('plots/TimeSpentMFB1Ball.png')
plot(density(TimeSpentGms), ylab = 'MFB1Ball', main = 'Time Spent-G (ms)')
dev.off()
TimeSpentGms = MFB1EyesClean$TimeSpentGms
TimeSpentGms[TimeSpentGms==0] <- NA
TimeSpentGms <- na.omit(TimeSpentGms)
png('plots/TimeSpentMFB1Eyes.png')
plot(density(TimeSpentGms), ylab = 'MFB1Eyes', main = 'Time Spent-G (ms)')
dev.off()
TimeSpentGms = MFB2BallClean$TimeSpentGms
TimeSpentGms[TimeSpentGms==0] <- NA
TimeSpentGms <- na.omit(TimeSpentGms)
png('plots/TimeSpentMFB2Ball.png')
plot(density(TimeSpentGms), ylab = 'MFB2Ball', main = 'Time Spent-G (ms)')
dev.off()
TimeSpentGms = MFB2EyesClean$TimeSpentGms
TimeSpentGms[TimeSpentGms==0] <- NA
TimeSpentGms <- na.omit(TimeSpentGms)
png('plots/TimeSpentMFB2Eyes.png')
plot(density(TimeSpentGms), ylab = 'MFB2Eyes', main = 'Time Spent-G (ms)')
dev.off()

# Make some plots showing the distribution of Fixation Count for each AOI
# Fixation Count = Number of fixations recorded inside this AOI for this responent
Fixations = MFB1BallClean$Fixations
Fixations[Fixations==0] <- NA
Fixations <- na.omit(Fixations)
png('plots/FixesMFB1Ball.png')
plot(density(Fixations), ylab = 'MFB1Ball', main = 'Fixation Count')
dev.off()
Fixations = MFB1EyesClean$Fixations
Fixations[Fixations==0] <- NA
Fixations <- na.omit(Fixations)
png('plots/FixesMFB1Eyes.png')
plot(density(Fixations), ylab = 'MFB1Eyes', main = 'Fixation Count')
dev.off()
Fixations = MFB2BallClean$Fixations
Fixations[Fixations==0] <- NA
Fixations <- na.omit(Fixations)
png('plots/FixesMFB2Ball.png')
plot(density(Fixations), ylab = 'MFB2Ball', main = 'Fixation Count')
dev.off()
Fixations = MFB2EyesClean$Fixations
Fixations[Fixations==0] <- NA
Fixations <- na.omit(Fixations)
png('plots/FixesMFB2Eyes.png')
plot(density(Fixations), ylab = 'MFB2Eyes', main = 'Fixation Count')
dev.off()




#Replace 0's with NA's for stats purposes
Interesting$HitTime[Interesting$HitTime == 0] <- NA
Interesting$TimeSpentGms[Interesting$TimeSpentGms == 0] <- NA
# I took out TTFF and Fixations because they were strongly linked to Hit time and Time Spent,
# respectively. It seemed redundant.
#Interesting$TTFF[Interesting$TTFF == 0] <- NA
#Interesting$Fixations[Interesting$Fixations == 0] <- NA
Interesting$SRS.Raw[Interesting$SRS.Raw == 0] <- NA
Interesting$SCQ[Interesting$SCQ == 0] <- NA

#Plot correlations
png('plots/Corrs/Correlations.png')
cor.plot(Interesting)
dev.off()
png('plots/Corrs/CorrsBall.png')
cor.plot(Interesting[Interesting$AOI %in% c(1,3),], main = "All Ball")
dev.off()
png('plots/Corrs/CorrsEyes.png')
cor.plot(Interesting[Interesting$AOI %in% c(2,4),], main = "All Eyes")
dev.off()
png('plots/Corrs/CorrsMFB1Ball.png')
cor.plot(Interesting[Interesting$AOI==1,], main = "Correlations, MFB1Ball")
dev.off()
png('plots/Corrs/CorrsMFB1Eyes.png')
cor.plot(Interesting[Interesting$AOI==2,], main = "Correlations, MFB1Eyes")
dev.off()
png('plots/Corrs/CorrsMFB2Ball.png')
cor.plot(Interesting[Interesting$AOI==3,], main = "Correlations, MFB2Ball")
dev.off()
png('plots/Corrs/CorrsMFB2Eyes.png')
cor.plot(Interesting[Interesting$AOI==4,], main = "Correlations, MFB2Eyes")
dev.off()
png('plots/Corrs/CorrsBallno.png')
cor.plot(Interesting[Interesting$AOI %in% c(1,3) & Interesting$ToM.Both == 1,], main = "Correlations, Ball")
dev.off()
png('plots/Corrs/CorrsEyesno.png')
cor.plot(Interesting[Interesting$AOI %in% c(2,4) & Interesting$ToM.Both == 1,], main = "Correlations, Eyes")
dev.off()
png('plots/Corrs/CorrsBallYes.png')
cor.plot(Interesting[Interesting$AOI %in% c(1,3) & Interesting$ToM.Both == 2,], main = "Correlations, Ball")
dev.off()
png('plots/Corrs/CorrsEyesYes.png')
cor.plot(Interesting[Interesting$AOI %in% c(2,4) & Interesting$ToM.Both == 2,], main = "Correlations, Eyes")
dev.off()

SRS <- subset(AssessmentsClean, select = c(ID, SRS.Raw, SRS.T,
                                           AWR.Raw, COG.Raw, COM.Raw, MOT.Raw, RRB.Raw,
                                           AWR.T, COG.T, COM.T.Score, MOT.T.Score, RRB.T.Score,
                                           SCQ, Ravens ))
SRS$ID <- as.numeric(SRS$ID)
SRSadj <- subset(SRS, select = c(ID, SRS.Raw, SRS.T, AWR.Raw, COG.Raw, COM.Raw, MOT.Raw,
                                 RRB.Raw, SCQ))

#PCA on Assessment Scores
pca1 = prcomp(na.omit(SRS[2:14]))
png('plots/scree1.png')
screeplot(pca1)
dev.off()
pca1 <- pca(SRS[2:14], nfactors=3)
png('plots/pca1.png')
plot(pca1)
dev.off()

pca2 = prcomp(na.omit(SRSadj[2:9]))
png('plots/scree2.png')
screeplot(pca2)
dev.off()
pca2 <- pca(SRSadj[2:9], nfactors=3)
png('plots/pca2.png')
plot(pca2)
dev.off()

#Factor analysis
fa3 <- fa.poly(SRS[2:14], nfactors = 3, n.obs=nrow(na.omit(SRS[2:14])), fm = 'pa', rotate='oblimin')
fa2 <- fa.poly(SRS[2:14], nfactors = 2, n.obs=nrow(na.omit(SRS[2:14])), fm = 'pa', rotate='oblimin')

Space <- subset(EverythingClean, select=c(ID, AOI, HitTime, TimeSpentGms, Age, Gender, SRS.Raw, SCQ, ToM.Both))

# Plots to examine distribution
png('plots/HitTimeXAOI.png')
boxplot(Space$HitTime~Space$AOI, main="Hit time by AOI")
dev.off()
png('plots/TimeSpentXAOI.png')
boxplot(Space$TimeSpentGms~Space$AOI, main="Time spent by AOI")
dev.off()
png('plots/HitTimeXToMXAOI.png')
boxplot(Space$HitTime~Space$ToM.Both*Space$AOI, main="Hit time by ToM*AOI")
dev.off()
png('plots/TimeSpentXToMXAOI.png')
boxplot(Space$TimeSpentGms~Space$ToM.Both*Space$AOI, main="Time spent by ToM*AOI")
dev.off()

png('plots/ToMX/Diagnosis.png')
plot(EverythingClean$Diagnosis, EverythingClean$ToM.Both, xlab = "Diagnosis", ylab = "ToM", main = "ToM per Diagnosis")
dev.off()
cor
png('plots/ToMX/Age.png')
boxplot(EverythingClean$Age~EverythingClean$ToM.Both, xlab = "Age", ylab = "ToM", main = "Age per ToM")
dev.off()

png('plots/ToMX/Gender.png')
plot(EverythingClean$Gender, EverythingClean$ToM.Both, xlab = "Gender", ylab = "ToM", main = "ToM per Gender")
dev.off()

png('plots/ToMX/GenderDiag.png')
plot(EverythingClean$Gender, EverythingClean$Diagnosis, xlab = "Gender", ylab = "Diagnosis", main = "Diagnosis per Gender")
dev.off()

png('plots/ToMX/HitTime.png')
boxplot(EverythingClean$HitTime~EverythingClean$ToM.Both, ylab = "Hit Time", xlab = "ToM", main = "Hit Time per ToM")
dev.off()

png('plots/ToMX/TimeSpent.png')
boxplot(EverythingClean$TimeSpentGms~EverythingClean$ToM.Both, ylab = "Time Spent", xlab = "ToM", main = "Time Spent per ToM")
dev.off()

png('plots/ToMX/SCQ.png')
boxplot(EverythingClean$SCQ~EverythingClean$ToM.Both, ylab = "SCQ", xlab = "ToM", main = "SCQ per ToM")
dev.off()

png('plots/ToMX/SRS.png')
boxplot(EverythingClean$SRS.Raw~EverythingClean$ToM.Both, ylab = "SRS", xlab = "ToM", main = "SRS per ToM")
dev.off()

png('plots/ToMX/Ravens.png')
boxplot(EverythingClean$Ravens~EverythingClean$ToM.Both, ylab = "Ravens", xlab = "ToM", main = "Ravens per ToM")
dev.off()


model <- lm(EverythingClean$Ravens~EverythingClean$Age+EverythingClean$ToM.Both)

EverythingBall <- EverythingClean[EverythingClean$AOI %in% c("MFB1Ball", "MFB2 Ball"),]
EverythingEyes <- EverythingClean[EverythingClean$AOI %in% c("MFB1Eyes", "MFB2 Eyes"),]

EverythingNo <- EverythingClean[EverythingClean$ToM.Both=="no",]
EverythingNoBall <- EverythingNo[EverythingNo$AOI %in% c("MFB1Ball", "MFB2 Ball"),]
EverythingNoEyes <- EverythingNo[EverythingNo$AOI %in% c("MFB1Eyes", "MFB2 Eyes"),]
EverythingYes <- EverythingClean[EverythingClean$ToM.Both=="Yes",]
EverythingYesBall <- EverythingYes[EverythingYes$AOI %in% c("MFB1Ball", "MFB2 Ball"),]
EverythingYesEyes <- EverythingYes[EverythingYes$AOI %in% c("MFB1Eyes", "MFB2 Eyes"),]
EverythingMFB1 <- EverythingClean[EverythingClean$AOI %in% c("MFB1Ball","MFB1Eyes"),]
EverythingMFB1No <- EverythingMFB1[EverythingMFB1$ToM.Both=="no",]
EverythingMFB1Yes <- EverythingMFB1[EverythingMFB1$ToM.Both=="Yes",]
EverythingMFB2 <- EverythingClean[EverythingClean$AOI %in% c("MFB2 Ball", "MFB2 Eyes"),]
EverythingMFB2No <- EverythingMFB2[EverythingMFB2$ToM.Both=="no",]
EverythingMFB2Yes <- EverythingMFB2[EverythingMFB2$ToM.Both=="Yes",]
EverythingMFB1Ball <- EverythingMFB1[EverythingMFB1$AOI=="MFB1Ball",]
EverythingMFB1BallNo <- EverythingMFB1Ball[EverythingMFB1Ball$ToM.Both=="no",]
EverythingMFB1BallYes <- EverythingMFB1Ball[EverythingMFB1Ball$ToM.Both=="Yes",]
EverythingMFB1Eyes <- EverythingMFB1[EverythingMFB1$AOI=="MFB1Eyes",]
EverythingMFB1EyesNo <- EverythingMFB1Eyes[EverythingMFB1Eyes$ToM.Both=="no",]
EverythingMFB1EyesYes <- EverythingMFB1Eyes[EverythingMFB1Eyes$ToM.Both=="Yes",]

EverythingMFB2Ball <- EverythingMFB2[EverythingMFB2$AOI=="MFB2 Ball",]
EverythingMFB2BallNo <- EverythingMFB2Ball[EverythingMFB2Ball$ToM.Both=="no",]
EverythingMFB2BallYes <- EverythingMFB2Ball[EverythingMFB2Ball$ToM.Both=="Yes",]
EverythingMFB2Eyes <- EverythingMFB2[EverythingMFB2$AOI=="MFB2 Eyes",]
EverythingMFB2EyesNo <- EverythingMFB2Eyes[EverythingMFB2Eyes$ToM.Both=="no",]
EverythingMFB2EyesYes <- EverythingMFB2Eyes[EverythingMFB2Eyes$ToM.Both=="Yes",]


plot(EverythingClean$HitTime, EverythingClean$TimeSpentGms, xlab = "Hit time", ylab = "Time Spent", main = "All", xlim=c(100000, 180000), ylim=c(0, 5000))
abline(lm(EverythingClean$TimeSpentGms~EverythingClean$HitTime), col="red")
plot(EverythingNo$HitTime, EverythingNo$TimeSpentGms, xlab = "Hit time", ylab = "Time Spent", main = "No", xlim=c(100000, 180000), ylim=c(0, 5000))
plot(EverythingYes$HitTime, EverythingYes$TimeSpentGms, xlab = "Hit time", ylab = "Time Spent", main = "Yes", xlim=c(100000, 180000), ylim=c(0, 5000))

plot(EverythingBall$HitTime, EverythingBall$TimeSpentGms, xlab = "Hit time", ylab = "Time Spent", main = "All Ball", xlim=c(100000, 180000), ylim=c(0, 5000))
abline(lm(EverythingBall$TimeSpentGms~EverythingBall$HitTime), col="red")
plot(EverythingNoBall$HitTime, EverythingNoBall$TimeSpentGms, xlab = "Hit time", ylab = "Time Spent", main = "No Ball", xlim=c(100000, 180000), ylim=c(0, 5000))
abline(lm(EverythingNoBall$TimeSpentGms~EverythingNoBall$HitTime), col="red")
plot(EverythingYesBall$HitTime, EverythingYesBall$TimeSpentGms, xlab = "Hit time", ylab = "Time Spent", main = "Yes Ball", xlim=c(100000, 180000), ylim=c(0, 5000))
abline(lm(EverythingYesBall$TimeSpentGms~EverythingYesBall$HitTime), col="red")

plot(EverythingEyes$HitTime, EverythingEyes$TimeSpentGms, xlab = "Hit time", ylab = "Time Spent", main = "All Eyes", xlim=c(100000, 180000), ylim=c(0, 5000))
abline(lm(EverythingEyes$TimeSpentGms~EverythingEyes$HitTime), col="red")

plot(EverythingNoEyes$HitTime, EverythingNoEyes$TimeSpentGms, xlab = "Hit time", ylab = "Time Spent", main = "No Eyes", xlim=c(100000, 180000), ylim=c(0, 5000))
abline(lm(EverythingNoEyes$TimeSpentGms~EverythingNoEyes$HitTime), col="red")

plot(EverythingYesEyes$HitTime, EverythingYesEyes$TimeSpentGms, xlab = "Hit time", ylab = "Time Spent", main = "Yes Eyes", xlim=c(100000, 180000), ylim=c(0, 5000))

plot(EverythingMFB1$HitTime, EverythingMFB1$TimeSpentGms, xlab = "Hit time", ylab = "Time Spent", main = "All MFB1", xlim=c(100000, 180000), ylim=c(0, 5000))
abline(lm(EverythingMFB1$TimeSpentGms~EverythingMFB1$HitTime), col="red")


plot(EverythingMFB1No$HitTime, EverythingMFB1No$TimeSpentGms, xlab = "Hit time", ylab = "Time Spent", main = "MFB1 No", xlim=c(100000, 180000), ylim=c(0, 5000))
plot(EverythingMFB1Yes$HitTime, EverythingMFB1Yes$TimeSpentGms, xlab = "Hit time", ylab = "Time Spent", main = "MFB1 Yes", xlim=c(100000, 180000), ylim=c(0, 5000))

plot(EverythingMFB1Eyes$HitTime, EverythingMFB1Eyes$TimeSpentGms, xlab = "Hit time", ylab = "Time Spent", main = "MFB1Eyes", xlim=c(100000, 180000), ylim=c(0, 5000))
abline(lm(EverythingMFB1Eyes$TimeSpentGms~EverythingMFB1Eyes$HitTime), col="red")
plot(EverythingMFB2Eyes$HitTime, EverythingMFB2Eyes$TimeSpentGms, xlab = "Hit time", ylab = "Time Spent", main = "MFB2 Eyes", xlim=c(100000, 180000), ylim=c(0, 5000))
abline(lm(EverythingMFB2Eyes$TimeSpentGms~EverythingMFB2Eyes$HitTime), col="red")

plot(EverythingMFB1EyesNo$HitTime, EverythingMFB1EyesNo$TimeSpentGms, xlab = "Hit time", ylab = "Time Spent", main = "MFB1Eyes No", xlim=c(100000, 180000), ylim=c(0, 5000))
plot(EverythingMFB1EyesYes$HitTime, EverythingMFB1EyesYes$TimeSpentGms, xlab = "Hit time", ylab = "Time Spent", main = "MFB1Eyes Yes", xlim=c(100000, 180000), ylim=c(0, 5000))

plot(EverythingMFB1Ball$HitTime, EverythingMFB1Ball$TimeSpentGms, xlab = "Hit time", ylab = "Time Spent", main = "MFB1 Ball", xlim=c(100000, 180000), ylim=c(0, 5000))
abline(lm(EverythingMFB1Ball$TimeSpentGms~EverythingMFB1Ball$HitTime), col="red")
plot(EverythingMFB2Ball$HitTime, EverythingMFB2Ball$TimeSpentGms, xlab = "Hit time", ylab = "Time Spent", main = "MFB2 Ball", xlim=c(100000, 180000), ylim=c(0, 5000))
abline(lm(EverythingMFB2Ball$TimeSpentGms~EverythingMFB2Ball$HitTime), col="red")

plot(EverythingMFB1BallNo$HitTime, EverythingMFB1BallNo$TimeSpentGms, xlab = "Hit time", ylab = "Time Spent", main = "MFB1 Ball No", xlim=c(100000, 180000), ylim=c(0, 5000))
plot(EverythingMFB1BallYes$HitTime, EverythingMFB1BallYes$TimeSpentGms, xlab = "Hit time", ylab = "Time Spent", main = "MFB1 Ball Yes", xlim=c(100000, 180000), ylim=c(0, 5000))

plot(EverythingMFB2$HitTime, EverythingMFB2$TimeSpentGms, xlab = "Hit time", ylab = "Time Spent", main = "All MFB2", xlim=c(100000, 180000), ylim=c(0, 5000))
abline(lm(EverythingMFB2$TimeSpentGms~EverythingMFB2$HitTime), col="red")

plot(EverythingMFB2No$HitTime, EverythingMFB2No$TimeSpentGms, xlab = "Hit time", ylab = "Time Spent", main = "MFB2 No", xlim=c(100000, 180000), ylim=c(0, 5000))
plot(EverythingMFB2Yes$HitTime, EverythingMFB2Yes$TimeSpentGms, xlab = "Hit time", ylab = "Time Spent", main = "MFB2 Yes", xlim=c(100000, 180000), ylim=c(0, 5000))

EverythingMFB2$HitTime[EverythingMFB2$HitTime==0] <- NA
plot(na.omit(EverythingMFB2$HitTime~EverythingMFB2$ToM.Both))

