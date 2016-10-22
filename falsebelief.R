## Script No. 1 ##
# Gets all the data in the right place

library(psych)

# Read CSV into R
allEg <- read.csv(file="Data/allEg.csv", header=TRUE, sep=",", stringsAsFactors = TRUE)
MFB2Ball <- read.csv(file="Data/Ball2egxToM.csv", header=TRUE, sep=",", stringsAsFactors = TRUE)
MFB2Eyes <- read.csv(file="Data/Eyes2EgXToM.csv", header=TRUE, sep=",", stringsAsFactors = TRUE)
Sheet1 <- read.csv(file="Data/Sheet1.csv", header=TRUE, sep=",", stringsAsFactors = TRUE)
MFBEyeGaze <- read.csv(file="Data/ToMEyeGaze.csv", header=TRUE, sep=",", stringsAsFactors = TRUE)
MFB1Ball <- read.csv(file="Data/ToMxBallEG1.csv", header=TRUE, sep=",", stringsAsFactors = TRUE)
MFB1Eyes <- read.csv(file="Data/TOMxEyeEG.csv", header=TRUE, sep=",", stringsAsFactors = TRUE)
Ravens <- read.csv(file="Data/RavensScores.csv", header=TRUE, sep=",", stringsAsFactors = TRUE)
# Fix ID's
Sheet1$Participant <- as.factor(gsub('-', '', Sheet1$Participant))

#Change Respondent to ID
names(allEg)[names(allEg) == "Respondent"] <- "ID"
names(MFB1Ball)[names(MFB1Ball) == "Respondent"] <- "ID"
names(MFB2Ball)[names(MFB2Ball) == "Respondent"] <- "ID"
names(MFB1Eyes)[names(MFB1Eyes) == "Respondent"] <- "ID"
names(MFB2Eyes)[names(MFB2Eyes) == "Respondent"] <- "ID"
names(MFBEyeGaze)[names(MFBEyeGaze) == "Respondent"] <- "ID"
names(Sheet1)[names(Sheet1) == "Participant"] <- "ID"

#Change AOI to AOI
names(allEg)[names(allEg) == "AOI.Name"] <- "AOI"
names(MFB1Ball)[names(MFB1Ball) == "AOI.Name"] <- "AOI"
names(MFB2Ball)[names(MFB2Ball) == "AOI.Name"] <- "AOI"
names(MFB1Eyes)[names(MFB1Eyes) == "AOI.Name"] <- "AOI"
names(MFB2Eyes)[names(MFB2Eyes) == "AOI.Name"] <- "AOI"
names(MFBEyeGaze)[names(MFBEyeGaze) == "AOI.Name"] <- "AOI"
names(Sheet1)[names(Sheet1) == "AOI.Name"] <- "AOI"

# Change eye tracking names
names(allEg)[names(allEg) == "Hit.time.G..ms."] <- "HitTime"
names(allEg)[names(allEg) == "Time.spent.G..ms."] <- "TimeSpentGms"
names(allEg)[names(allEg) == "Time.spent.G...."] <- "TimeSpentGpc"
names(allEg)[names(allEg) == "TTFF.F..ms."] <- "TTFF"
names(allEg)[names(allEg) == "Time.spent.F..ms."] <- "TimeSpentFms"
names(allEg)[names(allEg) == "Time.spent.F...."] <- "TimeSpentFpc"
names(allEg)[names(allEg) == "Fixations.Count"] <- "Fixations"

names(MFB1Ball)[names(MFB1Ball) == "Hit.time.G..ms."] <- "HitTime"
names(MFB1Ball)[names(MFB1Ball) == "Time.spent.G..ms."] <- "TimeSpentGms"
names(MFB1Ball)[names(MFB1Ball) == "Time.spent.G...."] <- "TimeSpentGpc"
names(MFB1Ball)[names(MFB1Ball) == "TTFF.F..ms."] <- "TTFF"
names(MFB1Ball)[names(MFB1Ball) == "Time.spent.F..ms."] <- "TimeSpentFms"
names(MFB1Ball)[names(MFB1Ball) == "Time.spent.F...."] <- "TimeSpentFpc"
names(MFB1Ball)[names(MFB1Ball) == "Fixations.Count"] <- "Fixations"

names(MFB2Ball)[names(MFB2Ball) == "Hit.time.G..ms."] <- "HitTime"
names(MFB2Ball)[names(MFB2Ball) == "Time.spent.G..ms."] <- "TimeSpentGms"
names(MFB2Ball)[names(MFB2Ball) == "Time.spent.G...."] <- "TimeSpentGpc"
names(MFB2Ball)[names(MFB2Ball) == "TTFF.F..ms."] <- "TTFF"
names(MFB2Ball)[names(MFB2Ball) == "Time.spent.F..ms."] <- "TimeSpentFms"
names(MFB2Ball)[names(MFB2Ball) == "Time.spent.F...."] <- "TimeSpentFpc"
names(MFB2Ball)[names(MFB2Ball) == "Fixations.Count"] <- "Fixations"

names(MFB1Eyes)[names(MFB1Eyes) == "Hit.time.G..ms."] <- "HitTime"
names(MFB1Eyes)[names(MFB1Eyes) == "Time.spent.G..ms."] <- "TimeSpentGms"
names(MFB1Eyes)[names(MFB1Eyes) == "Time.spent.G...."] <- "TimeSpentGpc"
names(MFB1Eyes)[names(MFB1Eyes) == "TTFF.F..ms."] <- "TTFF"
names(MFB1Eyes)[names(MFB1Eyes) == "Time.spent.F..ms."] <- "TimeSpentFms"
names(MFB1Eyes)[names(MFB1Eyes) == "Time.spent.F...."] <- "TimeSpentFpc"
names(MFB1Eyes)[names(MFB1Eyes) == "Fixations.Count"] <- "Fixations"

names(MFB2Eyes)[names(MFB2Eyes) == "Hit.time.G..ms."] <- "HitTime"
names(MFB2Eyes)[names(MFB2Eyes) == "Time.spent.G..ms."] <- "TimeSpentGms"
names(MFB2Eyes)[names(MFB2Eyes) == "Time.spent.G...."] <- "TimeSpentGpc"
names(MFB2Eyes)[names(MFB2Eyes) == "TTFF.F..ms."] <- "TTFF"
names(MFB2Eyes)[names(MFB2Eyes) == "Time.spent.F..ms."] <- "TimeSpentFms"
names(MFB2Eyes)[names(MFB2Eyes) == "Time.spent.F...."] <- "TimeSpentFpc"
names(MFB2Eyes)[names(MFB2Eyes) == "Fixations.Count"] <- "Fixations"

names(MFBEyeGaze)[names(MFBEyeGaze) == "Hit.time.G..ms."] <- "HitTime"
names(MFBEyeGaze)[names(MFBEyeGaze) == "Time.spent.G..ms."] <- "TimeSpentGms"
names(MFBEyeGaze)[names(MFBEyeGaze) == "Time.spent.G...."] <- "TimeSpentGpc"
names(MFBEyeGaze)[names(MFBEyeGaze) == "TTFF.F..ms."] <- "TTFF"
names(MFBEyeGaze)[names(MFBEyeGaze) == "Time.spent.F..ms."] <- "TimeSpentFms"
names(MFBEyeGaze)[names(MFBEyeGaze) == "Time.spent.F...."] <- "TimeSpentFpc"
names(MFBEyeGaze)[names(MFBEyeGaze) == "Fixations.Count"] <- "Fixations"

#Change level names
levels(allEg$AOI) <- c("MFB1Ball", "MFB1Eyes", "MFB2Ball", "MFB2Eyes")
levels(MFB1Ball$AOI) <- c("MFB1Ball", "MFB1Eyes", "MFB2Ball", "MFB2Eyes")
levels(MFB1Eyes$AOI) <- c("MFB1Ball", "MFB1Eyes", "MFB2Ball", "MFB2Eyes")
levels(MFB2Ball$AOI) <- c("MFB1Ball", "MFB1Eyes", "MFB2Ball", "MFB2Eyes")
levels(MFB2Eyes$AOI) <- c("MFB1Ball", "MFB1Eyes", "MFB2Ball", "MFB2Eyes")
levels(MFBEyeGaze$AOI) <- c("MFB1Ball", "MFB1Eyes", "MFB2Ball", "MFB2Eyes")

#Fix cognitive assessments data
Assessments <- Sheet1[-which(!(Sheet1$ID %in% MFB1Ball$ID)),]
Assessments$ID <- factor(Assessments$ID)

#Use Assessments for just standard Ravens score, but use TestScores if
# you need Ravens broken into subtests
TestScores <- na.omit(cbind(Assessments[match(Ravens$ID, Assessments$ID),], Ravens))
names(Assessments)[names(Assessments) == "Total.Raw.Score"] <- "SRS.Raw"
names(Assessments)[names(Assessments) == "T.Score"] <- "SRS.T"
names(Assessments)[names(Assessments) == "AWR.T.Score"] <- "AWR.T"
names(Assessments)[names(Assessments) == "COG.T.Score"] <- "COG.T"
names(Assessments)[names(Assessments) == "COM.T.Score"] <- "COM.T"
names(Assessments)[names(Assessments) == "MOT.T.Score"] <- "MOT.T"
names(Assessments)[names(Assessments) == "RRB.T.Score"] <- "RRB.T"
names(Assessments)[names(Assessments) == "Ravens.Total"] <- "Ravens"
names(TestScores)[names(TestScores) == "Total.Raw.Score"] <- "SRS.Raw"
names(TestScores)[names(TestScores) == "T.Score"] <- "SRS.T"
names(TestScores)[names(TestScores) == "AWR.T.Score"] <- "AWR.T"
names(TestScores)[names(TestScores) == "COG.T.Score"] <- "COG.T"
names(TestScores)[names(TestScores) == "COM.T.Score"] <- "COM.T"
names(TestScores)[names(TestScores) == "MOT.T.Score"] <- "MOT.T"
names(TestScores)[names(TestScores) == "RRB.T.Score"] <- "RRB.T"
names(TestScores)[names(TestScores) == "Ravens.Total"] <- "Ravens"

#Remove unnecessary variables
allEg$Respondent.ratio.G <- NULL
allEg$Revisit.G..Visitors. <- NULL
allEg$Revisit.G..Revisits. <- NULL
allEg$Revisit.F..Visitors. <- NULL
allEg$Revisit.F..Revisits. <- NULL
allEg$Revisit.G..Revisitors. <- NULL
allEg$Revisit.F..Revisitors. <- NULL

MFB1Ball$Respondent.ratio.G <- NULL
MFB1Ball$Revisit.G..Visitors. <- NULL
MFB1Ball$Revisit.G..Revisits. <- NULL
MFB1Ball$Revisit.F..Visitors. <- NULL
MFB1Ball$Revisit.F..Revisits. <- NULL
MFB1Ball$Revisit.G..Revisitors. <- NULL
MFB1Ball$Revisit.F..Revisitors. <- NULL
MFB1Ball$T1.B..ToM. <- NULL
MFB1Ball$T2.Y...ToM <- NULL

MFB2Ball$Respondent.ratio.G <- NULL
MFB2Ball$Revisit.G..Visitors. <- NULL
MFB2Ball$Revisit.G..Revisits. <- NULL
MFB2Ball$Revisit.F..Visitors. <- NULL
MFB2Ball$Revisit.F..Revisits. <- NULL
MFB2Ball$Revisit.G..Revisitors. <- NULL
MFB2Ball$Revisit.F..Revisitors. <- NULL
MFB2Ball$T1.B..ToM. <- NULL
MFB2Ball$T2.Y...ToM <- NULL

MFB1Eyes$Respondent.ratio.G <- NULL
MFB1Eyes$Revisit.G..Visitors. <- NULL
MFB1Eyes$Revisit.G..Revisits. <- NULL
MFB1Eyes$Revisit.F..Visitors. <- NULL
MFB1Eyes$Revisit.F..Revisits. <- NULL
MFB1Eyes$Revisit.G..Revisitors. <- NULL
MFB1Eyes$Revisit.F..Revisitors. <- NULL
MFB1Eyes$T1.B..ToM. <- NULL
MFB1Eyes$T2.Y...ToM <- NULL

MFB2Eyes$Respondent.ratio.G <- NULL
MFB2Eyes$Revisit.G..Visitors. <- NULL
MFB2Eyes$Revisit.G..Revisits. <- NULL
MFB2Eyes$Revisit.F..Visitors. <- NULL
MFB2Eyes$Revisit.F..Revisits. <- NULL
MFB2Eyes$Revisit.G..Revisitors. <- NULL
MFB2Eyes$Revisit.F..Revisitors. <- NULL
MFB2Eyes$T1.B..ToM. <- NULL
MFB2Eyes$T2.Y...ToM <- NULL

MFBEyeGaze$Respondent.ratio.G <- NULL
MFBEyeGaze$Revisit.G..Visitors. <- NULL
MFBEyeGaze$Revisit.G..Revisits. <- NULL
MFBEyeGaze$Revisit.F..Visitors. <- NULL
MFBEyeGaze$Revisit.F..Revisits. <- NULL
MFBEyeGaze$Revisit.G..Revisitors. <- NULL
MFBEyeGaze$Revisit.F..Revisitors. <- NULL
MFBEyeGaze$T1.B..ToM. <- NULL
MFBEyeGaze$T2.Y...ToM <- NULL

Sheet1$Respondent.ratio.G <- NULL
Sheet1$Revisit.G..Visitors. <- NULL
Sheet1$Revisit.G..Revisits. <- NULL
Sheet1$Revisit.F..Visitors. <- NULL
Sheet1$Revisit.F..Revisits. <- NULL
Sheet1$Revisit.G..Revisitors. <- NULL
Sheet1$Revisit.F..Revisitors. <- NULL



#Fix eye gaze data
allEg <- allEg[-(which(allEg$ID == "974714b")),]
allEg <- allEg[-(which(allEg$ID == "974705")),]
allEg <- allEg[-(which(allEg$ID == "974718")),]
allEg <- unique(allEg)
EyeGaze <- allEg[-(which(allEg$ID == "974706" & allEg$Gender == "Female")),]
EyeGaze$ID <- factor(EyeGaze$ID)
EyeGaze <- EyeGaze[order(EyeGaze$ID),]
MFBEyeGaze <- MFBEyeGaze[order(MFBEyeGaze$ID),]
MFBEyeGaze <- cbind(MFBEyeGaze, EyeGaze$Gender)
MFBEyeGaze <- MFBEyeGaze[order(MFBEyeGaze$AOI),]


MFB2Eyes <- MFBEyeGaze[which(MFBEyeGaze$AOI == "MFB2Eyes"),]
#MFB2Eyes <- cbind(MFB2Eyes[match(MFB1Eyes$ID, MFB2Eyes$ID),], MFB1Eyes$Gender)

assessx4 <- rbind(Assessments, Assessments, Assessments, Assessments)
assessx4 <- assessx4[order(assessx4$ID),]
Everything <- cbind(MFBEyeGaze[order(MFBEyeGaze$ID),], assessx4)
EverythingClean <- Everything[-(which(Everything$ID == "974726")),]
EverythingClean <- EverythingClean[-(which(EverythingClean$ID == "974729")),]
names(EverythingClean)[names(EverythingClean) == "EyeGaze$Gender"] <- "Gender"
EverythingClean <- EverythingClean[order(EverythingClean$AOI),]

MFB1Ball <- cbind(MFB1Ball[match(MFB1Ball$ID, Assessments$ID),], Assessments)
MFB1Eyes <- cbind(MFB1Eyes[match(MFB1Eyes$ID, Assessments$ID),], Assessments)
MFB2Ball <- cbind(MFB2Ball[match(MFB2Ball$ID, Assessments$ID),], Assessments)
MFB2Eyes <- cbind(MFB2Eyes[match(MFB2Eyes$ID, Assessments$ID),], Assessments)

MFB1BallClean <- MFB1Ball[-(which(MFB1Ball$ID == "974726")),]
MFB1BallClean <- MFB1BallClean[-(which(MFB1Ball$ID == "974729")),]
MFB1EyesClean <- MFB1Eyes[-(which(MFB1Eyes$ID == "974726")),]
MFB1EyesClean <- MFB1EyesClean[-(which(MFB1Eyes$ID == "974729")),]
MFB2BallClean <- MFB2Ball[-(which(MFB2Ball$ID == "974726")),]
MFB2BallClean <- MFB2BallClean[-(which(MFB2Ball$ID == "974729")),]
MFB2EyesClean <- MFB2Eyes[-(which(MFB2Eyes$ID == "974726")),]
MFB2EyesClean <- MFB2EyesClean[-(which(MFB2Eyes$ID == "974729")),]

AssessmentsClean <- Assessments[-(which(Assessments$ID == "974726")),]
AssessmentsClean <- AssessmentsClean[-(which(Assessments$ID == "974729")),]

#Combine Eyes and Balls,
#MFB1 and MFB2
#AllEyes = cbind(MFB1EyesClean[match(MFB1EyesClean$ID, MFB2EyesClean$ID),], MFB2EyesClean)
#AllBall = cbind(MFB1BallClean[match(MFB1BallClean$ID, MFB2BallClean$ID),], MFB2BallClean)
#AllMFB1 = cbind(MFB1EyesClean[match(MFB1EyesClean$ID, MFB1BallClean$ID),], MFB1BallClean)
#AllMFB2 = cbind(MFB2EyesClean[match(MFB2EyesClean$ID, MFB2BallClean$ID),], MFB2BallClean)

#Don't litter
remove(Sheet1)
remove(allEg)
remove(EyeGaze)
remove(assessx4)
remove(Assessments)
remove(Everything)
remove(MFB1Eyes)
remove(MFB2Eyes)
remove(MFB1Ball)
remove(MFB2Ball)

# Assume every score of 0 indicates missing data or a fluke
EverythingClean$HitTime[EverythingClean$HitTime == 0] <- NA
EverythingClean$TimeSpentGms[EverythingClean$TimeSpentGms == 0] <- NA
EverythingClean$SRS.Raw[EverythingClean$SRS.Raw == 0] <- NA
EverythingClean$SCQ[EverythingClean$SCQ==0] <- NA

MFB1EyesClean$HitTime[MFB1EyesClean$HitTime == 0] <- NA
MFB1EyesClean$TimeSpentGms[MFB1EyesClean$TimeSpentGms == 0] <- NA
MFB1EyesClean$SRS.Raw[MFB1EyesClean$SRS.Raw == 0] <- NA
MFB1EyesClean$SCQ[MFB1EyesClean$SCQ==0] <- NA

MFB2EyesClean$HitTime[MFB2EyesClean$HitTime == 0] <- NA
MFB2EyesClean$TimeSpentGms[MFB2EyesClean$TimeSpentGms == 0] <- NA
MFB2EyesClean$SRS.Raw[MFB2EyesClean$SRS.Raw == 0] <- NA
MFB2EyesClean$SCQ[MFB2EyesClean$SCQ==0] <- NA

MFB1BallClean$HitTime[MFB1BallClean$HitTime == 0] <- NA
MFB1BallClean$TimeSpentGms[MFB1BallClean$TimeSpentGms == 0] <- NA
MFB1BallClean$SRS.Raw[MFB1BallClean$SRS.Raw == 0] <- NA
MFB1BallClean$SCQ[MFB1BallClean$SCQ==0] <- NA

MFB2BallClean$HitTime[MFB2BallClean$HitTime == 0] <- NA
MFB2BallClean$TimeSpentGms[MFB2BallClean$TimeSpentGms == 0] <- NA
MFB2BallClean$SRS.Raw[MFB2BallClean$SRS.Raw == 0] <- NA
MFB2BallClean$SCQ[MFB2BallClean$SCQ==0] <- NA

# Rename ToM to MFB
names(EverythingClean)[names(EverythingClean) == "TOM.1"] <- "MFB.1"
names(EverythingClean)[names(EverythingClean) == "TOM.2"] <- "MFB.2"
names(EverythingClean)[names(EverythingClean) == "ToM.Both"] <- "MFB.Both"

names(MFB1EyesClean)[names(MFB1EyesClean) == "TOM.1"] <- "MFB.1"
names(MFB1EyesClean)[names(MFB1EyesClean) == "TOM.2"] <- "MFB.2"
names(MFB1EyesClean)[names(MFB1EyesClean) == "ToM.Both"] <- "MFB.Both"

names(MFB2EyesClean)[names(MFB2EyesClean) == "TOM.1"] <- "MFB.1"
names(MFB2EyesClean)[names(MFB2EyesClean) == "TOM.2"] <- "MFB.2"
names(MFB2EyesClean)[names(MFB2EyesClean) == "ToM.Both"] <- "MFB.Both"

names(MFB1BallClean)[names(MFB1BallClean) == "TOM.1"] <- "MFB.1"
names(MFB1BallClean)[names(MFB1BallClean) == "TOM.2"] <- "MFB.2"
names(MFB1BallClean)[names(MFB1BallClean) == "ToM.Both"] <- "MFB.Both"

names(MFB2BallClean)[names(MFB2BallClean) == "TOM.1"] <- "MFB.1"
names(MFB2BallClean)[names(MFB2BallClean) == "TOM.2"] <- "MFB.2"
names(MFB2BallClean)[names(MFB2BallClean) == "ToM.Both"] <- "MFB.Both"

# temporarily delete some of the df's we probably won't need:
remove(AssessmentsClean)
remove(MFBEyeGaze)
remove(Ravens)
remove(TestScores)
