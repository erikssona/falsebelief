#ratio <- EverythingClean$HitTime/EverythingClean$TimeSpentGpc

EverythingClean <- cbind(EverythingClean, Ravens.Age)
EFAdata <- subset(EverythingClean, select = c(Ravens.Age,
                                              Diagnosis,
                                              TimeSpentGpc, SRS.T, SCQ, ToM.Both))
#EFAdata <- cbind(EFAdata, ratio)
#EFAdata$Age <- as.numeric(EFAdata$Age)
#EFAdata$Gender <- as.numeric(EFAdata$Gender)
EFAdata$Diagnosis <- as.numeric(EFAdata$Diagnosis)
EFAdata$ToM.Both <- as.numeric(EFAdata$ToM.Both)
#EFAdata$TimeSpentFpc <- as.numeric(EFAdata$TimeSpentFpc)
EFAdata$TimeSpentGpc <- as.numeric(EFAdata$TimeSpentGpc)

#EFAdata$HitTime <- NULL
#EFAdata$TimeSpentFpc <- NULL


correlations <- cor(na.omit(EFAdata))
KMO(correlations)


#PCA on Assessment Scores
pca1 = prcomp(na.omit(Interesting))
screeplot(pca1)
pca1 <- prcomp(Interesting, nfactors=3)
plot(pca1)

pca2 = prcomp(na.omit(SRSadj[2:9]))
screeplot(pca2)
pca2 <- pca(Interesting, nfactors=5)
plot(pca2)

