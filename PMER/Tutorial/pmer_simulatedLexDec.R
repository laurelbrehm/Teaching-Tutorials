library(lme4) #for mixed effect models
library(tidyverse)  #plotting, tabulating, etc.
library(languageR)  #the data set's in here

data(lexdec)

### These are simulated data for native language effects in lexical decision-- created for teaching contrasts.
## Laurel Brehm

## separate the data by native language
lexdecE <- lexdec[lexdec$NativeLanguage=="English",]
lexdecE <- droplevels(lexdecE)
lexdecO <- lexdec[lexdec$NativeLanguage=="Other",]
lexdecO <- droplevels(lexdecO)

## draw subjects with replacement for each of 3 Other groups
## this generates 15 people from our original set of 9 that will vary
ldD<- sample(levels(lexdecO$Subject),replace=T,15)
ldG<- sample(levels(lexdecO$Subject),replace=T,15)
ldS<- sample(levels(lexdecO$Subject),replace=T,15)

## add a subject number that we will use to create a unique identity per person
ldD <- as.data.frame(cbind(ldD,1:15))
colnames(ldD)=c("Subject","Sn")
ldG <- as.data.frame(cbind(ldG,1:15))
colnames(ldG)=c("Subject","Sn")
ldS <- as.data.frame(cbind(ldS,1:15))
colnames(ldS)=c("Subject","Sn")    

## add a random RT adjustment value per sampled person. 
## mean differences are bigger for the G and S groups
ldD$RTAdj <- rnorm(15, 0,.25)
ldG$RTAdj <- rnorm(15, .5,.25)
ldS$RTAdj <- rnorm(15, .5,.25)

## add new native language variable
ldD$NativeLangSim <- "Dutch"
ldG$NativeLangSim <- "German"
ldS$NativeLangSim <- "Spanish"

## create separate data frames for 3 Other groups by merging subject list, subject number
lexdecD <- merge(lexdecO,ldD)
lexdecG <- merge(lexdecO,ldG)
lexdecS <- merge(lexdecO,ldS)

## stick the "other" ones together
lexdecO <- rbind(lexdecD,lexdecG,lexdecS)

## adjust the RT based on our random draw
lexdecO$RT <- lexdecO$RT + lexdecO$RTAdj
## drop the adjustment variable
lexdecO$RTAdj <- NULL

## add native language variable & Sn for English frame
lexdecE$NativeLangSim <- lexdecE$NativeLanguage
lexdecE$Sn <- as.numeric(lexdecE$Subject)

## bind both ds together
lexdecSm <- rbind(lexdecO,lexdecE)

## and revalue Subject to be unique per person
lexdecSm$Subject <- as.factor(paste(lexdecSm$Subject,lexdecSm$Sn,lexdecSm$NativeLangSim,sep=""))

## and make NativeLangSim a factor
lexdecSm$NativeLangSim <- as.factor(lexdecSm$NativeLangSim)

## write out
write.table(lexdecSm,'simulatedLexDec.txt',col.names=T,row.names=F,sep="\t")
