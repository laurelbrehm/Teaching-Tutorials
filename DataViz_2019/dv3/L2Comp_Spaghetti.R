### making spaghtti plots in data vis course, march 2019
### These data come from Brehm, Jackson, & Miller, BUCLD 2018 proceedings.
### Note: a lot of data cleaning happened in an earlier file.
### These are not files fresh from the tracker-- they have the IAs and trial ids merged in.
### In this experiment, proficient L2 speakers of English (L1 Spanish) hear sentences with occasional agreement anomalies.
### We ask them to select which image matches the subject of the sentence
### Items are sentences like: "The key to the cabinet(s) literally was/were on the table"  
### noun1, noun2, adverb, verb = timestamps of important words in the sentence (varies by item)
### RT = clicking latency for subject selection zeroed to trial onset

### these data are from the fixation report outputb type in eyelink. this is a nice, compact way of storing data-- 
## simply combines all the fixations to one pre-defined interest area over time, listing the start and end of the fixation.
## to make a spaghetti plot, we need to expand the fixations to list out what's being looked at every msec
## then we'll do some subtraction to set the zero point to the onset of the verb (=the critical word)

## these data also only contain trials of the SPP type (=key to the cabinets were) = a highly plausible error.
## sometimes these were answered canonically subject='key' (=Lithead) and sometimes non-literally subject='keys' (=LitHeadFoil)

library(reshape)
library(ggplot2)
library(plyr)

##### read in & expand ######
ssSVA1<- read.table("L2Comp-SPP-n28.txt",header=T)

## add row names
ssSVA1$row <- row.names(ssSVA1)

#expand fixations to every msec-- current fix dur to list of fixes in the duration
ssSVA2<-untable(ssSVA1, num=ssSVA1[,19])  #expanding the column current_fix_dur, adding extra rows for every msec of the duration of the fixation
ssSVA2$row2 <- rownames(ssSVA2) ## check whether the row name has been changed by this
ssSVA2$row2<- ifelse(ssSVA2$row2 == ssSVA2$row,paste(ssSVA2$row2,0,sep="."),ssSVA2$row2)  # if it has, take the tabled part of the row name off and assign it to index
index <-  colsplit(ssSVA2$row2,"[.]",names=c("row","index"))[2]
ssSVA2 <- cbind(ssSVA2,index)
ssSVA2$msec <- ssSVA2$CURRENT_FIX_START + ssSVA2$index  ## add the index to the current fix

## now drop out the extra stuff from this computation
ssSVA2$CURRENT_FIX_DUR <- NULL
ssSVA2$CURRENT_FIX_START <- NULL
ssSVA2$CURRENT_FIX_END <- NULL
ssSVA2$row <- NULL
ssSVA2$row2 <- NULL

## initialize zero from critical words
ssSVA2$zN1 <- as.numeric(ssSVA2$msec - ssSVA2$noun1)
ssSVA2$zN2 <- as.numeric(ssSVA2$msec - ssSVA2$noun2)
ssSVA2$zV <- as.numeric(ssSVA2$msec - ssSVA2$verb)
ssSVA2$zC <- as.numeric(ssSVA2$msec - ssSVA2$RT)


####tabulate for spaghetti   ###
### change zero by changing msec variable. zV is zeroed to verb.
ssSVA <- as.data.frame(table(ssSVA2$RespCode,ssSVA2$PicID,round(ssSVA2$zV)))
colnames(ssSVA)<- c("Response","PicID","msec","Count")
ssSVA <- ssSVA[ssSVA$Count!=0,]  ## if using table rather than dplyr, it's gonna be slow

## also count how many observations there are at each msec, to tabulate across
ss2 <- as.data.frame(table(ssSVA2$RespCode,round(ssSVA2$zV)))
colnames(ss2)<- c("Response","msec","TotalCount")
ss2 <- ss2[ss2$TotalCount!=0,]

ssSVA <- merge(ssSVA,ss2) ## this lazy merge will take only the cases where ssSVA occurred
ssSVA$Prop <- ssSVA$Count / ssSVA$TotalCount
ssSVA$msec = as.numeric(as.character(ssSVA$msec))

## downsample to 2 msec
ssSVA <- ssSVA[ssSVA$msec %% 2 == 0,]


### warning: slow. suggested that you read this in instead.
## first, downsample data to every 10 msec
ssSVAe <- ssSVA2[ssSVA2$zV %% 10 == 0,]

## create a list of subs
ssI <- levels(ssSVAe$subj)
N <- length(ssI)
## make a trial counter for merges
is <- seq(1:N)

## draw with replacement many times. I'd normally do 1000.
for (i in 1:10){
  subj <- sample(ssI,N,replace=T)
  b <- cbind(subj,is)
  
  b2 <- merge(b,ssSVAe)
  
  ## do the same tabulation as before
  b3 <- as.data.frame(table(b2$RespCode,b2$zV))
  colnames(b3)<- c("Response","msec","TotalCount")
  b3 <- b3[b3$TotalCount!=0,]
  
  #overwrite b2
  b2 <- as.data.frame(table(b2$RespCode,b2$PicID,b2$zV))
  colnames(b2)<- c("Response","PicID","msec","Count")
  b2 <- b2[b2$Count!=0,]
  
  b2 <- merge(b2,b3)
  b2$Prop <- b2$Count / b2$TotalCount
  b2$msec = as.numeric(as.character(b2$msec))
  
  b2$Iter = i
  ###smush them together
  ifelse(i==1,bs<-b2,bs<-rbind(bs,b2))
}

## trim based on plot choices
bs <- bs[bs$msec > -1700 & bs$msec < 1500,]
## take only trials of interest
bs <- bs[bs$Response=="LitHeadFoil" | bs$Response=="LitHead",]

ssSVAcis <- ddply(bs,.(Response,msec,PicID),summarise, lower=quantile(Prop,.025),upper=quantile(Prop,.975))


## plot spaghetti ###
ggplot(ssSVA[ssSVA$Response=="LitHeadFoil" | ssSVA$Response=="LitHead",],
       aes(x=msec,y=Prop,color=PicID))+
  geom_line()+
  facet_grid(~Response)+
  scale_x_continuous("Time relative to verb onset (ms)", limits=c(-1700,1500))+
  scale_y_continuous("Proportion of fixations", limits=c(0,1))+
  geom_ribbon(data=ssSVAcis, aes(ymin=lower, ymax=upper,x=msec,y=lower,fill=PicID),color="white",lwd=0,alpha=.2)+
  theme_bw()+
  scale_color_manual(values=c("red","firebrick","dodgerblue","blue","gray"))+
  scale_fill_manual(values=c("red","firebrick","dodgerblue","blue","gray"))




