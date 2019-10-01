## plotting examples for IMPRS data vis workshop
## Laurel Brehm, 28 March 2018

#install.packages('languageR')
## ^ install whatever you need to using code like this

## then load up your libraries
library(languageR)
library(ggplot2)
library(ggridges)  ## this is the package that has the joyplot stuff in it

### use lexical decision data from Harald Baayen: get it and look at it
data(lexdec)
summary(lexdec)

## your task is to play with these data!  
##use the geoms below to visualize various variables.
## try taking these code chunks and changing Y and X variables
## see what you can find!

#scatterplot using linear model smooth
ggplot(lexdec,aes(y=RT,x=Frequency))+
  geom_point(color="black")+
  geom_smooth(method="lm")

### binning frequency to make a bar plot
mf <- median(lexdec$Frequency)
lexdec$FreqBinned <- ceiling((lexdec$Frequency - mf)/mf)

## here's the code for a bar plot with error bars
## I've done my tabulation inside the plot!
ggplot(
  lexdec %>% group_by(FreqBinned) %>%
  summarise(meanRT = mean(RT), seRT = sd(RT), n=n()),
  aes(y=meanRT,x=FreqBinned))+
  geom_bar(stat='identity')+
  geom_errorbar(aes(ymax=meanRT+(seRT/sqrt(n)),ymin=meanRT-(seRT/sqrt(n))),width=.1)+
  scale_x_continuous(breaks=c(0,1),labels=c("Low","High"))

## joyplot based on previous answer
ggplot(lexdec,aes(x=RT,y=PrevCorrect))+
  geom_density_ridges(fill="violet",alpha=.8)

## density plot based on previous answer
ggplot(lexdec,aes(x=RT,fill=PrevCorrect))+
  geom_density(alpha=.5)

## violin plot based on previous answer
## overplotting points with some amount of jitter
ggplot(lexdec,aes(y=RT,x=PrevCorrect))+
  geom_violin()+
  geom_jitter(alpha=.1)

## joyplot based upon word length
ggplot(lexdec,aes(x=RT,y=as.factor(Length)))+
  geom_density_ridges(aes(fill=Length))+
  scale_y_discrete("Word length (letters)")


## violins with overplotted means and medians
## text labels are just a little hack solution-- 
## they are many times overplotted & it makes the graph big & slow to compute
ggplot(lexdec,aes(y=RT,x=as.factor(Length)))+
  geom_violin(aes(fill=Length))+
  stat_summary(fun.y="mean",geom="point",pch="-",size=10)+
  stat_summary(fun.y="median",geom="point",pch="-",size=10,color="blue")+
  geom_text(aes(label="mean",x=7,y=7.5))+
  geom_text(aes(label="median",x=7,y=7.4),color="blue")+
  scale_x_discrete("Word length (letters)")

## scatterplot with x-jittered points
ggplot(lexdec,aes(y=RT,x=Length,color=Length))+
  geom_jitter(height=0,width=.2)+
  geom_smooth(method="lm")
