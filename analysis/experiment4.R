rm(list=ls())
library(plyr)
library(reshape2)
library(ggplot2)
library(lme4)
raw.data <- read.csv("../data/experiment4.csv")

## for bootstrapping 95% confidence intervals
theta <- function(x,xdata,na.rm=T) {mean(xdata[x],na.rm=na.rm)}
ci.low <- function(x,na.rm=T) {
  mean(x,na.rm=na.rm) - quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.025,na.rm=na.rm)}
ci.high <- function(x,na.rm=T) {
  quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.975,na.rm=na.rm) - mean(x,na.rm=na.rm)}

##########################TEST TRIALS#########################
md = melt(raw.data,c("Subj_ID","age", "agegroup", "gender", "list"), c("trial1_correct", "trial2_correct", "trial3_correct", "trial4_correct", "trial5_correct", "trial6_correct", "trial7_correct", "trial8_correct", "trial1", "trial2", "trial3","trial4","trial5","trial6","trial7","trial8",
"trial1_type", "trial2_type", "trial3_type", "trial4_type", "trial5_type", "trial6_type", "trial7_type", "trial8_type", 
"trial1_adj", "trial2_adj", "trial3_adj", "trial4_adj", "trial5_adj", "trial6_adj", "trial7_adj", "trial8_adj"))

data <- md[1: 392,]
data$alienName <- md$value[393:784]
data$contrastType <- md$value[785:1176]
data$adj <- md$value[1177:1568]
names(data)[7] <- "correct"
data$correct <- data$correct==1

agg.data <- aggregate(data$correct, list(data$contrastType, data$agegroup), FUN=sum)
agg.data.len <- aggregate(data$correct, list(data$contrastType, data$agegroup), FUN=length)
agg.data$x <- agg.data$x 
agg.data.len$x <- agg.data.len$x 

names(agg.data) <- c("contrasts", "agegroup","count")
agg.data$total <- agg.data.len$x
agg.data$prop.corr <- agg.data$count / agg.data$total

agg.data$q <- 1 - agg.data$prop.corr
agg.data$err <- sqrt((agg.data$prop.corr * agg.data$q) / agg.data$total)



plot.style <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), axis.line = element_line(colour="black",size=.5), axis.ticks = element_line(size=.5),legend.justification=c(1,0), legend.position=c(0.25,.75),legend.title=element_blank(), axis.title.x = element_text(vjust=-.5), axis.title.y = element_text(angle=90,vjust=0.25))

dodge <- position_dodge(width=0.9) 
limits <- aes(ymax = prop.corr + err, ymin=prop.corr - err) 
qplot(data = agg.data,
	x = agegroup,
	y = prop.corr,
	geom="bar",
	stat="identity",
	fill=contrasts,	
	#main="Preschooler results", 
	ylab="Proportion Correct Contrast Judgement",
	xlab="Age",
	position=dodge,
	ylim=c(0,1))  + geom_abline(intercept=.5,slope=0,lty=2) + geom_errorbar(limits,position=dodge,width=0.25) + theme_bw() + plot.style + scale_fill_manual(values=c("orange", "red"))
	
	
	
gl <- glmer(correct ~ contrastType  * age * adj + (contrastType | Subj_ID), data=data, family=binomial)
summary(gl)


gl <- glmer(correct ~  adj + age + (contrastType | Subj_ID), data=data, family=binomial)
summary(gl)


mss <- ddply(data, .(agegroup, Subj_ID), summarise, m=mean(correct))

t.test(mss$m[mss$agegroup=="4.0--4.5"] - .5)
t.test(mss$m[mss$agegroup=="4.5--5.0"] - .5)

mss <- ddply(data, .(agegroup, Subj_ID, contrastType), summarise, m=mean(correct))


t.test(mss$m[mss$agegroup=="4.0--4.5" & mss$contrastType=="size"] - .5)
t.test(mss$m[mss$agegroup=="4.0--4.5" & mss$contrastType=="feature"] - .5)

t.test(mss$m[mss$agegroup=="4.5--5.0" & mss$contrastType=="size"] - .5)
t.test(mss$m[mss$agegroup=="4.5--5.0" & mss$contrastType=="feature"] - .5)


