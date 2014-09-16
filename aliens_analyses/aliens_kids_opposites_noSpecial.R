rm(list=ls())
library(lme4)
library(plotrix)
library(ggplot2)
d1 <- read.csv("~/new compy/Stanford/Research/Frank Lab/Experiments/Aliens/aliens_repo/aliens_analyses/aliens_kids_opposites_noSpecial.csv")


##########################TEST TRIALS#########################
library(reshape)

d2=melt.data.frame(d1,c("Sub_ID","experimenter","Age","condition","book_type","agegroup"),c("glorp","tibu","peebo", "zib", "glorp_type", "tibu_type", "peebo_type", "zib_type"))

data <- d2[1:576,] 
data$contrasts <- d2$value[577:1152]
names(data)[7] <- "alien"
names(data)[8] <- "correct"

data$contrasts <- as.factor(data$contrasts)
data$correct <- data$correct==1
data$Age=as.numeric(data$Age)
data$agegroup <- as.factor(data$agegroup)
#data$older <- (data$Age>4.5)



agg.data <- aggregate(data$correct, list(data$contrasts, data$agegroup, data$book_type), FUN=sum)
agg.data.len <- aggregate(data$correct, list(data$contrasts, data$agegroup, data$book_type), FUN=length)
agg.data$x <- agg.data$x 
agg.data.len$x <- agg.data.len$x 

names(agg.data) <- c("contrasts", "older", "book_type", "count")
agg.data$total <- agg.data.len$x
agg.data$prop.corr <- agg.data$count / agg.data$total

agg.data$q <- 1 - agg.data$prop.corr
agg.data$err <- sqrt((agg.data$prop.corr * agg.data$q) / agg.data$total)


plot.style <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), axis.line = element_line(colour="black",size=.5), axis.ticks = element_line(size=.5),legend.justification=c(1,0), legend.position=c(0.25,.75),legend.title=element_blank(), axis.title.x = element_text(vjust=-.5), axis.title.y = element_text(angle=90,vjust=0.25))

dodge <- position_dodge(width=0.9) 
limits <- aes(ymax = prop.corr + err, ymin=prop.corr - err) 

#agg.data$age <- ifelse(agg.data$older, "4.5-5.0", "4.0-4.5")

qplot(data = agg.data,
	x = older,
	y = prop.corr,
	geom="bar",
	stat="identity",
	fill=contrasts,	
	#main="Preschooler results", 
	ylab="Proportion Correct Contrast Judgement",
	xlab="Age",
	position=dodge,
	ylim=c(0,1)) + facet_wrap("book_type") + geom_abline(intercept=.5,slope=0,lty=2) + geom_errorbar(limits,position=dodge,width=0.25) + theme_bw() + plot.style + scale_fill_manual(values=c("orange", "red"))


################## 
## merge kid and adult data for opposites 'special'
d3 <- read.csv("~/new compy/Stanford/Research/Frank Lab/Experiments/Aliens/aliens_repo/aliens_analyses/aliens_adults_opposites_noSpecial.csv")

d3$Answer.trial1[d3$Answer.trial1==""] <- NA
d3$Answer.trial1 <- factor(d3$Answer.trial1)
d3$correct <- d3$Input.trial1_correct == d3$Answer.trial1

agg.data3 <- aggregate(d3$correct, list(d3$Input.trial1_adj, d3$age, d3$book_type), FUN=sum, na.rm=T)

##adds length i.e. number of total trials
agg.data3.len <- aggregate(d3$correct, list(d3$Input.trial1_adj, d3$age, d3$book_type), FUN=length)

##rename variables, add length and proportion correct
names(agg.data3) <- c("contrasts","older", "book_type", "count")
agg.data3$total <- agg.data3.len$x
agg.data3$prop.corr <- agg.data3$count / agg.data3$total

##calculating percent of correct answers
agg.data3$q <- 1 - agg.data3$prop.corr
##calculating standard error
agg.data3$err <- sqrt((agg.data3$prop.corr * agg.data3$q) / agg.data3$total)


agg.data2 <- rbind (agg.data, agg.data3)

plot.style <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), axis.line = element_line(colour="black",size=.5), axis.ticks = element_line(size=.5),legend.justification=c(1,0),legend.position=c(0.15,.75),legend.title=element_blank(), axis.title.x = element_text(vjust=-.5), axis.title.y = element_text(angle=90,vjust=0.25))


agg.data2$age <- if(agg.data2$older=="0", agg.data2$age=="4.0--4.5")
mydata <- rename(mydata, c(oldname="newname"))

agg.data2$older <- rename(agg.data2$older, c(0=="4.0--4.5", 1=="4.5--5.0", Adult=="Adult"))

qplot(data = agg.data2,
	x = older,
	y = prop.corr,
	geom="bar",
	stat="identity",
	fill=contrasts,	
	#main="Preschooler results", 
	ylab="Proportion Correct Contrast Judgement",
	xlab="Age",
	position=dodge,
	ylim=c(0,1)) + facet_wrap("book_type",  scales="free"
)  + geom_abline(intercept=.5,slope=0,lty=2) + geom_errorbar(limits,position=dodge,width=0.25) + theme_bw() + plot.style + scale_fill_manual(values=c("orange", "red"))







glmer1 <- glmer(correct ~ contrasts*older
	+ (contrasts|Sub_ID) + (contrasts|alien), 	
	family="binomial", data=data) 
summary(glmer1)



glmer2 <- glmer(correct ~ book_type*contrasts*older
	+ (contrasts|Sub_ID) + (contrasts|alien), 	
	family="binomial", data=data) 
summary(glmer2)



glmer3 <- glmer(correct ~ book_type*contrasts
	+ (contrasts|Sub_ID) + (contrasts|alien), 	
	family="binomial", data=data) 
summary(glmer3)


glmer4 <- glmer(correct ~ book_type + older
	+ (contrasts|Sub_ID) + (contrasts|alien), 	
	family="binomial", data=data) 
summary(glmer4)


gl <- glmer(correct ~ book_type + (contrasts | Sub_ID), data=data, family=binomial)
summary(gl)


gl <- glmer(correct ~ book_type + contrasts + agegroup + (1 | Sub_ID), data=subset(data,book_type!="scramble"), family=binomial)
summary(gl)



gl <- glmer(correct ~ contrasts * book_type * agegroup + (contrasts | Sub_ID), data=data, family=binomial)
summary(gl)

gl <- glmer(correct ~ contrasts * book_type + (contrasts | Sub_ID), data=data, family=binomial)
summary(gl)





################# average across age
ave.data <- aggregate(data$correct, list(data$older, data$book_type), FUN=sum)
ave.data.len <- aggregate(data$correct, list(data$older, data$book_type), FUN=length)
ave.data$x <- ave.data$x 
ave.data.len$x <- ave.data.len$x 

names(ave.data) <- c("older", "book_type", "count")
ave.data$total <- ave.data.len$x
ave.data$prop.corr <- ave.data$count / ave.data$total

ave.data$q <- 1 - ave.data$prop.corr
ave.data$err <- sqrt((ave.data$prop.corr * ave.data$q) / ave.data$total)


plot.style <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), axis.line = element_line(colour="black",size=.5), axis.ticks = element_line(size=.5),legend.justification=c(1,0), legend.position=c(0.25,.75),legend.title=element_blank(), axis.title.x = element_text(vjust=-.5), axis.title.y = element_text(angle=90,vjust=0.25))

dodge <- position_dodge(width=0.9) 
limits <- aes(ymax = prop.corr + err, ymin=prop.corr - err) 

ave.data$age <- ifelse(ave.data$older, "4.5-5.0", "4.0-4.5")

qplot(data = ave.data,
	x = age,
	y = prop.corr,
	geom="bar",
	stat="identity",
	fill="blue",	
	#main="Preschooler results", 
	ylab="Proportion Correct Contrast Judgement",
	xlab="Condition",
	position=dodge,
	ylim=c(0,1)) + facet_wrap("book_type")+ geom_abline(intercept=.5,slope=0,lty=2) + geom_errorbar(limits,position=dodge,width=0.25) + theme_bw() + plot.style 
	#+ scale_fill_manual(values=c("orange", "red"))






##########compare only no special and pairs
expt3 <- data[data$book_type!="scramble",]

gl <- glmer(correct ~ contrasts * book_type * agegroup + (contrasts | Sub_ID), data=data, family=binomial)
summary(gl)



## compare expt 2b and 3
int.glmer <- glmer(correct ~ contrasts*book_type*older+(contrasts|Sub_ID) + (1|alien), family="binomial", data=data) 
summary(int.glmer)


## expt 2b (No Special) only
twob <- data[data$book_type=="Experiment 2b",]
twob$contrasts <- factor(twob$contrasts,levels=c("Size","Feature"))

int.NoSpecial <- glmer(correct ~ contrasts*older+(contrasts|Sub_ID) + (1|alien), family="binomial", data=twob) 
summary(int.NoSpecial)
aggregate(correct ~ contrasts + older + alien, data=twob, mean)

## Expt 3 (opposite pairs) only 
int.pairs <- glmer(correct ~ contrasts*older+(contrasts|Sub_ID) + (1|alien), family="binomial", data=data[data$book_type=="Experiment 3",]) 
summary(int.pairs)

