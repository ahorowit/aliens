rm(list=ls())
library(plotrix)
library(ggplot2)

d <- read.csv("data/adults.csv")

#Assigning responses to correct or incorrect
d$Answer.trial1[d$Answer.trial1==""] <- NA
d$Answer.trial1 <- factor(d$Answer.trial1)
d$correct <- d$Input.trial1_correct == d$Answer.trial1

##aggregate correct responses along with adjective type and experiment (1a vs 2a)
agg.data <- aggregate(d$correct, list(d$Input.trial1_adj, d$expt), FUN=sum, na.rm=T)

##adds length i.e. number of total trials
agg.data.len <- aggregate(d$correct, list(d$Input.trial1_adj, d$expt), FUN=length)

##rename variables, add length and proportion correct
names(agg.data) <- c("adj","expt", "count")
agg.data$total <- agg.data.len$x
agg.data$prop.corr <- agg.data$count / agg.data$total

##calculating percent of correct answers
agg.data$q <- 1 - agg.data$prop.corr
##calculating standard error
agg.data$err <- sqrt((agg.data$prop.corr * agg.data$q) / agg.data$total)

### use ggplot to plot proportion correct by Experiment and trial type
##set up plot aethetics 
dodge <- position_dodge(width=0.9) 
limits <- aes(ymax = prop.corr + err, ymin=prop.corr - err) 

## plot proportion correct (y) by experiment (x) and adjective type (bar fills)
qplot(data = agg.data,
	x = expt,
	y = prop.corr,
	geom="bar",
	stat="identity",
	fill=adj,	
	#main="Adults Special, No Special", 
	ylab="Proportion Correct Contrast Judgement",
	xlab="Condition",
	position=dodge,
	ylim=c(0,1)) + geom_abline(intercept=.5,slope=0,lty=2) + theme_bw() + plot.style + scale_fill_manual(values=c("orange", "red"))

