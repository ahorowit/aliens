rm(list=ls())
library(plotrix)
#install.packages("ggplot2")
library(ggplot2)

d1 <- read.csv("~/Desktop/aliens_anayses/aliens_adults_opposites_special_and_noSpecial.csv")

##############################################

###anonymize worker IDs
anonymize.sids <- function(df, subject_column_label) {
  subj_col = which(names(df) == subject_column_label) # get workerid column index
  temp <- data.frame(workerid = unique(df[,subj_col])) # make new df of unique workerids
  temp$subid <- 1:length(unique(df[,subj_col])) # make list of subids
  index <- match(df[,subj_col], temp$workerid) 
  df$subids <- temp$subid[index]
  df[,subj_col] <- NULL 
  df$subids  = as.factor(df$subids)
  return(df)
}



#Assigning responses to correct or incorrect
d1$Answer.trial1[d1$Answer.trial1==""] <- NA
d1$Answer.trial1 <- factor(d1$Answer.trial1)
d1$correct <- d1$Input.trial1_correct == d1$Answer.trial1

##aggregate correct responses along with adjective type and experiment (1a vs 2a)
agg.data <- aggregate(d1$correct, list(d1$Input.trial1_adj, d1$expt), FUN=sum, na.rm=T)

##adds length i.e. number of total trials
agg.data.len <- aggregate(d1$correct, list(d1$Input.trial1_adj, d1$expt), FUN=length)

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
plot.style <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), axis.line = element_line(colour="black",size=.5), axis.ticks = element_line(size=.5),legend.justification=c(1,0), legend.position=c(1,.85),legend.title=element_blank(), axis.title.x = element_text(vjust=-.5), axis.title.y = element_text(angle=90,vjust=0.25))

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

