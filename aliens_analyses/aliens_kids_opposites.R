rm(list=ls())
library(lme4)
library(plotrix)
d1 <- read.csv("~/Desktop/aliens_anayses/aliens_kids_opposites.csv")


##########################TEST TRIALS#########################
library(reshape)

d2=melt.data.frame(d1,c("Sub_ID","Age","condition","agegroup"),c("glorp","tibu","peebo", "zib", "glorp_type", "tibu_type", "peebo_type", "zib_type"))

data <- d2[1:392,] 
data$contrasts <- d2$value[393:784]
names(data)[5] <- "alien"
names(data)[6] <- "correct"

data$contrasts <- as.factor(data$contrasts)
data$correct <- data$correct==1
data$alien <- as.factor(data$alien)
data$Age=as.numeric(data$Age)
data$agegroup <- as.factor(data$agegroup)

agg.data <- aggregate(data$correct, list(data$contrasts, data$agegroup), FUN=sum)
agg.data.len <- aggregate(data$correct, list(data$contrasts, data$agegroup), FUN=length)
agg.data$x <- agg.data$x 
agg.data.len$x <- agg.data.len$x 

names(agg.data) <- c("contrasts", "agegroup", "count")
agg.data$total <- agg.data.len$x
agg.data$prop.corr <- agg.data$count / agg.data$total

agg.data$q <- 1 - agg.data$prop.corr
agg.data$err <- sqrt((agg.data$prop.corr * agg.data$q) / agg.data$total)



plot.style <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), axis.line = element_line(colour="black",size=.5), axis.ticks = element_line(size=.5),legend.justification=c(1,0),legend.position=c(1,.85),legend.title=element_blank(), axis.title.x = element_text(vjust=-.5), axis.title.y = element_text(angle=90,vjust=0.25))

dodge <- position_dodge(width=0.9) 
limits <- aes(ymax = prop.corr + err, ymin=prop.corr - err) 

qplot(data = agg.data,
	x = agegroup,
	y = prop.corr,
	geom="bar",
	stat="identity",
	fill=contrasts,
	main="Opposites -- preschooler results", ylab="Proportion Correct Contrast Judgement", xlab="Age",position=dodge,ylim=c(0,1))  + geom_abline(intercept=.5,slope=0,lty=2) + geom_errorbar(limits,position=dodge,width=.2) + theme_bw() + plot.style + scale_fill_manual(values=c("orange", "red"))





glmer1 <- glmer(correct ~ contrasts*agegroup
	+ (contrasts|Sub_ID) + (contrasts|alien), 	
	family="binomial", data=data) 
summary(int.glmer)


glmer2 <- glmer(correct ~ contrasts*Age
	+ (contrasts|Sub_ID) + (contrasts|alien), 	
	family="binomial", data=data) 
summary(int.glmer2)


