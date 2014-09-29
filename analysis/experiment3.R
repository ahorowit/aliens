rm(list=ls())
library(lme4)
library(plotrix)
library(plyr)
d1 <- read.csv("~/new compy/Stanford/Research/Frank Lab/Experiments/Aliens/aliens_repo/aliens_analyses/aliens_freeResponse.csv")


##########################TEST TRIALS#########################
library(reshape)




d2=melt.data.frame(d1,c("Sub_ID","age","condition"),c("plizzle", "gorpu", "moki", "toba", "plizzle_type","gorpu_type","moki_type", "toba_type"))

data <- d2[1:96,] 
data$contrasts <- d2$value[97:192]
names(data)[4] <- "alien"
names(data)[5] <- "correct"

data$contrasts <- as.factor(data$contrasts)

hist(as.numeric(data$correct, breaks=c(0,2))
hist(as.numeric(data$correct[data$contrasts=="size"]))
hist(as.numeric(data$correct[data$contrasts=="feature"]))

###
data$correct.code <- factor(data$correct)

mss <- ddply(data, .(contrasts,correct.code, Sub_ID), summarise,
	m = length(correct.code))
ms <- ddply(mss, .(contrasts), summarise,
	None = mean(correct.code=="0"),
	Partial = mean(correct.code=="1"),
	Exact = mean(correct.code=="2"))
	

plot.style <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), axis.line = element_line(colour="black",size=.5), axis.ticks = element_line(size=.5), axis.title.x = element_text(vjust=-.5), axis.title.y = element_text(angle=90,vjust=0.25))



md <- melt(ms, id.var="contrasts")
names(md) <- c("contrasts", "Match", "prop")
md$Match <- factor(md$Match, levels=c("Exact","Partial","None"))
md$percent <- md$prop * 100


qplot(contrasts, prop, fill=Match,geom="bar",data=md, stat="identity", ylab="Proportion Response Score", xlab="Adjective Type") + plot.style







plot.style <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), legend.position=c(0.15,.75),axis.line = element_line(colour="black",size=.5), axis.ticks = element_line(size=.5), axis.title.x = element_text(vjust=-.5), axis.title.y = element_text(angle=90,vjust=0.25))


dodge <- position_dodge(width=0.9)

qplot(data = md,
	x = contrasts,
	y = prop,
	geom="bar",
	stat="identity",
	fill=Match,
	#main="Opposites -- free response", 
	ylab="Proportion Response Score", xlab="Adjective type",position=dodge,ylim=c(0,1))   + theme_bw() + plot.style #+ scale_fill_manual(values=c("orange", "red"))












lmer1 <- lmer(correct.code ~ contrasts*Age
	+ (contrasts|Sub_ID) + (contrasts|alien), 	
	 data=data) 
summary(lmer1)


glmer1<- glmer(correct ~ contrasts
	+ (1|Sub_ID) + (1|alien),	
	family="binomial", data=data) 
summary(glmer1)


binom.test(26,48)
binom.test(29,48)


