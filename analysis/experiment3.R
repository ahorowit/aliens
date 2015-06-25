rm(list=ls())
source("~/Projects/R/Ranalysis/useful.R") # from github.com/langcog/Ranalysis
raw.data <- read.csv("data/experiment1.csv")

#### PREP DATA #### 

md <- melt.data.frame(raw.data, c("Sub_ID","Age","condition","agegroup"),
                      c("glorp","tibu","peebo", "zib", "glorp_type", 
                        "tibu_type", "peebo_type", "zib_type"))
data <- subset(md,!grepl("type",variable))
data$contrast <- md$value[grepl("type",md$variable)]
names(data)[5] <- "alien"
names(data)[6] <- "correct"

data$contrast <- as.factor(data$contrast)
data$correct <- data$correct==1
data$alien <- as.factor(data$alien)
data$Age <- as.numeric(data$Age)
data$agegroup <- as.factor(data$agegroup)

#### AGGREGATE ####

mss <- ddply(data, .(contrast,agegroup,Sub_ID), summarise,
             m = mean(correct))
ms <- ddply(mss, .(contrast,agegroup), summarise,
            correct = mean(m),
            cil = ci.low(m),
            cih = ci.high(m))             

#### ADD ADULTS ####
adults <- read.csv("data/adults.csv")
adults$Answer.trial1[adults$Answer.trial1==""] <- NA
adults$Answer.trial1 <- factor(adults$Answer.trial1)
adults$corr <- adults$Input.trial1_correct == adults$Answer.trial1
adults$contrast <- adults$Input.trial1_adj

ams <- ddply(subset(adults, expt=="Opposites_contrastFraming"), 
             .(contrast), summarise,
             correct = mean(corr, na.rm=TRUE),
             cil = ci.low(corr),
             cih = ci.high(corr), 
             agegroup = "adults")     

ms <- rbind.fill(ms,ams)


#### PLOT #### 
dodge <- position_dodge(width=0.9) 

pdf("writeup/figures/expt1.pdf",width=7, height = 4)
qplot(data = ms, x = agegroup, y = correct,
      geom="bar", stat="identity", fill=contrast,
      ylab="Proportion Correct Contrast Judgements", 
      xlab="Age", position=dodge, ylim=c(0,1))  + 
  geom_linerange(aes(ymin=correct - cil, ymax=correct + cih), 
                 position=dodge) +
  geom_abline(intercept=.5,slope=0,lty=2) +
  scale_fill_manual(values=c("orange", "red"))
dev.off()  

#### STATS #### 

glmer1 <- glmer(correct ~ contrast*agegroup
	+ (contrast|Sub_ID) + (contrast|alien), 	
	family="binomial", data=data) 
summary(int.glmer)

glmer2 <- glmer(correct ~ contrast*Age
	+ (contrast|Sub_ID) + (contrast|alien), 	
	family="binomial", data=data) 
summary(int.glmer2)

## for t-tests
mss <- ddply(data, .(agegroup,Sub_ID), summarise,
             m = mean(correct))
t.test(mss$m[mss$agegroup=="3.0-3.5"] - .5)
t.test(mss$m[mss$agegroup=="3.5-4.0"] - .5)
t.test(mss$m[mss$agegroup=="4.0-4.5"] - .5)
t.test(mss$m[mss$agegroup=="4.5-5.0"] - .5)
