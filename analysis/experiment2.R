rm(list=ls())
source("~/Projects/R/Ranalysis/useful.R") # from github.com/langcog/Ranalysis
raw.data <- read.csv("data/experiment2.csv")

#### PREP DATA #### 
md <- melt.data.frame(raw.data, c("Sub_ID","Age","condition","agegroup","book_type"),
                      c("glorp","tibu","peebo", "zib", "glorp_type", 
                        "tibu_type", "peebo_type", "zib_type"))
data <- subset(md,!grepl("type",variable))
data$contrast <- md$value[grepl("type",md$variable)]
names(data)[6] <- "alien"
names(data)[7] <- "correct"

data$contrast <- as.factor(data$contrast)
data$correct <- data$correct==1
data$alien <- as.factor(data$alien)
data$Age <- as.numeric(data$Age)
data$agegroup <- as.factor(data$agegroup)

#### AGGREGATE ####
mss <- ddply(data, .(contrast, book_type, agegroup,Sub_ID), summarise,
             m = mean(correct),
             age = Age[1])
ms <- ddply(mss, .(contrast,agegroup, book_type), summarise,
            correct = mean(m),
            cil = ci.low(m),
            cih = ci.high(m))  

## descriptive
aggregate(age ~ agegroup, mss, mean)

#### ADD ADULTS ####
adults <- read.csv("data/adults.csv")
adults$Answer.trial1[adults$Answer.trial1==""] <- NA
adults$Answer.trial1 <- factor(adults$Answer.trial1)
adults$corr <- adults$Input.trial1_correct == adults$Answer.trial1
adults$contrast <- adults$Input.trial1_adj

ams <- ddply(subset(adults, expt=="Opposites_noSpecial"), 
             .(contrast), summarise,
             correct = mean(corr, na.rm=TRUE),
             cil = ci.low(corr),
             cih = ci.high(corr), 
             agegroup = "adults", 
             book_type = "Adjective_Only")     

ms <- rbind.fill(ms,ams)
ms$agegroup <- factor(ms$agegroup, levels=c("4.0-4.5","4.5"-"5.0","adults"))
ms$agebook <- factor(interaction(ms$agegroup,ms$book_type),
                     levels=c("4.0-4.5.Adjective_Only",
                              "4.5-5.0.Adjective_Only",
                              "4.0-4.5.Paired",
                              "4.5-5.0.Paired",
                              "adults.Adjective_Only"))

#### PLOT #### 
dodge <- position_dodge(width=0.9) 

pdf("writeup/figures/expt2.pdf",width=7, height = 4)
qplot(data = ms, 
      x = agebook, y = correct,
      geom="bar", stat="identity", fill=contrast,
      ylab="Proportion Correct Contrast Judgements", 
      xlab="Age", position=dodge, ylim=c(0,1))  + 
  geom_linerange(aes(ymin=correct - cil, ymax=correct + cih), 
                 position=dodge) +
  geom_abline(intercept=.5,slope=0,lty=2) +
  scale_fill_manual(values=c("orange", "red"))
dev.off()  

#### STATS #### 

#note models with larger random effect structures don't converge
glmer1 <- glmer(correct ~ contrast*Age*book_type
                + (contrast|Sub_ID) , 	
                family="binomial", data=data)
summary(glmer1)

glmer2 <- glmer(correct ~ contrast + Age + book_type +
                + (contrast|Sub_ID),
                family="binomial", data=data)
summary(glmer2)

anova(glmer1,glmer2)

## for t-tests
mss <- ddply(data, .(agegroup,book_type,Sub_ID), summarise,
             m = mean(correct))
t.test(mss$m[mss$agegroup=="4.0-4.5" & mss$book_type=="Adjective_Only"] - .5)
t.test(mss$m[mss$agegroup=="4.5-5.0" & mss$book_type=="Adjective_Only"] - .5)

t.test(mss$m[mss$agegroup=="4.0-4.5" & mss$book_type=="Paired"] - .5)
t.test(mss$m[mss$agegroup=="4.5-5.0" & mss$book_type=="Paired"] - .5)

t.test(mss$m[mss$agegroup=="4.0-4.5" & mss$book_type=="Adjective_Only"],
       mss$m[mss$agegroup=="4.0-4.5" & mss$book_type=="Paired"])
t.test(mss$m[mss$agegroup=="4.5-5.0" & mss$book_type=="Adjective_Only"],
       mss$m[mss$agegroup=="4.5-5.0" & mss$book_type=="Paired"])

mss <- ddply(data, .(agegroup,book_type,contrast, Sub_ID), summarise,
             m = mean(correct))

t.test(mss$m[mss$agegroup=="4.0-4.5" & mss$book_type=="Adjective_Only" & mss$contrast=="Feature"] - .5)
t.test(mss$m[mss$agegroup=="4.0-4.5" & mss$book_type=="Adjective_Only" & mss$contrast=="Size"] - .5)
t.test(mss$m[mss$agegroup=="4.5-5.0" & mss$book_type=="Adjective_Only" & mss$contrast=="Feature"] - .5)
t.test(mss$m[mss$agegroup=="4.5-5.0" & mss$book_type=="Adjective_Only" & mss$contrast=="Size"] - .5)

t.test(mss$m[mss$agegroup=="4.0-4.5" & mss$book_type=="Paired" & mss$contrast=="Feature"] - .5)
t.test(mss$m[mss$agegroup=="4.0-4.5" & mss$book_type=="Paired" & mss$contrast=="Size"] - .5)
t.test(mss$m[mss$agegroup=="4.5-5.0" & mss$book_type=="Paired" & mss$contrast=="Feature"] - .5)
t.test(mss$m[mss$agegroup=="4.5-5.0" & mss$book_type=="Paired" & mss$contrast=="Size"] - .5)

