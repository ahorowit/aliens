rm(list=ls())
library(plyr)
library(reshape2)
library(ggplot2)
library(lme4)
library(bootstrap)
raw.data <- read.csv("../data/experiment5.csv")

## for bootstrapping 95% confidence intervals
theta <- function(x,xdata,na.rm=T) {mean(xdata[x],na.rm=na.rm)}
ci.low <- function(x,na.rm=T) {
  mean(x,na.rm=na.rm) - quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.025,na.rm=na.rm)}
ci.high <- function(x,na.rm=T) {
  quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.975,na.rm=na.rm) - mean(x,na.rm=na.rm)}

#### PREP DATA ####
md <- melt(raw.data, c("Sub_ID","age","condition"),
                      c("plizzle","gorpu","moki", "toba", "plizzle_type", 
                        "gorpu_type", "moki_type", "toba_type"))
data <- subset(md,!grepl("type",variable))
data$contrast <- md$value[grepl("type",md$variable)]
names(data)[4] <- "alien"
names(data)[5] <- "correct"

data$correct.code <- factor(data$correct)

mss <- ddply(data, .(contrast,correct.code, Sub_ID), summarise,
             m = length(correct.code))
ms <- ddply(mss, .(contrast), 
            function(x) {
              y <- data.frame(contrast = rep(x$contrast[1],2),
                              match = c("Partial","Exact"),
                              prop = c(mean(x$correct.code=="1"),
                                     mean(x$correct.code=="2")),
                              cil = c(ci.low(x$correct.code=="1"),
                                      ci.low(x$correct.code=="2")),
                              cih = c(ci.high(x$correct.code=="1"),
                                      ci.high(x$correct.code=="2")))
              return(y)})

#### PLOT ####
ms$stackprop <- ms$prop
ms$stackprop[ms$match=="Exact"] <- ms$stackprop[ms$match=="Exact"] + ms$stackprop[ms$match=="Partial"]

levels(ms$match) <- c("Exact contrast", "Other contrasting term")
pdf("writeup/figures/expt3.pdf",width=4, height = 4)
qplot(contrast, prop, fill=match,geom="bar",
      stat="identity", 
      data=ms, 
      ylab="Proportion of Responses", xlab="Adjective Type") + 
  ylim(c(0,1)) + 
  geom_linerange(aes(ymin=stackprop-cil,ymax=stackprop+cih)) + 
  scale_fill_discrete(name="Production Type")
dev.off()
