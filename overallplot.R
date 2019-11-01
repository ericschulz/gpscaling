#house keeping
rm(list=ls())

packages <- c('MASS', 'jsonlite', 'scales', 'ggplot2', 'gridExtra', 'lsr', 'plyr')
#load them
lapply(packages, library, character.only = TRUE)

#read in data
dat<-read.csv("data/exp1data.csv")

#removing outliers
dat<-subset(dat, t>=250 & t<=5000)

#color palette
cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#standard error
se<-function(x){sd(x)/sqrt(length(x))}

#dodge
pd <- position_dodge(.2)

#TIME PER CONDITION
dp<-ddply(dat, ~cond, summarize, mu=mean(t), se=1.96*se(t))

limits <- aes(ymax = mu + se, ymin=mu - se)
dp$cond<-mapvalues(dp$cond, c("cued", "uncued"), c("Cued", "Uncued"))
p1 <- ggplot(dp, aes(y=mu, x=cond, fill=cond)) + 
  #bars
  geom_bar(position="dodge", stat="identity")+
  #geom_point(mapping = aes(x = which, y = participants), position = 'dodge')+
  #0 to 1
  #golden ratio error bars
  geom_errorbar(limits, position="dodge", width=0.31)+
  # #point size
  # geom_point(size=3)+
  scale_fill_manual(values = c(cbPalette[c(7,6)]))+
  #title
  theme_minimal() +xlab("Predictor")+ylab("Time in ms")+
  #scale_y_continuous(limits = c(0,1.0), expand = c(0, 0)) +
  ggtitle("(a) Reaction times")+
  #adjust text size
  theme(text = element_text(size=18,  family="sans"))+
  theme(legend.position = "none")+xlab("Condition")

p1


#ACUURACY PER CONDITION
dp<-ddply(dat, ~cond, summarize, mu=mean(correct), se=1.96*se(correct))
dp$cond<-mapvalues(dp$cond, c("cued", "uncued"), c("Cued", "Uncued"))
p2 <- ggplot(dp, aes(y=mu, x=cond, fill=cond)) + 
  #bars
  geom_bar(position="dodge", stat="identity")+
  #geom_point(mapping = aes(x = which, y = participants), position = 'dodge')+
  #0 to 1
  #golden ratio error bars
  geom_errorbar(limits, position="dodge", width=0.31)+
  # #point size
  # geom_point(size=3)+
  scale_fill_manual(values = c(cbPalette[c(7,6)]))+
  #title
  theme_minimal() +xlab("Predictor")+ylab("P(correct)")+
  #scale_y_continuous(limits = c(0,1.0), expand = c(0, 0)) +
  ggtitle("(b) Accuracy")+
  #adjust text size
  theme(text = element_text(size=18,  family="sans"))+
  theme(legend.position = "none")+xlab("Condition")

p2



#TIME PER CONDITION AND N
dp<-ddply(dat, ~n+cond, summarize, mu=mean(t), se=se(t))
pd <- position_dodge(.2)
dp$Condition<-mapvalues(dp$cond, c("cued", "uncued"), c("Cued", "Uncued"))

p3<-ggplot(dp, aes(x=n, y=mu, col=Condition)) +
  geom_point(position =pd)+
  #error bars
  geom_errorbar(aes(ymin=mu-se, ymax=mu+se), width=0, size=1, position=pd) +
  scale_color_manual(values = c(cbPalette[c(7,6)]))+
  #lines
  geom_line(position=pd, size=1.2) +
  #classic theme, legend on bottom
  theme_minimal()+
  theme(text = element_text(size=18,  family="sans"))+
  scale_x_continuous(breaks = round(seq(min(0), max(50), by = 10),1)) +
  ylab("Time in ms")+xlab("Number of data points")+
  theme(legend.position = "top")+
  #change theme
  ggtitle("(c) Sample size and RT")
p3


#ACCURACY PER CONDITION AND N
dp<-ddply(dat, ~n+cond, summarize, mu=mean(correct), se=se(correct))
pd <- position_dodge(.2)
dp$Condition<-mapvalues(dp$cond, c("cued", "uncued"), c("Cued", "Uncued"))

p4<-ggplot(dp, aes(x=n, y=mu, col=Condition)) +
  geom_point(position =pd)+
  #error bars
  geom_errorbar(aes(ymin=mu-se, ymax=mu+se), width=0, size=1, position=pd) +
  scale_color_manual(values = c(cbPalette[c(7,6)]))+
  #lines
  geom_line(position=pd, size=1.2) +
  #classic theme, legend on bottom
  theme_minimal()+
  theme(text = element_text(size=18,  family="sans"))+
  scale_x_continuous(breaks = round(seq(min(0), max(50), by = 10),1)) +
  ylab("P(correct)")+xlab("Number of data points")+
  theme(legend.position = "top")+
  #change theme
  ggtitle("(d) Sample size and Accuracy")
p4



#TIME PER CONDITION AND NEARBY POINTS
dp<-ddply(dat, ~near+cond, summarize, mu=mean(t), se=se(t))
dp$Condition<-mapvalues(dp$cond, c("cued", "uncued"), c("Cued", "Uncued"))
pd <- position_dodge(0.9)
dp$near<-as.factor(dp$near)
p5<-ggplot(dp, aes(x=near, y=mu, fill=Condition)) +
  #error bars
  scale_fill_manual(values = c(cbPalette[c(7,6)]))+
  #lines
  geom_bar(stat="identity", position="dodge")+  #classic theme, legend on bottom
  theme_minimal()+
  geom_errorbar(aes(ymin=mu-1.96*se, ymax=mu+1.96*se), width=0.25, size=1, position=pd) +
  theme(text = element_text(size=18,  family="sans"))+
  ylab("Time in ms")+xlab("Percentage of nearby points")+
  theme(legend.position = "top")+
  #change theme
  ggtitle("(e) Nearby points and RT")
p5


#ACCURACY PER CONDITION AND NEARBY POINTS
dp<-ddply(dat, ~near+cond, summarize, mu=mean(correct), se=se(correct))
dp$Condition<-mapvalues(dp$cond, c("cued", "uncued"), c("Cued", "Uncued"))
pd <- position_dodge(0.9)
dp$near<-as.factor(dp$near)
p6<-ggplot(dp, aes(x=near, y=mu, fill=Condition)) +
  #error bars
  scale_fill_manual(values = c(cbPalette[c(7,6)]))+
  #lines
  geom_bar(stat="identity", position="dodge")+  #classic theme, legend on bottom
  theme_minimal()+
  geom_errorbar(aes(ymin=mu-1.96*se, ymax=mu+1.96*se), width=0.25, size=1, position=pd) +
  theme(text = element_text(size=18,  family="sans"))+
  ylab("P(correct)")+xlab("Percentage of nearby points")+
  theme(legend.position = "top")+
  #change theme
  ggtitle("(f) Nearby points and Accuracy")
p6


#TIME PER CONDITION AND SMOOTHNESS
dp<-ddply(dat, ~l+cond, summarize, mu=mean(t), se=se(t))
dp$Condition<-mapvalues(dp$cond, c("cued", "uncued"), c("Cued", "Uncued"))
pd <- position_dodge(0.9)
dp$l<-as.factor(dp$l)
p7<-ggplot(dp, aes(x=l, y=mu, fill=Condition)) +
  #error bars
  scale_fill_manual(values = c(cbPalette[c(7,6)]))+
  #lines
  geom_bar(stat="identity", position="dodge")+  #classic theme, legend on bottom
  theme_minimal()+
  geom_errorbar(aes(ymin=mu-1.96*se, ymax=mu+1.96*se), width=0.25, size=1, position=pd) +
  theme(text = element_text(size=18,  family="sans"))+
  ylab("Time in ms")+xlab(expression(lambda))+
  theme(legend.position = "top")+
  #change theme
  ggtitle("(g) Smoothness and RT")
p7


#ACCURACY PER CONDITION AND SMOOTHNESS
dp<-ddply(dat, ~l+cond, summarize, mu=mean(correct), se=se(correct))
dp$Condition<-mapvalues(dp$cond, c("cued", "uncued"), c("Cued", "Uncued"))
pd <- position_dodge(0.9)
dp$l<-as.factor(dp$l)
p8<-ggplot(dp, aes(x=l, y=mu, fill=Condition)) +
  #error bars
  scale_fill_manual(values = c(cbPalette[c(7,6)]))+
  #lines
  geom_bar(stat="identity", position="dodge")+  #classic theme, legend on bottom
  theme_minimal()+
  geom_errorbar(aes(ymin=mu-1.96*se, ymax=mu+1.96*se), width=0.25, size=1, position=pd) +
  theme(text = element_text(size=18,  family="sans"))+
  ylab("P(correct)")+xlab(expression(lambda))+
  theme(legend.position = "top")+
  #change theme
  ggtitle("(h) Smoothness and Accuracy")
p8

#SAVE PLOT
library(gridExtra)
pdf("results.pdf", width=10, height=14)
grid.arrange(p1,p2,p3,p4,p5, p6,p7,p8, ncol=2)
dev.off()