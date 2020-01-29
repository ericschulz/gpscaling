#Experiment 1
#GP scaling laws
#Eric and Charley
#house keeping
rm(list=ls())

packages <- c('MASS', 'jsonlite', 'scales', 'ggplot2', 'gridExtra', 'lsr', 'plyr', 'ggbeeswarm')
invisible(lapply(packages, library, character.only = TRUE))#load them

################################################
#Data processing
################################################
#read in data
dat<-read.csv("data/exp1data.csv")

#Apply Tukey Outlier removal criteria on log transformed RTs
uppertquartile <- quantile(log(dat$t), probs=c(.25, .75), na.rm=T)[2] #quantiles
lowertquartile <- quantile(log(dat$t), probs=c(.25, .75), na.rm=T)[1] 
H <- 1.5 * IQR(log(dat$t), na.rm = T)
upperLimit <- uppertquartile + H
lowerLimit <- lowertquartile - H
dat <- subset(dat, log(t)>=lowerLimit & log(t)<=upperLimit)

#Eric's outlier criteria?
#dat<-subset(dat, t>=250 & t<=5000)

#color palette
cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#standard error
se<-function(x){sd(x)/sqrt(length(x))}

#dodge
pd <- position_dodge(.2)

#########TIME PER CONDITION############
dp<-ddply(dat, ~cond, plyr::summarize, mu=mean(t), se=1.96*se(t)) #aggregate data
dpid <- ddply(dat, ~cond+id, plyr::summarize, mu = mean(t)) #individual data
limits <- aes(ymax = mu + se, ymin=mu - se)
dp$cond<-mapvalues(dp$cond, c("cued", "uncued"), c("Cued", "Uncued"))
p1 <- ggplot(dp, aes(y=mu, x=cond, fill=cond, color =cond)) + 
  #bars
  #geom_bar(position="dodge", stat="identity")+
  #geom_point(mapping = aes(x = which, y = participants), position = 'dodge')+
  #0 to 1
  #golden ratio error bars
  geom_errorbar(limits, position="dodge", width=0.25, size = 1)+
  # #point size
  geom_point()+
  scale_fill_manual(values = c(cbPalette[c(7,6)]))+
  scale_color_manual(values = c(cbPalette[c(7,6)]))+
  #title
  theme_classic() +xlab("Predictor")+ylab("Time in ms")+
  #scale_y_continuous(limits = c(0,1.0), expand = c(0, 0)) +
  ggtitle("(a) Condition")+
  #adjust text size
  theme(text = element_text(size=18,  family="sans"))+
  theme(legend.position = "none")+xlab("Condition")
p1


#ACUURACY PER CONDITION
dp<-ddply(dat, ~cond, plyr::summarize, mu=mean(correct), se=1.96*se(correct))
dp$cond<-mapvalues(dp$cond, c("cued", "uncued"), c("Cued", "Uncued"))
p2 <- ggplot(dp, aes(y=mu, x=cond, color = cond, fill=cond)) + 
  #bars
  #Geom_bar(position="dodge", stat="identity")+
  #geom_point(mapping = aes(x = which, y = participants), position = 'dodge')+
  #0 to 1
  #golden ratio error bars
  geom_errorbar(limits, position="dodge", width=0.25, size = 1)+
  # #point size
  geom_point()+
  scale_fill_manual(values = c(cbPalette[c(7,6)]))+
  scale_color_manual(values = c(cbPalette[c(7,6)]))+
  #title
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  theme_classic() +xlab("Predictor")+ylab("P(correct)")+
  #scale_y_continuous(limits = c(0,1.0), expand = c(0, 0)) +
  #ggtitle("Accuracy")+
  #adjust text size
  theme(text = element_text(size=18,  family="sans"))+
  theme(legend.position = "none")+xlab("Condition")

p2



#TIME PER CONDITION AND N
dp<-ddply(dat, ~n+cond, plyr::summarize, mu=mean(t), se=se(t))
pd <- position_dodge(.2)
dp$Condition<-mapvalues(dp$cond, c("cued", "uncued"), c("Cued", "Uncued"))

p3<-ggplot(dp, aes(x=n, y=mu, col=Condition)) +
  geom_point(position =pd)+
  #error bars
  geom_errorbar(aes(ymin=mu-se, ymax=mu+se), width=0, size=1, position=pd) +
  scale_color_manual(values = c(cbPalette[c(7,6)]), name='')+
  #lines
  geom_line(position=pd, size=1.2) +
  #classic theme, legend on bottom
  theme_classic()+
  theme(text = element_text(size=18,  family="sans"))+
  scale_x_continuous(breaks = round(seq(min(0), max(50), by = 10),1)) +
  ylab("Time in ms")+xlab("Data points")+
  theme(legend.position = c(1,1.2), legend.justification = c(1,1), legend.background=element_blank())+
  #change theme
  ggtitle("(b) Sample size")
p3


#ACCURACY PER CONDITION AND N
dp<-ddply(dat, ~n+cond, plyr::summarize, mu=mean(correct), se=se(correct))
pd <- position_dodge(.2)
dp$Condition<-mapvalues(dp$cond, c("cued", "uncued"), c("Cued", "Uncued"))

p4<-ggplot(dp, aes(x=n, y=mu, col=Condition)) +
  geom_point(position =pd)+
  #error bars
  geom_errorbar(aes(ymin=mu-se, ymax=mu+se), width=0, size=1, position=pd) +
  scale_color_manual(values = c(cbPalette[c(7,6)]), name='')+
  #lines
  geom_line(position=pd, size=1.2) +
  #classic theme, legend on bottom
  theme_classic()+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  theme(text = element_text(size=18,  family="sans"))+
  scale_x_continuous(breaks = round(seq(min(0), max(50), by = 10),1)) +
  ylab("P(correct)")+xlab("Data points")+ #+
  theme(legend.position = c(1,0), legend.justification = c(1,0), legend.background=element_blank())
  #change theme
  
p4



#TIME PER CONDITION AND NEARBY POINTS
dp<-ddply(dat, ~near+cond, plyr::summarize, mu=mean(t), se=se(t))
dp$Condition<-mapvalues(dp$cond, c("cued", "uncued"), c("Cued", "Uncued"))
pd <- position_dodge(0.9)
dp$near<-as.factor(dp$near)
p5<-ggplot(dp, aes(x=paste0(near,"%"), y=mu, color=Condition)) +
  #error bars
  scale_color_manual(values = c(cbPalette[c(7,6)]), name = '')+
  #lines
  #geom_bar(stat="identity", position="dodge")+  #classic theme, legend on bottom
  geom_point(position=pd)+
  theme_classic()+
  geom_errorbar(aes(ymin=mu-1.96*se, ymax=mu+1.96*se), width=0.25, size=1, position=pd) +
  theme(text = element_text(size=18,  family="sans"))+
  ylab("Time in ms")+xlab("Nearby points")+ ggtitle("(c) Nearby points")+
  theme(legend.position = c(1,1.2), legend.justification = c(1,1), legend.background=element_blank())
  #change theme
  #scale_x_discrete(labels = c("30\%", "70\%"))+
  
p5

#ACCURACY PER CONDITION AND NEARBY POINTS
dp<-ddply(dat, ~near+cond, plyr::summarize, mu=mean(correct), se=se(correct))
dp$Condition<-mapvalues(dp$cond, c("cued", "uncued"), c("Cued", "Uncued"))
pd <- position_dodge(0.9)
dp$near<-as.factor(dp$near)
p6<-ggplot(dp, aes(x=paste0(near,"%"), y=mu, color=Condition)) +
  #error bars
  scale_color_manual(values = c(cbPalette[c(7,6)]), name = '')+
  #lines
  #geom_bar(stat="identity", position="dodge")+  #classic theme, legend on bottom
  geom_point(position=pd)+
  theme_classic()+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  geom_errorbar(aes(ymin=mu-1.96*se, ymax=mu+1.96*se), width=0.25, size=1, position=pd) +
  theme(text = element_text(size=18,  family="sans"))+
  ylab("P(correct)")+xlab("Nearby points")+ #ggtitle("Nearby points")+
  theme(legend.position = c(0,1.2), legend.justification = c(0,1), legend.background=element_blank())
  #change theme
  
p6


#TIME PER CONDITION AND SMOOTHNESS
dp<-ddply(dat, ~l+cond, plyr::summarize, mu=mean(t), se=se(t))
dp$Condition<-mapvalues(dp$cond, c("cued", "uncued"), c("Cued", "Uncued"))
pd <- position_dodge(0.9)
dp$l<-as.factor(dp$l)
p7<-ggplot(dp, aes(x=l, y=mu, color=Condition)) +
  #error bars
  scale_color_manual(values = c(cbPalette[c(7,6)]), name = '')+
  #lines
  #geom_bar(stat="identity", position="dodge")+  #classic theme, legend on bottom
  geom_point(position=pd)+
  theme_classic()+
  geom_errorbar(aes(ymin=mu-1.96*se, ymax=mu+1.96*se), width=0.25, size=1, position=pd) +
  theme(text = element_text(size=18,  family="sans"))+
  ylab("Time in ms")+xlab(expression(lambda))+
  theme(legend.position = c(1,0), legend.justification = c(1,0), legend.background=element_blank())+
  #change theme
  ggtitle("(d) Smoothness")
p7


#ACCURACY PER CONDITION AND SMOOTHNESS
dp<-ddply(dat, ~l+cond, plyr::summarize, mu=mean(correct), se=se(correct))
dp$Condition<-mapvalues(dp$cond, c("cued", "uncued"), c("Cued", "Uncued"))
pd <- position_dodge(0.9)
dp$l<-as.factor(dp$l)
p8<-ggplot(dp, aes(x=l, y=mu, color=Condition)) +
  #error bars
  scale_color_manual(values = c(cbPalette[c(7,6)]), name = '')+
  #lines
  #geom_bar(stat="identity", position="dodge")+  #classic theme, legend on bottom
  geom_point(position=pd)+
  theme_classic()+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  geom_errorbar(aes(ymin=mu-1.96*se, ymax=mu+1.96*se), width=0.25, size=1, position=pd) +
  theme(text = element_text(size=18,  family="sans"))+
  ylab("P(correct)")+xlab(expression(lambda))+ #ggtitle("Smoothness and Accuracy")+
  theme(legend.position = c(1,0), legend.justification = c(1,0), legend.background=element_blank())
  #change theme
  
p8

#SAVE PLOT
library(cowplot)
p <- cowplot::plot_grid(p1,p3, p5, p7,p2, p4,p6,p8, ncol=4)
p
ggsave('plots/exp1Results.pdf', width = 14, height = 6, units = 'in')
