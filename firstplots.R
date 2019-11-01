#house keeping
rm(list=ls())

packages <- c('MASS', 'jsonlite', 'scales', 'ggplot2', 'gridExtra', 'lsr', 'plyr')
#load them
lapply(packages, library, character.only = TRUE)

dat<-read.csv("data/exp1data.csv")
dat<-subset(dat, cond=="uncued")
dat<-subset(dat, t>=250 & t<=5000)


d<-ddply(subset(dat,t>=250, t<=5000), ~near, summarize, m=mean(t, na.rm=TRUE))

cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


dat$l<-as.factor(dat$l)
#data frame with everything we need, i.e. min, max, lower nad upper quantile, median, CIs & mean
dp1<-ddply(subset(dat,t>=250, t<=5000), ~l, summarize, 
           d_ymin = max(min(t), quantile(t, 0.25) - 1.5 * IQR(t)), 
           d_ymax = min(max(t), quantile(t, 0.75) + 1.5 * IQR(t)),
           d_lower = quantile(t, 0.25),  d_middle = median(t), d_upper = quantile(t, 0.75),
           mu=mean(t))

########################################################
#Figure A: Error
########################################################
dat1<-ddply(subset(dat,t>=250, t<=5000), ~id+l, summarise, error=mean(t))
t.test(subset(dat1, l==2)$error, subset(dat1, l==5)$error)


p1<-ggplot(data = dp1) +
  #boxplot with given values, we only need half of it
  geom_boxplot(aes(x = as.numeric(l)-0.2, ymin = d_lower, ymax = d_upper, lower = d_lower, 
                   middle = d_middle, upper = d_upper, width = 2 * 0.2, fill = l), stat = "identity") +
  #jitter of raw data points, needs the full data frame
  geom_jitter(data=dat1, aes(x = as.numeric(l) + 0.2,  y = error,  color = l), 
              width = 0.2 - 0.25 * 0.2, height = 0, size=1)+
  #vertical segment
  geom_segment(aes(x = as.numeric(l), y = d_ymin, xend = as.numeric(l), yend = d_ymax)) +
  geom_point(aes(x = as.numeric(l)-0.2, y = mu), shape=23, size=3, fill="white", color="black") +
  #top horizontal segment
  geom_segment(aes(x = as.numeric(l) - 0.1,  y = d_ymax, xend = as.numeric(l),  yend = d_ymax)) +
  #top vertical segment
  geom_segment(aes(x = as.numeric(l) - 0.1, y = d_ymin, xend = as.numeric(l), yend = d_ymin)) +
  #theme minimal
  theme_minimal()+
  #sans
  theme(text = element_text(size=18,  family="sans"))+
  #colors and fill
  scale_fill_manual(values = c(cbPalette[c(7,6)], "grey40"))+
  scale_color_manual(values = c(cbPalette[c(7,6)], "grey40"))+
  #labs
  xlab(expression(lambda))+ylab("Time in ms")+
  #no legend
  theme(legend.position="none", strip.background=element_blank(), legend.key=element_rect(color=NA))+
  #labe x-axis
  scale_x_continuous(breaks = c(1,2),labels = c("2","5"))+ggtitle("(a) Smoothness")
p1


dat$near<-as.factor(dat$near)
#data frame with everything we need, i.e. min, max, lower nad upper quantile, median, CIs & mean
dp1<-ddply(subset(dat,t>=250, t<=5000), ~near, summarize, 
           d_ymin = max(min(t), quantile(t, 0.25) - 1.5 * IQR(t)), 
           d_ymax = min(max(t), quantile(t, 0.75) + 1.5 * IQR(t)),
           d_lower = quantile(t, 0.25),  d_middle = median(t), d_upper = quantile(t, 0.75),
           mu=mean(t))

dat1<-ddply(subset(dat,t>=250, t<=5000), ~id+near, summarise, error=mean(t))
dat1

t.test(subset(dat1, near==70)$error-subset(dat1, near==30)$error)

p2<-ggplot(data = dp1) +
  #boxplot with given values, we only need half of it
  geom_boxplot(aes(x = as.numeric(near)-0.2, ymin = d_lower, ymax = d_upper, lower = d_lower, 
                   middle = d_middle, upper = d_upper, width = 2 * 0.2, fill = near), stat = "identity") +
  #jitter of raw data points, needs the full data frame
  geom_jitter(data=dat1, aes(x = as.numeric(near) + 0.2,  y = error,  color = near), 
              width = 0.2 - 0.25 * 0.2, height = 0, size=1)+
  #vertical segment
  geom_segment(aes(x = as.numeric(near), y = d_ymin, xend = as.numeric(near), yend = d_ymax)) +
  geom_point(aes(x = as.numeric(near)-0.2, y = mu), shape=23, size=3, fill="white", color="black") +
  #top horizontal segment
  geom_segment(aes(x = as.numeric(near) - 0.1,  y = d_ymax, xend = as.numeric(near),  yend = d_ymax)) +
  #top vertical segment
  geom_segment(aes(x = as.numeric(near) - 0.1, y = d_ymin, xend = as.numeric(near), yend = d_ymin)) +
  #theme minimal
  theme_minimal()+
  #sans
  theme(text = element_text(size=18,  family="sans"))+
  #colors and fill
  scale_fill_manual(values = c(cbPalette[c(7,6)], "grey40"))+
  scale_color_manual(values = c(cbPalette[c(7,6)], "grey40"))+
  #labs
  xlab("Proportion of nearby points")+ylab("Time in ms")+
  #no legend
  theme(legend.position="none", strip.background=element_blank(), legend.key=element_rect(color=NA))+
  #labe x-axis
  scale_x_continuous(breaks = c(1,2),labels = c("30%","70%"))+ggtitle("(b) Nearby points")
p2


se<-function(x){sd(x)/sqrt(length(x))}
dp<-ddply(dat, ~n, summarize, mu=mean(t), se=se(t))

pd <- position_dodge(.2)

p3<-ggplot(dp, aes(x=n, y=mu)) +
  geom_point(position =pd)+
  #error bars
  geom_errorbar(aes(ymin=mu-se, ymax=mu+se), width=0, size=1, position=pd) +
  #scale_color_manual(values=c("#F0E442", "#E69F00", "#009E73", "#56B4E9"))+
  #lines
  geom_line(position=pd, size=1.2) +
  scale_color_manual(values="grey")+
  #classic theme, legend on bottom
  theme_minimal()+
  theme(text = element_text(size=18,  family="sans"))+
  scale_x_continuous(breaks = round(seq(min(0), max(50), by = 10),1)) +
  ylab("Time in ms")+xlab("Number of data points")+
  #change theme
  ggtitle("(c) Sample size")
p3

library(gridExtra)
pdf("time_effects.pdf", width=13, height=4.5)
grid.arrange(p1,p2,p3, nrow=1)
dev.off()

library(brms)
dat$n<-as.numeric(scale(dat$n))
head(dat)
dat$l<-as.factor(dat$l)
dat$near<-as.factor(dat$near)

m<-brm(t~n+near+l+(1|id), data=dat)
summary(m)
hist(dat$t)

dat$l<-as.factor(dat$l)
dp<-ddply(dat, ~l, summarize, mu=mean(correct), se=1.96*se(correct))
limits <- aes(ymax = mu + se, ymin=mu - se)

p4 <- ggplot(dp, aes(y=mu, x=l, fill=l)) + 
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
  scale_y_continuous(limits = c(0,1.0), expand = c(0, 0)) +
  ggtitle("(a) Smoothness")+
  #adjust text size
  theme(text = element_text(size=18,  family="sans"))+
  theme(legend.position = "none")+xlab(expression(lambda))
p4


dat$near<-as.factor(dat$near)
dp<-ddply(dat, ~near, summarize, mu=mean(correct), se=1.96*se(correct))

limits <- aes(ymax = mu + se, ymin=mu - se)

p5 <- ggplot(dp, aes(y=mu, x=near, fill=near)) + 
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
  scale_y_continuous(limits = c(0,1.0), expand = c(0, 0)) +
  ggtitle("(b) Nearby points")+
  scale_x_discrete(labels = c("30%","70%"))+
  #adjust text size
  theme(text = element_text(size=18,  family="sans"))+
  theme(legend.position = "none")+xlab("Proportion of nearby points")
p5


dp<-ddply(dat, ~n, summarize, mu=mean(correct), se=se(correct))

pd <- position_dodge(.2)

p6<-ggplot(dp, aes(x=n, y=mu)) +
  geom_point(position =pd)+
  #error bars
  geom_errorbar(aes(ymin=mu-se, ymax=mu+se), width=0, size=1, position=pd) +
  #scale_color_manual(values=c("#F0E442", "#E69F00", "#009E73", "#56B4E9"))+
  #lines
  geom_line(position=pd, size=1.2) +
  scale_color_manual(values="grey")+
  #classic theme, legend on bottom
  theme_minimal()+
  theme(text = element_text(size=18,  family="sans"))+
  scale_x_continuous(breaks = round(seq(min(0), max(50), by = 10),1)) +
  ylab("P(correct)")+xlab("Number of data points")+
  #change theme
  ggtitle("(c) Sample size")
p6

pdf("choice_effects.pdf", width=13, height=4.5)
grid.arrange(p4,p5,p6, nrow=1)
dev.off()

m<-brm(correct~n+near+l+(1|id), data=dat, family="bernoulli")
summary(m)

m<-glm(dat$correct~dat$t, family="binomial")
summary(m)
