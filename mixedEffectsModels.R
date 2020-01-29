#mixedEffectsModels.R
#Charley Wu, Jan 2020
rm(list=ls())

#load packages
packages <- c('dplyr','cowplot', 'Rmisc', 'ggbeeswarm', 'brms', 'sjPlot', 'BayesFactor','scales',  'plyr', 'reshape2', 'ggridges', 'ggplot2', 'jsonlite', 'MASS', 'gridExtra', 'Hmisc', 'lsr', 'pander')
lapply(packages, require, character.only = TRUE)

theme_set(theme_cowplot(font_size=12))

#Wrapper for brm models such that it saves the full model the first time it is run, otherwise it loads it from disk
run_model <- function(expr, modelName, path='brmsModels', reuse = TRUE) {
  path <- paste0(path,'/', modelName, ".brm")
  if (reuse) {
    fit <- suppressWarnings(try(readRDS(path), silent = TRUE))
  }
  if (is(fit, "try-error")) {
    fit <- eval(expr)
    saveRDS(fit, file = path)
  }
  fit
}
#####################################################################################################################################################################
#Load experiment data
#####################################################################################################################################################################
#read in data
dat<-read.csv("data/exp1data.csv")

#Apply Tukey Outlier removal criteria on log transformed RTs
uppertquartile <- quantile(log(dat$t), probs=c(.25, .75), na.rm=T)[2] #quantiles
lowertquartile <- quantile(log(dat$t), probs=c(.25, .75), na.rm=T)[1] 
H <- 1.5 * IQR(log(dat$t), na.rm = T)
upperLimit <- uppertquartile + H
lowerLimit <- lowertquartile - H
dat <- subset(dat, log(t)>=lowerLimit & log(t)<=upperLimit)

#color palette
cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#####################################################################################################################################################################
#Mixed Effects Modeling
#####################################################################################################################################################################
#RT
dat$nearPercent <- dat$near/100
RTmm <- run_model(brm(log(t) ~ cond+n+nearPercent+l +(1+cond+n+nearPercent+l|id), data=subset(dat), cores=4,  iter = 4000, warmup = 1000, control = list(adapt_delta = 0.99)), modelName = 'RTmm')

bayes_R2(RTmm)
fixedTerms <- fixef(RTmm)#Look at fixed terms

#Correct
correctmm <- run_model(brm(correct ~ cond+n+nearPercent*l +(1+cond+n+nearPercent*l|id), data=subset(dat),family = "bernoulli", cores=4,  iter = 4000, warmup = 1000, control = list(adapt_delta = 0.99)), modelName = 'correctmm')

tab_model(distanceRewardMM, distanceInitialMM)
