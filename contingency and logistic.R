#Cleaning
#Read in provided dataset.
setwd('D:\\data_science\\data_science\\program\\m2982 µÿ¿Ìr”Ô—‘\\code\\code')
dat = read.csv("malaria.csv")

#Look at initial typing and for data entry errors. 
summary(dat)

#Change malaria from numeric to nominal. (Commented out because glm needs 0/1 numerical.)
#dat$malaria = factor(dat$malaria)

#Make behavior an ordinal variable (we don't know if the differences in levels are linear).
dat$behavior = factor(dat$behavior, order = T, levels = c('1', '2', '3', '4', '5'))

#Combine middle 3 factors for more even size of each category. 
levels(dat$behavior) = c('1', '3', '3', '3', '5')

#Make work nominal, assign helpful names.
dat$work = factor(dat$work, labels = c('Unemployed', "Employed", 'Not Working'))

#Remove Nid and district.1 columns (unnecessary)
drop = c('Nid', 'district.1')
dat2 = dat[,!(names(dat) %in% drop)] #New dataframe since we're slicing
summary(dat2)

#Remove row with extremely low stress level observation
dat2 = dat2[dat2$stress != -41.200,]

#Remove row with extremely high insecticide level observation
dat2 = dat2[dat2$insecticide != 129876,]

#Remove row where district = Moon
dat2 = dat2[dat2$district !='9Moon',]

#Change group names in district to make reading easier. 
dat2$district = factor(dat2$district, labels = c('North', "East", 'South'))

#Remove row where health is outside of range.
dat2 = dat2[dat2$health <= 35,]

#Final data summary
summary(dat2)

#Export new csv file
#write.csv(dat2, "malaria_cleaned.csv")

#Label Malaria for plotting ease
#dat2$malaria = factor(dat2$malaria, labels = c('No', 'Yes'))
#Visualisation
library(ggplot2)
library(gridExtra)

#Stress
st = ggplot(dat2, aes(y = stress, x = malaria, fill = malaria)) + geom_boxplot() + ylab('Stress') + coord_flip() + theme(axis.text.y=element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank())

#Insectiside
ins = ggplot(dat2, aes(y = insecticide, x = malaria, fill = malaria)) + geom_boxplot()+ ylab('Insecticide') + coord_flip() + theme(axis.text.y=element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank())

#Source
library(ggmosaic)
sou = ggplot(data = dat2) + geom_mosaic(aes(x = product(malaria, source), fill=malaria)) + xlab('Source') + theme(axis.text.y=element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank())

#Behavior
beh = ggplot(data = dat2) + geom_mosaic(aes(x = product(malaria, behavior), fill=malaria))  + xlab('Behavior')+ theme(axis.text.y=element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank())

#Nettype
net = ggplot(data = dat2) + geom_mosaic(aes(x = product(malaria, nettype), fill=malaria))  + xlab('Nettype') + theme(axis.text.y=element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank())

#District
dis = ggplot(data = dat2) + geom_mosaic(aes(x = product(malaria, district), fill=malaria))  + xlab('District')+ theme(axis.text.y=element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank())

#Health
hea = ggplot(dat2, aes(y = health, x = malaria, fill = malaria)) + geom_boxplot()  + ylab('Health') + coord_flip() + theme(axis.text.y=element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank())

#Work
wo =  ggplot(data = dat2) + geom_mosaic(aes(x = product(malaria, work), fill=malaria))  + xlab('Work') + theme(axis.text.y=element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank())

#Combine plots 
ggarrange(st, ins, sou, beh, net, dis, hea, wo, nrow = 4, ncol = 2, common.legend = T, legend = 'bottom')

#Transform?
#slicing and dicing for transformed insecticide
insect.transf = (dat2$insecticide)#transformed insect vector
breaks = quantile(insect.transf, probs = seq(0,1,.1)) #break by 10, 20, 30... percentiles to ensure equil group sizes in each bin
insect.transf.fac = factor(cut(insect.transf, breaks = breaks)) #split transformed insect vector into groups.

eprobs.tr = tapply(dat2$malaria, insect.transf.fac, mean) #calculate p(malaria) for each percentile bin
elogits.tr = log(eprobs.tr / (1-eprobs.tr)) #log odds for each percentile bin (this is what should be linear.)

slice.tr = tapply(insect.transf, insect.transf.fac, mean) #Find the mean of each percentile bin to plot on x axis. 

insect_glm.tr = glm(dat2$malaria ~ insect.transf, family="binomial") #Fit transformed glm model.

#Compare probabilities to probability curve. NOT what we are interested in. 
#plot(insect.transf, insect_glm.tr$fitted.values, ylim=c(0,1))
#points(slice.tr, eprobs.tr, col="blue")

#plot log-odds from fitted model and sample calculated log odds
plogits.tr = log(insect_glm.tr$fitted.values/(1-(insect_glm.tr$fitted.values)))
plot(insect.transf, plogits.tr, ylim = c(-2,0),xlim= c(0,500), pch = 16, main="Log-Odds of Insecticide", xlab = "Insecticide", ylab = "Log-Odds")
points(slice.tr, elogits.tr, col="blue", pch = 16)


#slicing and dicing for sqrt transformed insecticide (just to check. )
insect.transf = (dat2$insecticide)^(1/2) #transformed insect vector
breaks = quantile(insect.transf, probs = seq(0,1,.1)) #break by 10, 20, 30... percentiles to ensure equil group sizes in each bin
insect.transf.fac = factor(cut(insect.transf, breaks = breaks)) #split transformed insect vector into groups.

eprobs.tr = tapply(dat2$malaria, insect.transf.fac, mean) #calculate p(malaria) for each percentile bin
elogits.tr = log(eprobs.tr / (1-eprobs.tr)) #log odds for each percentile bin (this is what should be linear.)

slice.tr = tapply(insect.transf, insect.transf.fac, mean) #Find the mean of each percentile bin to plot on x axis. 

insect_glm.tr = glm(dat2$malaria ~ insect.transf, family="binomial") #Fit transformed glm model.

#Compare probabilities to probability curve. NOT what we are interested in. 
#plot(insect.transf, insect_glm.tr$fitted.values, ylim=c(0,1))
#points(slice.tr, eprobs.tr, col="blue")

#plot log-odds from fitted model and sample calculated log odds
plogits.tr = log(insect_glm.tr$fitted.values/(1-(insect_glm.tr$fitted.values)))
plot(insect.transf, plogits.tr, ylim = c(-2,1),xlim= c(0,21), pch = 16, main="Predicted and Empirical Log-Odds", xlab = "Insecticide^(1/2)", ylab = "Log-Odds")
points(slice.tr, elogits.tr, col="blue", pch = 16)

###FOR STRESS
insect.transf = (dat2$stress) 
breaks = quantile(insect.transf, probs = seq(0,1,.1)) 
insect.transf.fac = factor(cut(insect.transf, breaks = breaks)) 

eprobs.tr = tapply(dat2$malaria, insect.transf.fac, mean) 
elogits.tr = log(eprobs.tr / (1-eprobs.tr))

slice.tr = tapply(insect.transf, insect.transf.fac, mean) 

insect_glm.tr = glm(dat2$malaria ~ insect.transf, family="binomial") 

plogits.tr = log(insect_glm.tr$fitted.values/(1-(insect_glm.tr$fitted.values)))
plot(insect.transf, plogits.tr, ylim = c(-3,1), xlim = c(-1,20),pch = 16, main="Log-Odds of Stress", xlab = "Stress", ylab = "Log-Odds")
points(slice.tr, elogits.tr, col="blue", pch = 16)
legend(legend = c("Predicted", "Emperical"), col = c('black', 'blue'), pch = c(16,16), 'topleft')

###FOR HEALTH
insect.transf = (dat2$health) 
breaks = quantile(insect.transf, probs = seq(0,1,.1))
insect.transf.fac = factor(cut(insect.transf, breaks = breaks)) 

eprobs.tr = tapply(dat2$malaria, insect.transf.fac, mean) 
elogits.tr = log(eprobs.tr / (1-eprobs.tr)) 

slice.tr = tapply(insect.transf, insect.transf.fac, mean) 

insect_glm.tr = glm(dat2$malaria ~ insect.transf, family="binomial") 

plogits.tr = log(insect_glm.tr$fitted.values/(1-(insect_glm.tr$fitted.values)))
plot(insect.transf, plogits.tr,pch = 16, ylim = c(-2,0), main="Predicted and Empirical Log-Odds", xlab = "Health", ylab = "Log-Odds")
points(slice.tr, elogits.tr, col="blue", pch = 16)
legend(legend = c("Predicted", "Emperical"), col = c('black', 'blue'), pch = c(16,16), 'topleft')


#1 level Contingency Tests
summary(glm(malaria ~stress, data = dat2, binomial)) #Significant
summary(glm(malaria ~insecticide, data = dat2, binomial)) #Significant 
chisq.test(x = dat2$source, y = dat2$malaria) # Not significant
chisq.test(x = dat2$district, y = dat2$malaria) #Significant
chisq.test(x = dat2$behavior, y = dat2$malaria)  #Not Significant 
chisq.test(x = dat2$nettype, y = dat2$malaria)  # Significant 
chisq.test(x = dat2$work, y = dat2$malaria)  #Not Significant 
summary(glm(malaria ~health, data = dat2, binomial)) # Not Significant 

#Check associations between variables.
summary(lm(dat2$stress~ dat2$insecticide))
summary(aov(stress~source, dat2))
summary(aov(stress~behavior, dat2))
summary(aov(stress~nettype, dat2)) #Significant!
summary(aov(stress~district, dat2))
summary(lm(dat2$stress ~ dat2$health))
summary(aov(stress~work, dat2)) #Significant! 

summary(aov(insecticide~source, dat2))
summary(aov(insecticide~behavior, dat2))
summary(aov(insecticide~nettype, dat2)) 
summary(aov(insecticide~district, dat2))
summary(lm(dat2$insecticide~dat2$health))
summary(aov(insecticide~work, dat2))

chisq.test(dat2$source, dat2$behavior)
chisq.test(dat2$source, dat2$nettype)
chisq.test(dat2$source, dat2$district)
summary(aov(health~source, dat2)) #Significant
chisq.test(dat2$source, dat2$work)

chisq.test(dat2$behavior, dat2$nettype)
chisq.test(dat2$behavior, dat2$district)
summary(aov(health~behavior, dat2))
chisq.test(dat2$behavior, dat2$work)

chisq.test(dat2$nettype, dat2$district)
summary(aov(health~nettype, dat2))
chisq.test(dat2$nettype, dat2$work)

summary(aov(health~district, dat2))
chisq.test(dat2$district, dat2$work)

summary(aov(health~work, dat2))

##Models
#Backward selection from full model. No interactions
full = glm(malaria~(.), data = dat2, binomial)
summary(full)
library(MASS)
step.model <- stepAIC(full, direction = "backward", 
                      trace = TRUE)
summary(step.model)

#Backward selection with interactions 
full2 = glm(malaria~(.)^2, data = dat2, binomial)
summary(full2)
step.model2 <- stepAIC(full2, direction = "backward", 
                      trace = FALSE)
summary(step.model2)

#Alternative models (All worse!)
m2 = glm(malaria~stress + insecticide  + district,data = dat2, binomial)
summary(m2)
m3 = glm(malaria~ insecticide + nettype + district,data = dat2, binomial)
summary(m3)
m3 = glm(malaria~ stress + insecticide + nettype + district + health*insecticide,data = dat2, binomial)
summary(m3)
#And many other manual checks to make sure nothing was omitted. 


#Final model 
m1 = glm(malaria~stress + insecticide + nettype + district,data = dat2, binomial)
summary(m1)

#Contingency Tables and ROC curve. 
y = dat2$malaria
yprobs = m1$fitted.values
yhat = as.numeric(yprobs > 0.332)
x = table(y,yhat)
x
summary(factor(y))
library(pROC)
plot.roc(y, yprobs, print.auc=TRUE, 
         main="ROC Curve", ylim = c(0,1))
par(mfrow = c(1,1))