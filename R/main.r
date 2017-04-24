rm(list=ls())

# read the data into R
polls <- read.csv(file="2016-polls-only.csv", header=TRUE)
results <- read.csv(file="2016-results.csv", header=TRUE)

# data clean. remove the pollster that conduct less than 5 polls.
tmp <- as.data.frame(table(polls$Pollster))
postscript(file='conducted_number.eps', width=7.35, height=6.75, horizontal=FALSE, onefile=FALSE, paper="special")
par(mar = c(4.1,4.3,0.3,0.3), mgp = c(2.6,1,0))
barplot(table(tmp$Freq), xlab="Number of conducted polls", ylab="Frequency", cex.lab=1.8, cex.axis=1.5, cex.names=1.5)
dev.off()

pollers <- names(table(polls$Pollster)[table(polls$Pollster) >= 10])
subpollsID <- polls[,5]%in%pollers
polls_sub <- polls[subpollsID,]
polls_sub$Pollster <- as.factor(as.character(polls_sub$Pollster))
# postscript(file='conducted_number_2.eps', width=7.35, height=6.75, horizontal=FALSE, onefile=FALSE, paper="special")
# par(mar = c(4.1,4.3,1.0,0.3), mgp = c(2.6,1,0))
# barplot(table(polls_sub$Pollster), cex.names=NULL, cex.lab=1.8, cex.axis=1.5 )
# dev.off()

## reformating the 2008 poll and true results data set as desired
# Q2
winers <- as.integer(results[,2]-results[,3]>0)
StateID <- results[,1]
Allresponses <- NULL
for (sid in 1:51){
  polls_substate <- polls_sub[polls_sub$State==StateID[sid],]
  pollwinerstate <- as.integer(polls_substate[,2]-polls_substate[,3]>0)
  pollwinersIND <- as.integer(pollwinerstate==winers[sid])
  Allresponses <- c(Allresponses,pollwinersIND)
}
# define state edges.
margins <- abs(polls_sub[,2]-polls_sub[,3])
lagtime <- rep(0,dim(polls_sub)[1])
electiondate <- c("Nov 08 2016")
for (i in 1:dim(polls_sub)[1]){
  lagtime[i] <- as.Date(electiondate, format="%b %d %Y")-as.Date(as.character(polls_sub[i,4]), format="%b %d %Y")
}

resp <- Allresponses
statesFAC <- polls_sub[,1]
pollersFAC <- polls_sub[,5]

# formalized the data
pollsdata <- data.frame(resp=resp, statesFAC=statesFAC, margins=margins,lagtime=lagtime,pollersFAC=pollersFAC)

contrasts(pollsdata$statesFAC)
contrasts(pollsdata$pollersFAC)

# split the data into two chunks: training and testing set.

set.seed(101) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 75% of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(pollsdata), size = floor(.80*nrow(pollsdata)), replace = F)
train <- pollsdata[sample, ]
test  <- pollsdata[-sample, ]

#Now, let’s fit the model. Be sure to specify the parameter family=binomial in the glm() function.
logit_full <- glm(resp~statesFAC+margins+lagtime+pollersFAC, data=train, family=binomial(link="logit"))
summary(logit_full)

# anova
anova(logit_full, test="Chisq")
anova(logit_full, test="LRT")

# model selection
logit_null <- glm(resp~1,data=train, family=binomial(link="logit"))
step(logit_null, scope=list(lower=logit_null, upper=logit_full), direction="both")
step(logit_null, scope=list(lower=logit_null, upper=logit_full), direction="forward")
step(logit_full, direction="backward")

# anova compare
#From Wald's type test lagtime is not stat signif. variable.
logit_reg <- glm(resp~statesFAC+margins+pollersFAC, data=train, family=binomial(link="logit"))

# goodness of fit
anova(logit_reg, logit_full, test="Chisq")
pval <- 1-pchisq(0.025096, df=1)

logit_reg <- glm(resp~statesFAC+margins+pollersFAC, data=train, family=binomial(link="logit"))
logit_reg1 <- glm(resp~margins+pollersFAC, data=train, family=binomial(link="logit"))
anova(logit_reg1, logit_reg, test="Chisq")
logit_reg2 <- glm(resp~statesFAC+pollersFAC, data=train, family=binomial(link="logit"))
anova(logit_reg2, logit_reg, test="Chisq")
logit_reg3 <- glm(resp~statesFAC+margins, data=train, family=binomial(link="logit"))
anova(logit_reg3, logit_reg, test="Chisq")

# summary
summary(logit_reg)

## Deviance residual
postscript(file='deviance.eps', width=7.35, height=6.75, horizontal=FALSE, onefile=FALSE, paper="special")
par(mar = c(4.1,4.3,0.3,0.3), mgp = c(2.6,1,0))
Devianceres <- residuals(logit_reg, type = "deviance")
plot(fitted(logit_reg), Devianceres, xlab=expression(pi[i]), ylab = "Deviance Residuals", ylim = c(-3.5,3.5), cex=1.5, cex.lab=1.8, cex.axis=1.5)
abline(h=3,lty=2, lwd=3, col="grey")
abline(h=-3,lty=2, lwd=3, col="grey")
dev.off()

## Pearson residual
postscript(file='pearson.eps', width=7.35, height=6.75, horizontal=FALSE, onefile=FALSE, paper="special")
par(mar = c(4.1,4.3,0.3,0.3), mgp = c(2.6,1,0))
Pearsonres <- residuals(logit_reg, type = "pearson")
plot(fitted(logit_reg), Pearsonres, xlab=expression(pi[i]), ylab = "Pearson Residuals", cex=1.5, cex.lab=1.8, cex.axis=1.5)
abline(h=3,lty=2, lwd=2, col="grey")
abline(h=-3,lty=2, lwd=2, col="grey")
dev.off()

# Assessing the predictive ability of the model
fitted.results <- predict(logit_reg,newdata=test,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test$resp)
print(paste('Accuracy',1-misClasificError))

# 
library(ROCR)
p <- predict(logit_reg, newdata=test, type="response")
pr <- prediction(p, test$resp)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
postscript(file='roc.eps', width=7.35, height=6.75, horizontal=FALSE, onefile=FALSE, paper="special")
par(mar = c(4.1,4.3,0.3,0.3), mgp = c(2.6,1,0))
plot(prf@x.values[[1]], prf@y.values[[1]], xlab=prf@x.name, ylab=prf@y.name, type='l', lwd=3, cex=1.8, cex.lab=1.8, cex.axis=1.5)
dev.off()

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

# K-fold validation
library('caret')
ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)
mod_fit <- train(as.factor(resp)~statesFAC+margins+pollersFAC, data=train, method="glm", family=binomial(link="logit"), trControl = ctrl)
pred <- predict(mod_fit, newdata=test)
confusionMatrix(data=pred, test$resp)
