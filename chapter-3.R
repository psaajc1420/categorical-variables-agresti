# Clearing the screen and work environment
rm(list = ls())
cat("\014")

# Set the working directory
# setwd("/home/jacob/Documents")

library(ggplot2)

gforces <- read.table("gforces.txt", header = TRUE)

pglm<-glm(Signs~Age, data =gforces)
summary(pglm)
plot(x = gforces$Age, y=gforces$Signs)
abline(pglm)
y1<-predict(pglm) 


#probit model
prlm<-glm(Signs ~ Age, data=gforces, family = binomial(link="probit"))
summary(prlm)

y1hat<-predict(prlm)
y2<-pnorm(yhat)

#logit model
lglm<-glm(Signs ~ Age, data=gforces, family = binomial(link="logit"))
summary(lglm)
y2hat<-predict(lglm)
y3<-exp(y2hat)/(1+exp(y2hat))
