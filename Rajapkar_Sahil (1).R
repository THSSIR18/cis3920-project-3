# HW 3 Sahil Rajapkar CIS 3920 - NFA
# Getting Started
# ISLR installation----
install.packages("ISLR")
library(ISLR) #Variable descriptions

??College

str(College) #Structure of the dataset

College$Private = as.numeric(College$Private)

#Question 1a & 1b & 1c----
plot(Grad.Rate~S.F.Ratio,data=College,xlab='S.F.Ratio',ylab='Grad Rate',main='Grad vs S.F.Ratio')
model = lm(Grad.Rate~S.F.Ratio,data=College)
abline(model, col = "red")
summary(model)

par(mfrow=c(1,1))
plot(model)

confint(model)

#Question 1d----
plot(Grad.Rate~I(S.F.Ratio^2)+S.F.Ratio,data=College,xlab='S.F.Ratio',ylab='Grad Rate')
q1b_model = lm(Grad.Rate~I(S.F.Ratio^2)+S.F.Ratio,data=College)
summary(q1b_model)

par(mfrow=c(1,1))
plot(q1b_model)

confint(q1b_model)
#Based on where all the data is, a linear fit is more better for the graph

#Question 2a----
q2_model = lm(Grad.Rate~Private+Top25perc+Outstate+Room.Board,data=College)
summary(q2_model)
#Question2b----
plot(Grad.Rate~Private,data=College,xlab='Grad.Rate',ylab='Schools')

#Question2c----
confint(q2_model)

#Question2d----
Outstate = c(25000)
Room.Board = c(4000)
Top25perc = c(55)
Private = c(1)

new_obs = data.frame(Private,Top25perc,Outstate,Room.Board)
new_obs

predict(q2_model,new_obs)
predict(q2_model,new_obs,interval='prediction')

#Question2e----
par(mfrow=c(2,2))

plot(q2_model)

#Question3a----
q3_model = lm(Grad.Rate~.,data=College)
summary(q3_model)

#Question3b & 3d----
install.packages("leaps")
library(leaps)

model_fwd = regsubsets(Grad.Rate~., data=College,
                       nvmax=NULL, method="forward")

summary(model_fwd)

plot(model_fwd, scale="adjr2", main="Forward Selection: AdjR2")
par(mfrow=c(1,1))

model_fwd_summary = summary(model_fwd) #Store summary output
which.max(model_fwd_summary$adjr2) #Display best subset by adjr2

summary(model_fwd)$which[13,]

#Question3c----
best_model_fwd = lm(Grad.Rate~Private+Apps+Top10perc+Top25perc+F.Undergrad+P.Undergrad
                    +Outstate+Room.Board+Personal+perc.alumni+Expend, data=College)

summary(best_model_fwd)
