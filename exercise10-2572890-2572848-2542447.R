### Stats with R Exercise sheet 10

##########################
#Week 11: Model Selection, Transformations, Power
##########################


## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Sunday, January 20. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via moodle.
## You are allowed to work together in group up to three students, but everybody 
## needs to submit the group version of the homework via moodle.


## Please write below your (and your teammates) name, matriculation number. 
## Name:
## Matriculation number:

#Muhammad Hamza jamil  2572890
#Fahad Aslam   2572848
#Thorsten Schamper 2542447

## Change the name of the file by adding your matriculation numbers
## (exercise0N_firstID_secondID_thirdID.R)

###########################################################################################
###########################################################################################


#Play around with the simulation code. The code simulates how a dataset may be generated. 
#The advantage over using a real data set is that we know exactly how the data was generated, 
#and can observe whether the model manages to correctly identify the original model structure and coefficients.
# IMPORTANT! Run each model simulation code several times to see how stable the results are -- this is necessary
#because we are sampling the data randomly, so it could be that we sometimes get more or less "lucky" draws.

library(lme4)
library(car)

n <- 200 # number of observations to be simulated
predA <- rnorm(n, 100, 20)
predB <- rnorm (n, 60, 30)
interact <- 0.002*(predA*predB)
error <- rnorm (n, 0, 20)

resp <- 25 + predA + 1.2*predB - interact + error
d <- data.frame(predA, predB, resp)

# 1. Write down what values you would hope for the model to estimate in the ideal case:
## a)intercept= 25
## b)predA= 1
## c)predB= 1.2
## d)predA:predB = 0

 m1<- lm(resp~predA*predB, data=d)
# Ignore the warning message about rescaling for now, we'll get to that below.
summary(m1)  
# 2. Can the model recover the original model structure and estimate correct coefficients for the predictors?
## Yes running multiple simulations we see that the coefficients are kind of the same.
# 3. What happens if you change the number of subjects?
## with more subjects we can see that the value for intercept is either too high or too low.
## the other values are still rather good.
# 4. What happens if you change the variance of the error term?
## If we increase the variance of the error value by a significant amount, like 100 for example, we will see that the error is
## too big and we can no longer recover the original. If we choose a small variance for the error we have the
## exact recover of the original.
# 5. What happens if you change the effect sizes?
## Neither an increase nor a decrease of the effect size will produce a good recovery of the original model.

# Next, we want to observe the effect of scaling the predictors. 
# By hand, we could do: normpredA <- (predA - mean(predA)) / sd(predA)
# this is the same as calling "normpredA <- scale(predA)"
# we can do this for the whole data frame:
nd <- as.data.frame(scale(d))
plot(d)
plot(nd)
sm1<- lm(resp~predA*predB, data=nd)
summary(sm1)
vif(m1)
vif(sm1)
# 6. Are the predictors currently correlated? What does the vif value mean?
## no the two predictor are not correlated at all.
## the VIF tells us how much higher the variance are if predA and predB are correlated
## compared to when they are not. In our case we see that the scaled model has rather high
## variance when correlated.

# 7. Check whether normalization also has a large effect when there is no interaction present in the model
sm2<- lm(resp~predA+predB, data=nd)
m2<- lm(resp~predA+predB, data=d)
summary(sm2)
summary(m2)
vif(m2)
vif(sm2)
## in terms of the variance inflating factor there is now no difference between the scaled and the non-scaled model

# 8. Try out what happens if there was originally no interaction in the data.
mni<- lm(resp~predA+predB, data=d)
summary(mni)
## without the interaction between the predictors we still get a rather good recovery of the coefficients.

#Next, we want to calculate interpretable estimates

names(sm1)
coef(sm1)

 denormPredA <- coef(sm1)[2] *sd(d$resp)/ sd(d$predA) 
 # normpredA <- (predA - mean(predA)) / sd(predA)
 denormPredA
#    predA 
# 0.9635143 
 
# 10. Explain in your own words, why the denormalization for predictor A works this way.
 
 #Dividing the sd of the response by the sd of the predictor it gave us factor
 # that will leads the normalized value towards original value of curve.

denormPredB <- coef(sm1)[3] * sd(d$resp)/ sd(d$predB)
 denormPredB
# expected: 1.2

 denormIntercept<-coef(sm1)[1] * sd(d$resp)+mean(d$resp)-
 (denormPredA*mean(d$predA) + denormPredB* mean(d$predB))
 denormIntercept
# expected: 25

denormInteract <- coefficients(sm1)[4] / (sd(d$predA)*sd(d$predB)) * sd(d$resp)
denormInteract


# Next, we create correlated variables 
n <- 200 # number of observations to be simulated
predA <- rnorm(n, 100, 20)
predB<- rnorm (n, 60, 30)
predC <- -1* predA + rnorm(n,0,10)
error <- rnorm (n, 0, 30)
respcor <- 25 + predA + 3* predB+ 2*predC - (0.02*(predA*predC)) + error
d2<-data.frame(predA, predB, predC, respcor)
summary(lm(respcor ~ predA * predC + predB, data=d2))

sd2 <-as.data.frame(scale(d2))
summary(lm(respcor ~ predA * predC + predB, data=sd2))

# 11. What do you observe regarding the results from the models? Do the models obtain the same or different results 
# with / without normalization?
## Both models have the same residuals(but scaled) as well as standard error and R-squared. But the coefficients are different.

# 12. Denormalize the coefficients.
model<-lm(respcor ~ predA * predC + predB, data=d2)
denorm_PredA <- coef(model)[2] *sd(d2$respcor)/ sd(d2$predA) 
denorm_PredA
#predA 
#5.837491
denorm_PredB <- coef(model)[4] *sd(d2$respcor)/ sd(d2$predB) 
denorm_PredB
#predB 
#12.72507
denorm_PredC <- coef(model)[3] *sd(d2$respcor)/ sd(d2$predC) 
denorm_PredC
#predC 
#10.15479
denorm_Interact <- coefficients(model)[5] / (sd(d2$predA)*sd(d2$predC)) * sd(d2$respcor)
denorm_Interact
#predA:predC 
#-0.004505686
denorm_Intercept<-coef(model)[1] * sd(d2$respcor)+mean(d2$respcor)-
  (denorm_PredA*mean(d2$predA) + denorm_PredB* mean(d2$predB) + denorm_PredC* mean(d2$predC))
denorm_Intercept
#(Intercept) 
#1787.861 
 
# Finally, we will generate repeated measures!
# For this, we will use the dataframe d; for simplicity of interpretation, we will do no normalization here.
n<-400
predA <- rnorm(n, 100, 20)
predB <- rnorm (n, 60, 30)
error <- rnorm (n, 0, 30)
subjno <- 20 #number of subjects; 
itemno<- 20 # number of items;
subj<-as.factor(rep(1:subjno,itemno))
item<-as.factor(rep(1:itemno, each=subjno))
lmerd <- data.frame(predA, predB, resp, subj, item)
# basic data frame done; now on to by subject and by item random effects:
subjid<-as.factor(1:subjno)
subjint<- rnorm(subjno, 0, 8)
subjeffdf<-data.frame(subjid, subjint)
itemid<-as.factor(1:itemno)
itemint<-rnorm(itemno, 0, 4)
itemeffdf<-data.frame(itemid, itemint)
newd <- merge(lmerd, itemeffdf, by.x="item", by.y="itemid")
newd<- merge(newd, subjeffdf, by.x="subj", by.y = "subjid")
lmerd <- newd
# add by-subject and by-item effects here!
lmerd$respr <- 25+newd$subjint + newd$itemint + newd$predA + 1.2*newd$predB + error

m0<-lm(respr ~ predA + predB , data=lmerd)
summary(m0)
m1<-lmer(respr ~ predA + predB + (1|subj) + (1| item), data=lmerd)
summary(m1)

lmerd$resp <- 25 + newd$predA + 1.2*newd$predB + error
m2<-lmer(resp ~ predA + predB + (1|subj) + (1| item), data=lmerd)
summary(m2)

#13. Explain the difference between models m0 m1 and m2

#in model m0, we used fixed effect predictors,
#in m1 we used fix and random predictors, 
#in m2 we used both as well as scaled value of the response to construct the model

#14. Play around with the size of the by item and by subject effects (here: intercepts only)


#15. Generate the data such that subjects differ in terms of how much predictor A affects them.
n <- 200 # number of observations to be simulated
predA <- rnorm(n, 100, 20)
predB <- rnorm (n, 60, 30)
interact <- 0.002*(predA*predB) 
error <- rnorm (n, 0, 30)
resp <- 25 + predB + 1.2*predA - interact + error
# then build a mixed effects model that includes a random slope for subjects.
mem<-lmer(resp ~ predA+ (1|subj) , data=lmerd)

#16. Then build a mixed effects model that includes a random slope for subjects.

mem<-lmer(resp ~ predA+ (1|subj) , data=lmerd)
summary(mem)


