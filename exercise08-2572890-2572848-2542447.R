### Stats with R Exercise sheet 8

##########################
#Week9: Checking Assumptions underlying ANOVA and linear regression
##########################


## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Sunday, December 16. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via moodle.
## You are allowed to work together in group up to three students, but everybody 
## needs to submit the group version of the homework via moodle.
## You need to provide a serious attempt to each exercise in order to have
## the assignment graded as complete.

## Please write below your (and your teammates) name, matriculation number. 
## Name:
## Matriculation number:

#Muhammad Hamza jamil  2572890
#Fahad Aslam   2572848
#Thorsten Schamper 2542447

## Change the name of the file by adding your matriculation numbers
## (exercise0N_firstID_secondID_thirdID.R)

###############################################################################
###############################################################################

########
### Exercise 1
########

########
### Please, use ggplot to make plots in all exercises below!
########

library(reshape)
library(languageR)
library(ggplot2)
# a) Read in the data kidiq.txt (available in the Moodle) and take a look
#    at the data summary. It contains information about the mum's iq and 
#    their child's iq. 
#    mom_hs indicates whether the mother has a high school degree
#    1= high school education, 0= no high school degree.
data<-read.table("kidiq.txt")
summary(data)
# b) Plot kid_score against mom_iq in a scatter plot, and add a regression line 
#    (kid_score should be the response variable and mom_iq the predictor 
#    variable) to the plot. 
#    Name the plot and the axis in sensible ways.

ggplot(data, aes(x=mom_iq,y=kid_score))+geom_point()+
  geom_smooth(method="lm",se=FALSE)+ggtitle("Kid Score vs Mom IQ")+labs(y="Kid Score",x="Mom IQ")

# c) Calculate a simple regression model for kid_score with mom_hs as a 
#    predictor and interpret the results.

model1 <- lm(kid_score~mom_hs, data, na.action = na.omit)
summary(model1)


# d) Next, fit a regression model with two predictors: mom_hs and mom_iq. 
#    Interpret the model and compare to the previous model.
model2 <- lm(kid_score~mom_hs+mom_iq, data, na.action = na.omit)
summary(model2)


# e) Now plot a model where both predictors are shown. Do this by plotting 
#    data points for mothers with high school degree==1 in one color and those 
#    without one in another color. Then also fit two separate regression lines such 
#    that these lines reflect the model results.
#	   HINT: One solution in ggplot is to calculate fitted values of the regression model 
#    and then plot them along with the original data points:
#    pred = data.frame(mom_iq=kidiq$mom_iq, mom_hs=kidiq$mom_hs, kid_score_pred=fitted(your_model))

###REFERENCE:::https://github.com/ericyewang/Duke-STA-210/blob/master/Lab5_R_script_full.R

ggplot(data, aes(x=mom_iq, y=kid_score, color= factor(mom_hs)))+geom_point()+
  scale_color_manual(guide=guide_legend(title="mom_hs"), values=c("red","green"))+
  geom_abline(intercept=model2$coefficients[1], slope=model2$coefficients[3], color="red")+
  geom_abline(intercept=model2$coefficients[1]+model2$coefficients[2], slope=model2$coefficients[3], color="green")


# f) Next, we will proceed to a model including an interaction between mom_hs
#    and mom_iq. Interpret your results.

model3 <- lm(kid_score~mom_hs+mom_iq+mom_hs:mom_iq, data, na.action = na.omit)
summary(model3)

# g) Next, let's plot the results of this model.
ggplot(data, aes(x=mom_iq, y=kid_score, color= factor(mom_hs)))+geom_point()+
  scale_color_manual(guide=guide_legend(title="mom_hs"), values=c("blue","red"))+
  geom_abline(intercept=model3$coefficients[1], slope=model3$coefficients[3], color="blue")+
  geom_abline(intercept=model3$coefficients[1]+model3$coefficients[2], slope=model3$coefficients[3], color="red")


# h) Next, let's explore the "predict.lm" function. Please first generate
#    a new dataframe with one datapoint (a mother with high school degree
#    and iq of 100). Then, use the predict function to predict the child's iq. 
#    Please specify the predict function to also give you the 0.95 confidence 
#    interval.


frame_new <- data.frame(mom_hs=1, mom_iq=100)
predict(model3, frame_new, interval = "confidence", level=0.95)
#      fit      lwr      upr
# 88.24766 86.31365 90.18167
predict(model1, frame_new, interval = "confidence", level=0.95)
#fit      lwr      upr
# 89.31965 87.20662 91.43268

# i) Meaning of confidence intervals for regression line.
#    Let's go back to the exercise b) and plot again the data points with the 
#    regression line. By default, there should also be displayed the borders of 
#    the confidence interval. What is the meaning of the confidence interval?

model <- lm(kid_score~mom_iq, data, na.action = na.omit)
min_iq <- floor(min(data$mom_iq))-1
max_iq <- ceiling(max(data$mom_iq))+1

data_frame_2 <- data.frame(seq(min_iq, max_iq, 1))
names(data_frame_2)[names(data_frame_2) == "seq.min_iq..max_iq..1."] <- "mom_iq"

predicted <- predict(model, newdata=data_frame_2, interval = "confidence", level=0.95)

df_2 <- cbind(data_frame_2, predicted)

ggplot(df_2, aes(x=mom_iq, y=fit))+geom_point()+geom_smooth(method="lm", se=FALSE)+
  geom_line(aes(x=mom_iq,y=lwr), color="green", linetype = "dashed")+
  geom_line(aes(x=mom_iq,y=upr), color="blue", linetype = "dashed")

# j) Finally, do model checking on your model with the interaction, i.e. inspect 
#    the standard model plots provided by R, and interpret what you see.
interactions <- lm(data$kid_score ~ data$mom_iq + data$mom_hs +(data$mom_iq*data$mom_hs))
summary(interactions)
par(mfrow=c(2,2))
plot(interactions)
#We see the residuals, leverage, and outliers by plotting the model
#The variable mom_hs has the most affect on the predicted values. SO it is the main predictor
#variable

