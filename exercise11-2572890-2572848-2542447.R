##########################
#Week 12: Model Families and Logistic Regression
##########################


## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Sunday, January 27. Write the code below the questions. 
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

######################################################################################################################


####
#Part 1
####
# The folder speed.dating
# contains data from an experiment on a few hundred students that randomly assigned
# each participant to 10 short dates with participants of the opposite sex
# (Fisman et al., 2006). For each date, each person recorded several subjective
# numerical ratings of the other person (attractiveness, compatibility, and some 
# other characteristics) and also wrote down whether he or she would like to meet
# the other person again. Label and rij1, . . . , rij6 as person i’s numerical ratings of person j 
# on the dimensions of attractiveness, compatibility, and so forth.



#(1) Fit a classical logistic regression predicting Pr(yij = 1) given person i's 
#    ratings of person j. For ratings, use the features attr, sinc, intel, fun; see the documentation for what exactly these
#    abbreviations stand for.
#    Also, please plot the data in order to inspect it, and discuss the importance of attractiveness, compatibility, and so 
#    forth in this predictive model.
DataFrame <- read.csv('SpeedDatingData.csv',header=T,na.strings=c(""))

model1 <- glm(dec~attr+sinc+intel+fun, data=DataFrame, family="binomial")
summary(model1)

library(ggplot2)
ggplot(aes(y =attr,factor(dec)),data=DataFrame)+ geom_boxplot()+labs(title = "Attractiveness Plot")
ggplot(aes(y =fun, factor(dec)),data=DataFrame)+  geom_boxplot()+labs(title = "Fun plot")
ggplot(aes(y =intel,factor(dec)),data=DataFrame)+ geom_boxplot()+labs(title = "interlligence plot")
ggplot(aes(y =sinc,factor(dec)),data=DataFrame)+geom_boxplot()+labs(title = "Sincerity plot")


#(2) Expand this model to allow varying intercepts for the persons making the
#    evaluation; that is, some people are more likely than others to want to meet
#    someone again. Discuss the fitted model.
library(lme4)
model2 <- glmer(dec ~ attr+sinc+intel+fun+(1|iid),data=DataFrame, family = "binomial", glmerControl(optimizer = "bobyqa"))
summary(model2)
#Predictors are not much correlated with each other.
#AIC is 6703.3  is lower and it is comparitively better model than a

#(3) Expand further to allow varying intercepts for the persons being rated. Discuss
#    the fitted model.
model3 <- glmer(dec ~ attr+ sinc + intel + fun + (1|pid)+ (1|iid), data = DataFrame, family = "binomial",glmerControl(optimizer = "bobyqa"))
summary(model3)
#AIC is 6664.0 is lower and it is comparitively better model than 2
#Predictors are not much correlated with each other.


#(4) Now fit some models that allow the coefficients for attractiveness, compatibility, and the 
#    other attributes to vary by person.  Fit a multilevel model, allowing the intercept and the 
#    coefficients for the 6 ratings to vary by the rater i. (Hint: The model will not converge when you 
#    include many predictors as random slopes; see with how many predictors you can get the model to converge;
#    and try out some of the tricks we have seen to see whether they affect convergence for this dataset.)
model4 <- glmer(dec ~ attr + sinc+ intel+ fun + like +prob + (1+attr|iid), data = DataFrame, family = 'binomial', glmerControl(optimizer = "bobyqa"))
summary(model4)

#(5) compare the output for the different models that you calculated - did the model design affect your conclusions?


####
#Part 2
####

# In this example, num_awards is the outcome variable and indicates the number of awards earned by students at
# a high school in a year, math is a continuous predictor variable and represents students' scores on their 
# math final exam, and prog is a categorical predictor variable with three levels indicating the type of program 
# in which the students were enrolled. It is coded as 1 = "General", 2 = "Academic" and 3 = "Vocational". 
# Let's start with loading the data and looking at some descriptive statistics.

p = read.csv("poisson_sim.csv", sep=";")
p <- within(p, {
  prog <- factor(prog, levels=1:3, labels=c("General", "Academic", "Vocational"))
  id <- factor(id)
})
summary(p)

#(6) Plot the data to see whether program type and math final exam score seem to affect the number of awards.
plot(p$math, p$num_awards)
plot(p$prog, p$num_awards)
## we can see that there are way more awards for people in the academy program.
## There is also clearly visible that people with higher math score in their finals get more awards.

#(7) Run a generalized linear model to test for significance of effects.
mod <- glm(data = p, formula = num_awards ~ prog + math, family = poisson)
mod_prog <- glm(data = p, formula = num_awards ~ prog, family = poisson)
mod_math <- glm(data = p, formula = num_awards ~ math, family = poisson)
summary(mod)
summary(mod_prog)
summary(mod_math)

#(8) Do model comparisons do find out whether the predictors significantly improve model fit.
anova(mod_prog, mod)
anova(mod_math, mod)
anova(mod_prog, mod_math)

#(9) Compare to a model that uses a gaussian distribution (normal lm model) for this data.
mod_lm <- glm(num_awards ~ math + prog, data = p, family = "gaussian")
summary(mod_lm)
#AIC is very large for this model as  variables are not be treats like normal distribution.