### Stats with R Exercise sheet 4

##########################
#Week5: Tests for Categorial Data
##########################

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Sunday, November 18. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via moodle.
## You are allowed to work together in group up to three students, but everybody 
## needs to submit the group version of the homework via moodle.


## Please write below your (and your teammates) name, matriculation number. 
## Name:
## Matriculation number:
#2572890
#2572848
#2542447
## Change the name of the file by adding your matriculation numbers
## (exercise0N_firstID_secondID_thirdID.R)

#################################################################################
#################################################################################

##########
##Exercise 1. Binomial distribution
##########
## Suppose there are 12 multiple choice questions in a quiz. 
## Each question has 5 possible answers, and only one of them is correct. 

## a) Please calculate the probability of getting exactly 4 answers right 
##    if you answer by chance. Calculate this using the dbinom() function.

dbinom(4, size=12, prob=0.2)
#[1] 0.1328756

## b) Next please calculate the probability of answering 4 or less questions 
##    correctly by chance.

dbinom(0, size=12, prob=0.2) + 
dbinom(1, size=12, prob=0.2) + 
dbinom(2, size=12, prob=0.2) + 
dbinom(3, size=12, prob=0.2) + 
dbinom(4, size=12, prob=0.2) 
##[1] 0.9274445

##########
##Exercise 2. Chi-square test
##########
## a) Consider the dataset dutchSpeakersDistMeta from our first tutorial again. 
##    Load the package (languageR) and look at the summary of the variables, 
##    as well as their classes. Which variables are factors?
library(languageR)
summary(dutchSpeakersDistMeta)

#forfactors
Filter(is.factor, dutchSpeakersDistMeta)
# all variables except AgeYear are factors

## b) We want to find out whether there is a difference between males and females 
##    with respect to the age groups they are in.
##	  First use the function 'table()' to get the counts and create 
##    a contingency table of AgeGroup by Sex.
age <- table(dutchSpeakersDistMeta$AgeGroup, dutchSpeakersDistMeta$Sex)
(age)
##    Visualize your data with a single bar plot (use ggplot) that represents the counts with respect to each age group 
##	  and each sex.
library(ggplot2)
ggplot(data = as.data.frame(age),aes(x = Var1, y = Freq, fill = Var2)) +geom_bar(stat='identity', position='dodge')


## c) Inspect the table 'age' you created. Does it look like there could be a significant 
##    difference between the sexes?
age
###In the group age35to44 the difference is that males are twice the number of females
###For the rest of the group the numbers of males are less than no of females

## d) We are going to calculate whether there's a difference between males and females 
##    regarding their age group
##    using the function chisq.test. Look at the help of this function. Then use the 
##    function to calculate whether there's a difference in our table 'age'. 
##    Is there a significant difference in age group?
?chisq.test
chisq.test(age,correct=FALSE)
### The p value is greater than the alpha so we accept the null hypothesis
###There is a significant difference in ageGroups

## e) What are the degrees of freedom for our data? How are they derived

####degrees of freedom are calculated as follows:
#### degrees of freedom = df =(rows_count-1)X(column_count-1)
####for our data = df = (5-1)x(2-1) = 4

##########
##Exercise 3. Binomial versus chi-square
##########
##    In this exercise, we'll do significance tests for a paper on therapeutic touch 
##    (google it if you want to know what that is...) that was published in the Journal 
##    of the American Medical Association (Rosa et al., 1996).
##    The experimenters investigated whether therapeutic touch is real by using the 
##    following method:
##    21 practitioners of therapeutic touch were blindfolded. The experimenter 
##    placed her hand over one of their hands. If therapeutic touch is a real 
##    phenomenon, the principles behind it suggest that the participant should 
##    be able to identify which of their hands is below the experimenter's hand. 
##    There were a total of 280 trials, of which the therapeutic touch therapists 
##    correctly indicated when a hand was placed over one of their hands 123 times.

## a) What is the null hypothesis, i.e. how often would we expect the participants to 
##    be correct by chance (in raw number and in percentage)?

#Null Hypothesis: H0: Therapeutic touch is real
##In Raw Numbers = 123/280
##In Percentage = 43.9% percent

## b) Using a chisquare test, what do you conclude about whether therapeutic touch 
##    works? 

#making a table first
########statistical significance level: alpha=0.05
the_touch <- matrix(c(140,140,123,157), ncol = 2, byrow = TRUE)
colnames(the_touch) <- c("yes","no")
rownames(the_touch) <- c("observed", "expected")
the_touch
####      yes no
#expected  140 140
#observed  123 157
###degrees of freedom here: df = (2-1)x(2-1) = 1
ans<-chisq.test(the_touch, correct=FALSE)
X_squared<-2*ans$statistic
X_squared 
#4.143847
?pchisq
pchisq(4.1438,1,lower.tail = FALSE)
#[1] 0.04178726
#which is less than alpha value so we reject the NULL Hypothesis

## c) Now calculate significance using the binomial test as we used it in exercise 1.
dbinom(123, size=280, prob=0.44)
#[1] 0.04798056
#dbinom(123, size=280, prob=0.5)
#[1] 0.006059922


## d) The results from these two tests are slightly different. Which test do you think 
##    is better for our data, and why?


##########
##Exercise 4.
##########
## Describe a situation where you would choose McNemar's test over the ChiSquare test. 
## What would be the problem of using the normal ChiSquare test in a case where 
## McNemar's test would be more appropriate?

##  A Clinical Study in which we are testing the efficiency of an analgesic would be such a situation.
##  McNemar's would be the appropriate one since it considers that some answers may change (sometimes randomly)
## and since we're not testing for independance but for consistency this is quite useful, since we can  see a
## direction to said change.
##
## That being said, the problem with ChiSquare is that it assumes independence, which is not the focus of our
## study.

