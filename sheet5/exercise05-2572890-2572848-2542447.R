### Stats with R Exercise sheet 5

##########################
#Week6: t-test and friends
##########################


## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Sunday, November 25. Write the code below the questions. 
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

###############
### Cleaning Data
###############

library(lsr)
library(tidyr)
library(effsize)


# set your wd and load the data frame digsym_clean.csv
getwd()
data = read.csv("digsym_clean.csv")
# get rid of the column "X"
data <- data[,-2]
head(data)
# Say you're interested in whether people respond with different accuracy to 
# right vs wrong picture-symbol combinations.
# In other words, you want to compare the average accuracy for the digsym-right 
# and digsym-wrong condition.
# Like the conscientious researcher you are, you want to take a look at the data 
# before you get into the stats.
# Therefore, you will need to create a barplot of the mean accuracy data 
# (split out by condition) using ggplot and the summarySE function (given below).
# Let's do it step by step.

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE, conf.interval=.95) {
  # data: an input dataframe
  # measurevar: a column name of <data> (as string), on which we would like to calculate standard 
  #             deviation (SD), standard error (SE) and confidence interval (CI).
  # groupvars: categorical columns of <data> (as vector of strings ) which we would like to use
  #            to make all possible combinations for which we calculate SD, SE, CI based 
  #            on <measurevar>.
  # na.rm: should we remove NA
  # conf.interval: confidence interval
  library(doBy)
  
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # Collapse the data
  formula <- as.formula(paste(measurevar, paste(groupvars, collapse=" + "), sep=" ~ "))
  datac <- summaryBy(formula, data=data, FUN=c(length2,mean,sd), na.rm=na.rm)
  
  # Rename columns
  names(datac)[ names(datac) == paste(measurevar, ".mean",    sep="") ] <- measurevar
  names(datac)[ names(datac) == paste(measurevar, ".sd",      sep="") ] <- "sd"
  names(datac)[ names(datac) == paste(measurevar, ".length2", sep="") ] <- "N"
  
  # Calculate standard error of the mean
  datac$se <- datac$sd / sqrt(datac$N)  
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

# apply the function summarySE on the accuracy data grouping by right/wrong condition
# (use the provided documentation inside the function for the arguments description)
dataSummary <- summarySE(data=data, measurevar="accuracy", groupvars="condition")

# Create the barplot (use ggplot2 for this and all tasks below) with error bars 
# (which the function summarySE readily provided).
# Gauging from the plot, does it look like there's a huge difference in accuracy 
# for responses to the right and wrong condition?
library(ggplot2)
ggplot(data=dataSummary, aes(x=condition, y= se))+geom_bar(stat="identity")
###The responsed for right and wrong differ in Standard errors by amount of 0.02 given the graph


# Let's go back to our data frame "data", which is still loaded in your console
# Now that you've taken a look at the data, you want to get into the stats.
# You want to compute a t-test for the average accuracy data in the right and 
# wrong condition.
# Why can't you compute a t-test on the data as they are now? 
# Hint: which assumption is violated?

#the data is still the mix between the right/wrong data, we need to split them.


# we need to reshape( - cast) the data to only one observation (average accuracy)
# per subject and right/wrong condition 
# Collapse the data, using 
# cast(data, var1 + var2 + var3 ... ~, function, value = var4, na.rm = T)
library(reshape)
library(reshape2)
data_wide <- cast(data=data, Subject ~ condition, mean, na.rm=T, value="accuracy")

dataWide <- gather(data_wide, condition, accuracy, right:wrong, factor_key = TRUE)

# Create a histogram of the accuracy data depending on the right and wrong 
# condition and display them side by side

ggplot(dataWide, aes(x=Subject, y=accuracy, fill=condition))+geom_bar(stat="identity")+facet_wrap(~condition)

# Display the same data in a density plot 

ggplot(dataWide, aes(x=Subject, y=accuracy, fill=condition))+geom_density(stat="identity")+facet_wrap(~condition)

# Based on the histograms and the density plots - are these data normally 
# distibuted?

#No,they are not Normally distributed

# Create a boxplot of the accuracy data

boxplot(dataWide$accuracy)
# Compute the t-test to compare the mean accuracy between wrong and right picture
# combinations.
# Do you need a paired t-test or independent sample t-test? why?

t.test(accuracy~condition, dataWide, paired=TRUE)
#We need paired t-test because our data comes from the same subjects.

# What does the output tell you? What conclusions do you draw?

#Paired t-test

#data:  accuracy by condition
#t = 3.7691, df = 36, p-value = 0.000588
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
 # 0.01303888 0.04341757
#sample estimates:
 # mean of the differences 
#0.02822823

##Since the true mean of difference is not zero it is (0.02822823) so we reject the NULL Hypothesis.
###NULL HYP: That the right and wrong combination of the pitcure has the same accuracy

# Compute the effect size using CohensD 
cohensD(accuracy~condition, dataWide, method = "paired")
#[1] 0.6196291

# How big it is? How do you interpret this result?

#It states that how much of an effect does the difference in accuracy causes.

# In addition to the long-format data we've just been working on, you may also 
# encounter data sets in a wide format 
# (this is the format we have been using in class examples.)
# Let's do a transformation of our data set to see how it would like in a wide 
# format.
# Use "spread" in tidyr.
dataWide <- spread(dataWide,condition,accuracy)
# Compute the t test again on the wide format data - note that for wide-format 
# data you need to use a different annotation for the t-test.
t.test(dataWide$right, dataWide$wrong, paired = TRUE)


# Compare the t-test results from the wide-format and the long-format data.

#Paired t-test

#data:  dataWide$right and dataWide$wrong
#t = 3.7691, df = 36, p-value = 0.000588
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
 # 0.01303888 0.04341757
#sample estimates:
 # mean of the differences 
#0.02822823 

#they are sAME

# Compute CohensD on the wide format data.

cohensD(dataWide$right, dataWide$wrong, method = "paired")
# 0.6196291

# Suppose you are interested in whether reaction times in the digit symbol 
# task differ depending on gender.
# In other words, you want to test whether or not men perform significantly 
# faster on average than women, or vice versa.
# Let's try the t-test again, but for a different question:
# Collapse the data again, using 
# cast(data, var1 + var2 + var3 ... ~ ., function, value = var4, na.rm = T)

castDat <- cast(data, condition~Gender, mean, value = "accuracy")

# Take a look at the resulting data frame using head()

head(castDat)
# Compute the t-test to compare the accuracy means of female and male 
# participants.

t.test(castDat$female, castDat$male)

#Welch Two Sample t-test

#data:  castDat$female and castDat$male
#t = 0.2257, df = 1.8635, p-value = 0.8437
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.0952706  0.1050590
#sample estimates:
#  mean of x mean of y 
#0.9456349 0.9407407 
###########t value is not in the confidence interval again. We go with the Alternative Hypothesis

# Which t-test do you need and why? How do you interpret the result?
##We use independent T test because the the data is from two different group of samples
