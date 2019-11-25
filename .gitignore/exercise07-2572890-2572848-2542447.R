### Stats with R Exercise sheet 7

##########################
#Week 8: ANOVA
##########################


## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Sunday, December 9. Write the code below the questions. 
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

###########################################################################################



#######################
### PART 1: Preparation
#######################

library(boot)
library(ggplot2)
library(reshape)

# This time we will be working with the "amis" data frame (package 'boot') that has 
# 8437 rows and 4 columns.

# In a study into the effect that warning signs have on speeding patterns, Cambridgeshire
# County Council considered 14 pairs of locations. The locations were paired to account 
# for factors such as traffic, volume and type of road. One site in each pair had a sign 
# erected warning of the dangers of speeding and asking drivers to slow down. No action 
# was taken at the second site. Three sets of measurements were taken at each site. 
# Each set of measurements was nominally of the speeds of 100 cars but not all sites 
# have exactly 100 measurements. These speed measurements were taken before the erection 
# of the sign, shortly after the erection of the sign, and again after the sign had been 
# in place for some time.

# 1. For the further reference please use ?amis. It may take some time to understand the dataset. 
?amis

# 2. Load the dataset and briefly inspect it. 
# Feel free to make some plots, calculate some statistics in order to understand the data.
head(amis,30)

# 3. All our columns have numeric type. Convert the categorial columns to factors.
amis$speed <-factor(amis$speed)
amis$warning <-factor(amis$warning)
amis$pair <-factor(amis$pair)
amis$period<-factor(amis$period)

# 4. Build a boxplot for the distribution of `speed` for each of `period` values 
# (before, immediately after and after some time). Build 2 plots side by side
# depending on the `warning` variable.
# (for all plots here and below please use ggplot)
boxplot1<- ggplot(amis, aes(x = factor(period), y = factor(speed))) +
  geom_jitter() +
  facet_grid(. ~ warning)
print(boxplot1)

# 5. What can you conclude according this plot? What can you say about people behaviour in
# different periods: before, immediately after and after some time?
##The 'immediately after' section is more scattered, the changes in the speed 
##are the most at this part, but 'before' and 'after some time' doesn't have much difference between them. 
##we can conclude this by saying that warnings only make a difference 'immediately after', not much otherwise. 

# 6. What are your ideas about why the data with warning==2 (which correspond to the
# measurements in different times on sites where no sign was erected) was collected?
##to see if there is a difference between speed if there are warnings or not. This way, we can compare if there
##is a difference in speeds when there are warnings or not. 

#######################
### PART 2: 1-way ANOVA
#######################

#1. First let's create a new data frame which will be used for all PART 2.
# For 1-way ANOVA we will be working with the subset of `amis` where the 
# warning sign was erected, which corresponds to warning==1, therefore first
# subset your data to filter out warning==2 and then apply cast() to average
# speed over each "pair" and "period. Assign this new data frame to the variable casted_data.
amis_sub <- subset(amis, amis$warning == 2)
head(amis_sub)
casted_data<- cast(amis_sub, pair+ period~.,value = "speed")
colnames(casted_data)[3] <- "speed"
(casted_data)

# 2. Build a boxplot of the average speed depending on period
boxplot2<- ggplot(casted_data, aes(x = factor(period), y = speed)) +
  geom_boxplot() +
  geom_jitter()
print(boxplot2)

# 3. Is there a difference between the periods?
##The period 2 with warning has less of higher speeds than the other periods.

# 4. Now, let's check each ANOVA assumptions and whether they are violated or not and why.

# a) Independence assumption
# (you need to figure out the best way to do it and give a detailed justified answer)

# b) Normality of residuals
# (you need to figure out the best way to do it and give a detailed justified answer)

##   To check if we have normally distributed residuals we can plot them (QQplot, Histogram) to visually
## verify and run a Shapiro-Wilk test. In case of not being normally distributed, we can switch o a 
## non-parametric test (e.g. Kruskal-Wallis rank sum test)

# c) Homogeneity of variance of residuals
# (you need to figure out the best way to do it and give a detailed justified answer)

##    We use the Levene Test to check if the assumption of homogeneity of variance is fine. If said assumption
## is violated, then we can use a test which does not assume equal variances: the Welch one-way test ANOVA
## which is implemented in R as "oneway.test()"

# 5.Now we are ready to perform 1-way ANOVA: please use the function aov() on the speed
# depending on the period,report p-value and interpret the result in details

summary(aov(casted_data$speed ~ casted_data$period))

##                    Df Sum Sq Mean Sq F value Pr(>F)
## casted_data$period  2    268   133.9   0.526  0.595
## Residuals          39   9923   254.4     

# 6. Please do a pairwise t-test with pairwise.t.test()
# 7. Report pair-wise p-values and interpret the result in details

pairwise.t.test(casted_data$speed, casted_data$period, p.adjust.method="none")

#Pairwise comparisons using t tests with pooled SD 

#data:  casted_data$speed and casted_data$period 

#1    2   
#2 1.00 -   
#  3 0.38 0.38

#P value adjustment method: none 

pairwise.t.test(casted_data$speed, casted_data$period, p.adjust.method="bonferroni")
#Pairwise comparisons using t tests with pooled SD 

#data:  casted_data$speed and casted_data$period 

#1 2
#2 1 -
#  3 1 1

#P value adjustment method: bonferroni 


# 8. Try to use no adjustment for pairwise testing and then Bonferroni correction.
# Does the result change?

## Yes, the result changes for every comparison.

#######################
### PART 3: 2-way ANOVA
#######################
# 1. Now we want to analyze the influence of 2 categorial variables (period and warning) on the speed.
# So let's turn back to our initial dataset amis (not its subset with warning==1)
# First, we need to again average the speed over each `pair`, `warning` and `period
# Cast your data again and assign the resuts to casted_data2
casted_data2<- cast(amis, pair + period + warning ~ .,value = "speed")
colnames(casted_data2)[4] <- "speed"
casted_data2

# 2. Calculate the mean for each of 6 pairs of `period` and `warning`
m = aggregate(. ~ pair, casted_data2, mean)
m

# 3. Do you think there is a significant difference in some of the groups?
## Yes for some of the groups there is a kind of significant difference, i.e. the group 4 with 94.5 in contrast to
## all the other with more then 97.16

# 4. Now apply 2-way ANOVA: please use the function aov() on the speed depending on the period and warning
# report p-value and interpret the result in details

summary(aov(speed ~ period + warning, data = casted_data2))
##             Df Sum Sq Mean Sq F value Pr(>F)
## period       2     33    16.3   0.121  0.886
## warning      1    152   152.0   1.132  0.291
## Residuals   80  10746   134.3 

# 5. What do you conclude about the behaviour of drivers based on the 2-way ANOVA?
## 
