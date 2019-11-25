### Stats with R Exercise sheet 6

##########################
#Week 7: Correlation and Regression
##########################


## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Sunday, December 2. Write the code below the questions. 
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


library(reshape)
library(languageR)

library(ggplot2)

#######################
### PART 1: Correlation
#######################

########
### Please, use ggplot to make plots in all exercises below!
########

# Get some data - access the ratings data set in languageR and name it "data".
# Subjective frequency ratings and their length averaged over subjects, for 81 concrete English nouns.
data("ratings")
data <- ratings
rm("ratings")

# Take a look at the data frame.
summary(data)
head(data)

# Let's say you're interested in whether there is a linear relationship between the word frequency of 
# the 81 nouns and their length.
# Take look at the relationship between the frequency and word length data by means a of a scatterplot 
# (from ggplot library).
ggplot(data, aes(x=Frequency, y=Length)) + geom_point()

# Judging from the graphs, do you think that word frequency and word length are in any way correlated 
# with one another?
## judging from the graph, there seems to be no relationship.

# Compute the Pearson correlation coefficient between the two variables by means of cor().
# Tell R to only include complete pairs of observations.
# As a reminder: Pearson coefficient denotes the covariance of the two variable divided by the product 
# of their variance. It is scaled between 1 (for a perfect positive correlation) to -1 (for a perfect 
# negative correlation).
cor_per <- cor(data$Frequency, data$Length, use = "complete.obs", method = "pearson")
cor_per
# Does the correlation coefficient suggest a small, medium or large effect?
# What about the direction of the effect?
## we have a correlation of -0.4281462, hence in negative direction. With 0.42 we have a medium effect.

# Note that we have a large number of tied ranks in word length data (since there are multiple words 
# with the length of, e.g. 5).
# Thus, we might draw more accurate conclusions by setting the method to Kendall's tau instead of 
# Pearson (which is the default).
# How do you interpret the difference between these 2 correlation coefficients?
cor_kendall <- cor(data$Frequency, data$Length, use = "complete.obs", method = "kendall")
cor_kendall
## we actually get a worse correlation (-0.316297) in the same direciton.

# What about significance? Use the more user-friendly cor.test!
significance <- cor.test(data$Frequency, data$Length)
significance
# Take a look at the output and describe what's in there.
# What do you conclude?
## we get a p-value of 6.685e-05, which is below our significance level and therefor significant.

# Finally, we can also calculate Spearman's rank correlation for the same data.
cor_spearman <- cor(data$Frequency, data$Length, use = "complete.obs", method = "spearman")
cor_spearman
## with the spearman method we get a slightly better corelation in negative direciton (-0.4311981)

###################################################################################################


#######################
### PART 2: Regression
#######################

# Fit a linear regression model to the data frame for the variables frequency (outcome variable) 
# and Length (predictor variable).
# General form: "modelname <- lm(outcome ~ predictor, data = dataFrame, na.action = an action)"
model <- lm(Frequency ~ Length, data = data)

# How do you interpret the output? Is the relationship between the two variables positive or negative?
# Plot the data points and the regression line.
## The relationship between the two variables is negative.
ggplot(model, aes(x = Frequency, y = Length)) +
  geom_point() +
  stat_smooth(method = "lm", col = "red")


# Run the plotting command again and have R display the actual words that belong to each point. 
# (Don't worry about readability of overlapping words.)
ggplot(model, aes(x = Frequency, y = Length)) +
  geom_text(aes(label = data$Word)) +
  stat_smooth(method = "lm", col = "red")



###################################################################################################


# Try this again for another example:
# Let's go back to our digsym data set.
# Set your wd and load the data frame digsym_clean.csv
# You can download this data frame from material of week 6: T-tests

setwd('D:\\MasterInsaarland\\Semester2\\statics\\sheet5')
data2 <- read.csv('digsym_clean.csv')

# Suppose you want to predict reaction times in the digit symbol task by people's age.
# Fit a linear regression model to the data frame for the variables correct_RT_2.5sd (outcome variable) 
# and Age (predictor variable).
# General form: "modelname <- lm(outcome ~ predictor, data = dataFrame, na.action = an action)"


# Let's cast the data to compute an RT mean (use correct_RT_2.5sd) for each subject, so that we have only one Age 
# observation by Subject.
# In case you're wondering why we still have to do this - like the t-test, linear regression assumes 
# independence of observations.
# In other words, one row should correspond to one subject or item only.

temp_data <- cast(data=data2, Subject+Age~., mean, value="correct_RT_2.5sd",na.rm=T)
temp_data$Age<-as.numeric(temp_data$Age)
temp_data <- rename(temp_data,c("(all)"="rt"))

# Fit the regression model.

model_digSys <- lm(rt~Age, data = temp_data, na.action=na.omit)

# Let's go over the output - what's in there?
# How do you interpret the output?
summary(model_digSys)

# Again plot the data points and the regression line. 
Plot1<-ggplot(data=temp_data, aes(x=Age,y=rt))+geom_point()+
  geom_smooth(method="lm",se=FALSE)
Plot1

# Plot a histogram and qq-plot of the residuals. Does their distribution look like the normal distribution?


ggplot(data=temp_data,aes(residuals(model_digSys)))+geom_histogram(bins=30)

#Almost a normal distribution 
ggplot(data=temp_data, aes(sample=residuals(model_digSys)))+stat_qq()

# Plot Cooks distance which estimates the residuals (i.e. distance between actual values and the 
# regression line) for individual data points in the model.
Cooks_dist<-as.data.frame(cooks.distance(model_digSys))
Cooks_dist

ggplot(data=temp_data, aes(x=row.names(Cooks_dist),y=cooks.distance((model_digSys))))+geom_point()
# It actually looks like we have 1 influential observation in there that has potential to distort 
# (and pull up) our regression line.
# The last observation (row 37) in cast yielded a Cooks D is very high (greater than 0.6).
# In other words, the of the entire regression function would change by more than 0.6 when this 
# particular case would be deleted.

# What is the problem with observation 37?
# Run the plotting command again and have R display the subjects that belong to each point.

ggplot(data=temp_data, aes(x=Age,y=rt))+geom_point()+
  geom_smooth(method="lm",se=FALSE)+
  geom_text(aes(label=Subject))

# Make a subset of "cast" by excluding this subject and name it cast2.


cast2 <- temp_data[-37,]


# Fit the model again, using cast2, and take a good look at the output.

model_cast2 <- lm(rt~Age, data = cast2, na.action=na.omit)
summary(model_cast2)

# What's different about the output?
# How does that change your interpretation of whether age is predictive of RTs?

# Plot the regression line again - notice the difference in slope in comparison to our earlier model fit?

Plot2<-ggplot(data=cast2, aes(x=Age,y=rt))+geom_point()+
  geom_smooth(method="lm",se=FALSE)
Plot2

# Display the two plots side by side to better see what's going on.

library(gridExtra)
par(mfrow=c(1,2))
plot(cast2$subject,cast2$correct_RT_2.5sd)
abline(model_digSys)

plot(temp_data$subject,temp_data$correct_RT_2.5sd)
abline(model_cast2)


# Compute the proportion of variance in RT that can be accounted for by Age.
# In other words: Compute R Squared.
# Refer to Navarro (Chapter on regression) if you have trouble doing this.


# How do you interpret this number?

#### cast2 regression line passes more through the points without the outlier
