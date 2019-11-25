###############
### Cleaning Data
###############


#Muhammad Hamza jamil  2572890
#Fahad Aslam   2572848
#Thorsten Schamper 2542447



# Please do the "Cleaning Data with R" exercise that was assigned in dataCamp.
# We recommend that you take notes during the DataCamp tutorial, so you're able to use the commands 
# you learned there in the exercise below.
# This week, the exercise will be about getting data into a good enough shape to start analysing. 
# Next week, there will be a tutorial on running t tests for this data.
## You need to provide a serious attempt to each exercise in order to have
## the assignment graded as complete. 

# 1. Download the data file "digsym.csv" from the moodle and save it in your working directory. 
# done
getwd()
setwd("D:\\MasterInsaarland\\Semester2\\statics\\Sheet2")

# 2. Read in the data into a variable called "dat".
dat <- read.csv("digsym.csv")

# 3. Load the libraries languageR, stringr, dplyr and tidyr.
library(languageR)
library(stringr)
library(dplyr)
library(tidyr)

# 4. How many rows, how many columns does that data have?
str(dat)
# the data has 11 variables/columns and 3700 observations/rows

# 5. Take a look at the structure of the data frame using "glimpse"
glimpse(dat)

# 6. View the first 20 rows, view the last 20 rows
head(dat, 20)
tail(dat, 20)

# 7. Is there any missing data in any of the columns?
any(is.na(dat))
# yes there is missing data

# 8. Get rid of the row number column
dat <- dat[,-1]

# 9. Put the Sub_Age column second
dat <- dat[,c(1, 10, 2, 3, 4, 5, 6, 7, 8, 9)]

# 10. Replace the values of the "ExperimentName" column with something shorter, more legible
dat$ExperimentName <- as.factor(str_replace(dat$ExperimentName,"Digit Symbol - Kopie","DSK"))
glimpse(dat)
# 11. Keep only experimental trials (encoded as "Trial:2" in List), get rid of practice trials 
# (encoded as "Trial:1"). When you do this, assign the subset of the data to a variable "data2", 
# then assign data2 to dat and finally remove data2.
data2 <- subset(dat, subset = List == "Trial:2")
dat <-data2
remove(data2)

# 12. Separate Sub_Age column to two columns, "Subject" and "Age", using the function "separate"
dat <- separate(dat, Sub_Age, c("Subject", "Age"))

# 13. Make subject a factor
dat$Subject <- as.factor(dat$Subject)

# 14. Extract experimental condition ("right" vs. "wrong") from the "File" column:
# i.e. we want to get rid of digit underscore before and the digit after the "right" and "wrong".
dat$File <- str_extract(string = dat$File, pattern = "right|wrong")

# 15. Using str_pad to make values in the File column 8 chars long, by putting 0 on the end  (i.e., 
# same number of characters, such that "1_right" should be replaced by "1_right0" etc)
dat$File <- str_pad(dat$File, width = 8, side = "right", pad = "0")

# 16. Remove the column "List"
dat$List <- NULL

# 17. Change the data type of "Age" to integer
dat$Age <- as.integer(dat$Age)

# 18. Missing values, outliers:
# do we have any NAs in the data, and if so, how many and where are they?
summary(dat)
any(is.na(dat))
# no there are no more NAs in the data

# 19. Create an "accuracy" column using if-statement
# if actual response (StimulDS1.RESP) is the same as the correct response (StimulDS1.CRESP), put 
# in value 1, otherwise put 0


totalROWS = nrow(dat)
value=NULL
for (i in 1:totalROWS)
  if(dat$StimulDS1.RESP[i]==dat$StimulDS1.CRESP[i]) value[i] <- 1 else value[i] <- 0
dat["accuracy"] <- value
glimpse(dat)
# 20. How many wrong answers do we have in total?
sum(with(dat,accuracy == 0))
#Total 185 wrong response

# 21. Whats the percentage of wrong responses?
(185/totalROWS)*100 
# The percentage of wrong response is 5.55%


# 22. Create a subset "correctResponses" that only contains those data points where subjects responded correctly. 

correct_RT=NULL
for(i in 1:totalROWS)
  if(dat$accuracy[i]==1) correct_RT[i] <- dat$StimulDS1.RT[i]

# 23. Create boxplot of StimulDS1.RT - any outliers?

boxplot(dat$StimulDS1.RT)
# 24. Create histogram of StimulDS1.RT with bins set to 50

hist(dat$StimulDS1.RT, breaks =50)
# 25. Describe the two plots - any tails? any suspiciously large values?

# The histogram has a suspeciously large vale of around 14000. It is left side positively skewed.
# There is a lower tail.

# 26. View summary of correct_RT
summary(correct_RT)

# 27. There is a single very far outlier. Remove it and save the result in a new dataframe named "cleaned".
index <- which(dat$StimulDS1.RT == 13852)
correct_RT <- correct_RT[-c(index)]

cleaned.data <- data.frame(dat[-c(index),])
glimpse(cleaned)
## EXTRA Exercises:
##You can stop here for your submission of this week's assignment,
##but you are encouraged to try it now. 
##All these exercises will be discussed and solved in the tutorial!

# 28. Dealing with the tail of the distribution: outlier removal
# Now, remove all correct_RT which are more than 2.5. SD away from the grand mean
UpperLimit <- mean(correct_RT,na.rm = TRUE) + (2.5*sd(correct_RT,na.rm = TRUE))
LowerLimit <- mean(correct_RT,na.rm = TRUE) - (2.5*sd(correct_RT,na.rm = TRUE))

rows <- correct_RT
len <- length(rows)

for(i in 1:len)
  if(!is.na(rows[i]))
    if((LowerLimit > rows[i]) || (rows[i] > UpperLimit)) rows[i] <- NA

# 29. Create new "correct_RT_2.5sd" column in data which prints NA if an RT value is below/above the cutoff
cleaned["correct_RT_2.5sd"] <- rows

# 30. Take a look at the outlier observations
# any subjects who performed especially poorly?
boxplot(cleaned$correct_RT_2.5sd)

# 31. How many RT outliers in total?

# 32. Plot a histogram and boxplot of the correct_RT_2.5sd columns again - nice and clean eh?

hist(cleaned$correct_RT_2.5sd, breaks = 50)
boxplot(cleaned$correct_RT_2.5sd)
# 33. Next, we'd like to take a look at the avrg accuracy per subject
# Using the "cast" function from the library "reshape", create a new data.frame which shows the 
# average accuracy per subject. Rename column which lists the average accuracy as "avrg_accuracy".
library(reshape)
avrg_accuracy <- cast(cleaned,Subject~accuracy,mean)

colnames(cleaned)[which(names(cleaned) == "accuracy")] <- "avrg_accuracy"

# 34. Sort in ascending order or plot the average accuracies per subject.

cleaned <- cleaned[order(cleaned$avrg_accuracy),]

# 35. Would you exclude any subjects, based on their avrg_accuracy performance?
# No, exclusing the subjects would lead to loss in information from the data.

# 36. Congrats! Your data are now ready for analysis. Please save the data frame you created into a new 
# file "digsym_clean.csv".
digsym_clean.csv <- write.csv(cleaned)