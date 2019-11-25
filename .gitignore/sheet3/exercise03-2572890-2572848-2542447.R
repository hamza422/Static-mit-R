### Stats with R Exercise Sheet 3

##########################
#Week4: Hypothesis Testing
##########################

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Sunday, November 11. Write the code below the questions. 
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


###############
### Exercise 1: Deriving sampling distributions
###############
## In this exercise, we're going to derive 5 sampling distributions with 
## different sizes.

## a) Load the package languageR. We're going to work with the dataset 'dative'. 
## Look at the help and summary for this dataset.
library(languageR)
summary(dative)
## The term dative alternation is used to refer to the alternation between 
## a prepositional indirect-object construction
## (The girl gave milk (NP) to the cat (PP)) and a double-object construction 
## (The girl gave the cat (NP) milk (NP)).
## The variable 'LenghtOfTheme' codes the number of words comprising the theme.

## b) Create a contingency table of 'LenghtOfTheme' using table(). 
##    What does this table show you?
LengthOftheme<-table(dative$LengthOfTheme)
(LengthOftheme)


## c) Look at the distribution of 'LenghtOfTheme' by plotting a histogram and a boxplot. 
##    Do there appear to be outliers? Is the data skewed?
hist(LengthOftheme)
boxplot(dative$LengthOfTheme)
# Yes in the Boxplot outliers exists

## d) Now we're going to derive sampling distributions of means for different 
##    sample sizes. 
##    What's the difference between a distribution and a sampling distribution?


#The distribution is just the distribution in the data itself, whereas the sampling distribution is the distribution
#in the sample mean. 

## e) We are going to need a random sample of the variable 'LengthOfTime'. 
##    First create a random sample of 5 numbers using sample(). 
##    Assign the outcome to 'randomsampleoflengths'

randomsampleoflengths<-sample(dative$LengthOfTheme,5)
(randomsampleoflengths)

## f) Do this again, but assign the outcome to 'randomsampleoflengths2'. 

randomsampleoflengths2<-sample(dative$LengthOfTheme,5)
(randomsampleoflengths2)
## g) Now calculate the mean of both vectors, and combine these means 
##    into another vector called 'means5'.
means5<-rowMeans(cbind(randomsampleoflengths,randomsampleoflengths2))

## h) In order to draw a distribution of such a sample, we want means of 
##    1000 samples. However, we don't want to repeat question e and f 
##    1000 times. We can do this in an easier way: 
##    by using a for-loop. See dataCamp or the course books for 
##    how to write loops in R.
means5<-NULL
for(i in 1:1000){
  means5<-c(means5,mean(sample(dative$LengthOfTheme,5)))
}

## i) Repeat the for-loop in question h, but use a sample size of 50. 
##    Assign this to 'means50' instead of 'means5'.
means50<-null
for(i in 1:1000){
  means50<-c(means50,mean(sample(dative$LengthOfTheme,5)))
}

## j) Explain in your own words what 'means5' and 'means50' now contain. 
##    How do they differ?

(means5)
(means50)
#mean5 and mean50 contain sets of mean of each sample values of mean5 is bigger than mean50

## k) Look at the histograms for means5 and means50. Set the number of breaks to 15.
##    Does means5 has a positive or negative skew?
hist(means5,breaks = 15)
hist(means50,breaks=15)
#mean5 is positive skew

## l) What causes this skew? In other words, why does means5 have bigger 
##    maximum numbers than means50?

##when there is a larger amount of sampling the mean is changing drastically, the numbers that were not involved
##are involved and it creates a skew. When there is more to sample on, the range of the values is getting larger.
##more numbers for sampling means more range.

###############
### Exercise 2: Confidence interval
###############

## A confidence interval is a range of values that is likely to contain an 
## unknown population parameter.
## The population parameter is what we're trying to find out. 
## Navarro discusses this in more depth in chapter 10.

## a) What does a confidence interval mean from the perspective of experiment replication?


## b) Let's calculate the confidence interval for our means from the previous 
##    question.
##    First, install and load the packages 'lsr' and 'sciplot'
library(lsr)
library(sciplot)
## c) Look at the description of the function ciMean to see which arguments it takes.
?ciMean()
#ciMean is a function in the library lsr
#Arguments taken by ciMean are as follows:
#ciMean(x,conf=0.95,na.rm=FALSE)
##X: data frame or vector
#conf: is the confidence interval //an optional argument, default value 95%
#na.rm=FALSE: also optional to remove the NA's // default value=FALSE

## d) Use ciMean to calculate the confidence interval of the dataset dative from
##    the previous exercise.
##    Also calculate the means for the variable LengthOfTheme.
ciMean(dative, conf = 0.95, na.rm = FALSE)
mean(dative$LengthOfTheme)
## e) Does the mean of the sample fall within the obtained interval? 
##    What does this mean?
#The mean lies in the obtained interval which is 4.121943 4.421115. 
##############It means thatthe 95% of the area includes the population that is 4.271529+-0.149586  

## f) As the description of dative mentions, the dataset describes the 
##    realization of the dative as NP or PP in two corpora.
##    The dative case is a grammatical case used in some languages 
##    (like German) to indicate the noun to which something is given.
##    This dataset shows us, among other things, how often the theme is 
##    animate (AnimacyOfTheme) and how long the theme is (LengthOfTheme).
##    Plot this using the function bargraph.CI(). Look at the help for this function. 
##    Use the arguments 'x.factor' and 'response'.
bargraph.CI(x.factor=AnimacyOfTheme, response = LengthOfTheme, data=dative)


## g) Expand the plot from question f with the ci.fun argument 
##    (this argument takes 'ciMean'). 
##    Why does the ci differ in this new plot compared to the previous plot?
bargraph.CI(x.factor=AnimacyOfTheme, response = LengthOfTheme, ci.fun=ciMean, data=dative)
###############CI differs since we have provided new confidence interval that is 95% (2.5% to 97.5%)



###############
### Exercise 3: Plotting graphs using ggplot.
###############
# There are many ways of making graphs in R, and each has its own advantages 
# and disadvantages. One popular package for making plots is known as ggplot2. 
# The graphs produced with ggplot2 look professional and the code is quite easy 
# to manipulate.
# In this exercise, we'll plot a few graphs with ggplot2 to show its functionalities.
# You'll find all the information you'll need about plotting with ggplot2 here: 
# http://www.cookbook-r.com/Graphs/
# Also, you have been assigned the ggplot2 course in DataCamp. Please work through 
# this course (Please, set up your name in the datacamp profile. 
# So I can find you quickly.)

## a) First install and load the ggplot2 package. Look at the help for ggplot.


## b) We're going to be plotting data from the dataframe 'ratings' 
##    (included in languageR). 
##    Look at the description of the dataset and the summary.


## For each word, we have three ratings (averaged over subjects), one for the 
## weight of the word's referent, one for its size, and one for the words' 
## subjective familiarity. Class is a factor specifying whether the word's 
## referent is an animal or a plant. 
## Furthermore, we have variables specifying various linguistic properties, 
## such a word's frequency, its length in letters, the number of synsets 
## (synonym sets) in which it is listed in WordNet [Miller, 1990], its 
## morphological family size (the number of complex words in which 
## the word occurs as a constituent), and its derivational entropy (an 
## information theoretic variant of the family size measure). 
## Don't worry, you don't have to know what all this means yet in order to 
## be able to plot it in this exercise!

## c) Let's look at the relationship between the class of words and the length. 
##    In order to plot this, we need a dataframe with the means.
##    Below you'll find the code to create a new dataframe based on the existing 
##    dataset ratings.
##    Plot a barplot of ratings.2 using ggplot. Map the two classes to two 
##    different colours. 
##    Remove the legend.
summary(ratings)
condition <- c("animal", "plant")
frequency <- c(mean(subset(ratings, Class == "animal")$Frequency), mean(subset(ratings, Class == "plant")$Frequency))
length <- c(mean(subset(ratings, Class == "animal")$Length), mean(subset(ratings, Class == "plant")$Length))
ratings.2 <- data.frame(condition, frequency, length)
ratings.2


## d) Let's assume that we have additional data on the ratings of words. 
##    This data divides the conditions up into exotic and common animals 
##    and plants.
##    Below you'll find the code to update the dataframe with this additional data.
##    Draw a line graph with multiple lines to show the relationship between 
##    the frequency of the animals and plants and their occurrence.
##    Map occurrence to different point shapes and increase the size 
##    of these point shapes.
condition <- c("animal", "plant")
frequency <- c(7.4328978, 3.5864538)
length <- c(5.15678625, 7.81536584)
ratings.add <- data.frame(condition, frequency, length)
ratings.3 <- rbind(ratings.2, ratings.add)
occurrence <- c("common", "common", "exotic", "exotic")
ratings.3 <- cbind(ratings.3, occurrence)
ratings.3


## e) Based on the graph you produced in question d, 
##    what can you conclude about how frequently 
##    people talk about plants versus animals, 
##    with regards to how common they are?
