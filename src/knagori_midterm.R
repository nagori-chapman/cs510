#CS510 Computing for Scientists Midterm Project
#Krupa Nagori

#Preliminaries
#We'll use the data file `midterm-dataset.csv`
# The file should be in the same directory as this markdown file (which should also be your working directory). 
#It is a data frame of expenditures by household from the consumer expenditure survey

#Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Set and create results folder
#img_path="midterm-submitted/results"
#dir.create(img_path)

#The package script is based on stackoverflow recommendations to install missing packages.
list.of.packages <- c("ggplot2", "plyr", "reshape2", "splines", "boot", "MASS", "broom")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#libraries
library(ggplot2)
library(plyr)
library(reshape2)
library(splines)
library(boot)
library(MASS)
library(broom)


#The command `set.seed(x)`, where `x` is any number, fixes the random number generator to give the same output every time.
set.seed(1)

#Functions
# Below are the functions used to conduct the analysis

glm.cv.loop = function(data, formula.text, DF.vector, K=10) {
# make sure boot library is loaded
require(boot) 
cv.scores = rep(0, times = length(DF.vector))
for (DF in DF.vector) {
# get the fitted model for current value of DF
spline.model = glm(as.formula(formula.text), data=data)
# run K-fold cross validation 
cv <- cv.glm(data=data, glmfit=spline.model, K=K)
# extract the cross-validation score
cv.scores[DF] = cv$delta[1]
}
# make the plot
data.out <- data.frame(df = DF.vector, cv.scores = cv.scores)
cv.plot <- ggplot(data = data.out, mapping=aes(x=df, y=cv.scores)) + geom_point() + labs(x='df', title='Cross Validation Scores')
# return a list containing the scores and the plot
return( list(scores = cv.scores, plot = cv.plot))
}  


Find.QQ <- function(.data, column.name, y) {
# how many quantiles are we plotting?
n.pts <- min( length(.data[, column.name]), length(y))
# which quantiles are we plotting?
probs = seq(from = 0, to = 1, length.out = n.pts)
# compute these quantiles for each group
q1 <- quantile(.data[, column.name], probs= probs)
q2 <- quantile(y, probs=probs )
# return them as a data frame
return( data.frame(q1 = q1, q2 = q2))
}

Pool.Residuals <- function (data.augment, x, qvec = c(.05, .15, .5, .85, .95)) {
require(plyr)
require(reshape2)
# find the quantiles of the residuals
resid.quantile <- quantile(x = data.augment$.resid, probs = qvec)
# add the quantiles of the residuals to the predicted trend
data.augment = mutate(data.augment, 
q1 = .fitted + resid.quantile[1],
q2 = .fitted + resid.quantile[2],                                      
q3 = .fitted + resid.quantile[3],                                      
q4 = .fitted + resid.quantile[4],              
q5 = .fitted + resid.quantile[5])
# combine all of the quantiles into one column for easier plotting:
data.melt <- melt(data.augment, id.vars= x, measure.vars = c('q1', 'q2', 'q3', 'q4', 'q5'))
return( data.melt )
}


# The first step will be to load the data file. 
# The focus of the analysis will be to look at healthcare costs when compared to age of the interviewee 

# Load the data file
demographic_dataset <- read.csv(file = 'dataset/midterm-dataset.csv', header = TRUE)

#Healthcare
#First create a simple lm with healthcare as a function of age.interviewee
agetohealthcost <- lm(formula = healthcare ~ age.interviewee, data = demographic_dataset)
agetohealthcost
#from here plot the data to have a starting point
agetohealthcost.plot <- ggplot(data=demographic_dataset, mapping=aes(x=age.interviewee, y=healthcare)) + geom_point()
agetohealthcost.plot + geom_smooth(method='lm') + 
labs(x = 'Interview Age', y='Healthcare')
# The resulting graph shows that the people of a younger age have lower cost of healthcare and as ages increase the healthcare costs appear to increase as well. 

#Here apply the qualifications
demographic_subset.1 <- demographic_dataset[which(demographic_dataset$healthcare > 1),]
demographic_subset <- demographic_subset.1[-which(demographic_subset.1$age.interviewee > 86),]
#Create a new plot based on the new data that will show the difference from before
agetohealthcost.subset.plot <- ggplot(data=demographic_subset, mapping=aes(x=age.interviewee, y=healthcare)) + geom_point()
agetohealthcost.subset.plot + geom_smooth(method='lm')+ 
labs(x = 'Interview Age', y='Healthcare')
#Removing the extremeties maintained a similar trend in terms of cost to age increase similarly.


#Cross Validation
#This step uses a log to fit a natural spline to your data for cross validation. 
#Mutate to add the log to the dataframe
crossvalidation.subset <- mutate(demographic_subset, log.healthcare = log(healthcare))
#Use the function given to us above
out <- glm.cv.loop(crossvalidation.subset, formula.text = "log.healthcare ~ ns(age.interviewee, df=DF)", DF.vector = 1:30)

crossvalidationmodel <- lm(formula = healthcare ~ age.interviewee, data = crossvalidation.subset)
#create a new subset for the s-l plot
ggplotcv.subset = augment(crossvalidationmodel, data=crossvalidation.subset)
#generate the s-l plot
ggplot(data = ggplotcv.subset, mapping=aes(x = .fitted, y = sqrt(abs(.resid)))) + geom_point(size = 1) + geom_smooth()

#Plot the residuals against the predicted values to see if they look identically distributed. 
#Divide `age.interviewee` and `.fitted` into groups
#use quantile plots or QQ plots to see if the residuals in each group look identically distributed.

#generate a subset from the previous data use cut to group data
residuals.subset <- mutate(ggplotcv.subset, pred = predict(crossvalidationmodel), resid = resid(crossvalidationmodel), age.interviewee.cat = cut_number(age.interviewee, n = 10))
residualsqq.subset <- mutate(residuals.subset, pred = predict(crossvalidationmodel), resid = resid(crossvalidationmodel), fitted.cat = cut_number(.fitted, n = 10))

#plot each of groups to compare
ggplot(data = residualsqq.subset, mapping=aes(sample = resid, color = age.interviewee.cat)) + stat_qq(distribution=qunif) + labs(x = 'quantiles', y = 'residual log healthcare', title = 'Quantile Plot, Residual Log Healthcare')

ggplot(data = residualsqq.subset, mapping=aes(sample = resid, color = fitted.cat)) + stat_qq(distribution=qunif) + labs(x = 'quantiles', y = 'residual log healthcare', title = 'Quantile Plot, Residual Log Healthcare')
#Create qq plots to compare more clearly
#The qq plots will look clustered inside of the plot viewing window in RStudio. For better viewing I suggest using the zoom button to pop it out.
QQ.df <- ddply(residualsqq.subset, 'age.interviewee.cat', Find.QQ, column.name ="resid", y = residualsqq.subset$resid)
QQ2.df <- ddply(residualsqq.subset, 'fitted.cat', Find.QQ, column.name = "resid", y = residualsqq.subset$resid)

ggplot(data = QQ.df, mapping=aes(x = q1, y = q2)) + geom_point() + geom_abline() + facet_wrap('age.interviewee.cat', nrow = 2) + labs(title='QQ Plot, grouped vs pooled residuals')

ggplot(data = QQ2.df, mapping=aes(x = q1, y = q2)) + geom_point() + geom_abline() + facet_wrap('fitted.cat', nrow = 2) + labs(title='QQ Plot, grouped vs pooled residuals')

#The QQ plot help signficantly in trying to see if the residuals in each group look identically distributed.
#Based on the plot generated the residuals in each group look identically distributed.
