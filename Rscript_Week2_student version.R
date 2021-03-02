


setwd("C:/Users/ahu.alanya/Desktop/SEM_2020/Week2/SEM_lab_Week2_toledo_2020")
getwd() 

## Install packages
install.packages("lavaan")
install.packages("lavaan.survey")
install.packages("semPlot")
# install.packages("corrplot")
# install.packages("psych")
# install.packages("ggplot2")

#load the packages, needed one time per session  
library(semPlot)
library(lavaan)


#########################
## GENERATING DATA
#########################

population.model<-'content  =~ 0.5*x1 + 0.6*x2 + -0.8*x3 + -0.9*x4 
style 	=~ 0.2*x1 + 0.2*x2 +  0.2*x3 +  0.2*x4 
content ~~1*content 
style   ~~ 1*style 
content ~~ 0*style' 

set.seed(5647)
myData<-simulateData(population.model,sample.nobs=500,standardized=TRUE)


#EXERCISE  1.1 Analyzing generated data

# . Analyze the data using the correct analysis model (i.e. including the style factor)
# . Review the output: are the parameter values as expected (factor loadings, factor covariances, etc.)?
# . Analyze the data using an incorrect analysis model by leaving out the style factor.  Compare model fit.
# Has the model fit significantly deteriorated? Compare factor loadings across models.  Is there evidence of bias on the estimates?



#EXERCISE  1.1 Solution

# analyzing according to the correct model 



# analyzing according to an incorrect analysis model 



########################
## SEM MODELS
########################

# . The "Political Democracy   model" from Bollen, K. A. (1989):
# Variables y1 through y4 are intended to be indicators of the latent variable political democracy 
# in 1960; y5 through y8 indicators of political democracy in 1965; 
# and x1 through x3 indicators of industrialization in 1960.  
# Number of observations is 75. We assume that industrialization 
# influences the political democracy variables in 1960 and 1965; 
# and that the 1960 pol.  dem.  
# variable influences the 1965 pol. dem.  variable:
# y1
# Freedom of the press, 1960
# y2
# Freedom of political opposition, 1960
# y3
# Fairness of elections, 1960
# y4
# Effectiveness of elected legislature, 1960
# y5
# Freedom of the press, 1965
# y6
# Freedom of political opposition, 1965
# y7
# Fairness of elections, 1965
# y8
# Effectiveness of elected legislature, 1965
# x1
# GNP per capita, 1960
# x2
# Energy consumption per capita, 1960
# x3
# Percentage of labor force in industry, 1960

# Political Democracy SEM: covariance matrix (N=75) & standar deviations.
pol.democ.sem.lower <- '
6.89
6.25	15.58				
5.84	5.84	10.76			
6.09	9.51	6.69	11.22		
5.06	5.60	4.94	5.70	6.83	
5.75	9.39	4.73	7.44	4.98	11.38			
5.81	7.54	7.01	7.49	5.82	6.75	10.80		
5.67	7.76	5.64	8.01	5.34	8.25	7.59	10.53	
0.73	0.62	0.79	1.15	1.08	0.85	0.94	1.10	0.54
1.27	1.49	1.55	2.24	2.06	1.81	2.00	2.23	0.99  2.28
0.91	1.17	1.04	1.84	1.58	1.57	1.63	1.69	0.82  1.81  1.98'
pol.democ.sd<- c(2.623,3.947,3.281,3.349,2.613,3.373,3.286,3.246)
pol.democ.sem.cov <- getCov(pol.democ.sem.lower, names=c(paste("y",1:8,sep=""),paste("x",1:3,sep="")))

  
#   EXERCISE  1.2 Political Democracy SEM
 
# . Use the covariance matrix below to fit the model depicted in the figure
# . Does the model fit the data well?
# . Ask lavaan to calculate these indirect and total effects. Compare to your own solution
# For those interested in
# . Inspect the regression parameters and the R-squared values of the latent variables
# . Manually calculate the indirect effect of Industrialization on Political Democracy in 1965 via Pol.  Dem. in 1960 (a*b) and its S.E. using Sobel's formula:
# . Manually calculate the total effect of Industrialization on Political Democracy in 1965



#   EXERCISE  1.2 Solution Political Democracy SEM






# SEM MODEL WITH A PREDICTOR

# EXAMPLE 1.2.1 PSYCHOsoCIAL HEALTH

# Data: Umstattd-Meyer, Janke and Beaujeen (2013) measured psychosocial and physical health, and personal mobility 
# Fit the SEM model to the data (see slides for the diagram)
# Are both psyshological and physical health predictive of personal mobility?

# convert vector of correlations into matrix
mobility.cov <- (c(0.77,   0.38,  0.65, 0.39,  0.39,  0.62,   -0.25, -0.32, -0.27, 6.09, 0.31,
                   0.29,   0.26, -0.36, 7.67,  0.24,  0.25,    0.19, -0.18,  0.51, 1.69, -3.16, 
                   -3.56, -2.63,  6.09, -3.12, -4.58, 204.79, -0.92, -0.88, -0.72, 0.88, -1.49,
                   -1.41, 16.53, 7.24))

lower <- '
0.77,   0.38,  0.65, 0.39,  0.39,  0.62,   -0.25, -0.32, -0.27, 6.09, 0.31,
0.29,   0.26, -0.36, 7.67,  0.24,  0.25,    0.19, -0.18,  0.51, 1.69, -3.16, 
-3.56, -2.63,  6.09, -3.12, -4.58, 204.79, -0.92, -0.88, -0.72, 0.88, -1.49,
-1.41, 16.53, 7.24'
mobility.cov <- getCov(lower, names = c("Dep.1", "Dep.2", "Dep.3", 
              "SocActivity", "Falls", "Chronic", "TotActivity", "PersMobility"))
# sample.nobs=6053


# EXAMPLE 1.2.1 PSYCHOsoCIAL HEALTH  Solution






###############
# MIMIC MODELS
###############
 
# . MIMIC stands for "Multiple Indicators, Multiple Causes"
# . Simple example:  one factor measured by 4 indicators, and influenced by 2 causes
# . Generate some data according  to a population model:
  
population.model<-'f =~ .7*x1 + .6*x2 + - .5*x3 +.4*x4
f ~ .2*y1 + .3*y2'

set.seed(114254)
mimic.data<-simulateData(population.model,sample.nobs=500,standardized=TRUE)

# Typically, the measurement model is developed first, after which covariates are added.


# # EXERCISE 1.3
# 
# a.  Fit the measurement model and review the fit statistics. Also review the parameter estimates. How close are they to the population values?
# b.  Add one covariate and watch for changes in fit statistics and parameter estimates.
# c.  Add the second covariate and again see if fit statistics or parameter estimates change. 
# d.  Calculate the degrees of freedom for each of these models manually.
# e.  Repeat steps a-c a few times with different data sets.
# Is there a trend for the fit statistics to change (improve or deteriorate) when including covariates?



# EXERCISE  1.3:  Solution MIMIC (a-c)



# EXERCISE  1.3: Solution (d)



# EXERCISE  1.3: Solution (e)



####################################
# MEASUREMENT EQUIVALENCE TESTING
####################################
 
# . Measurement Equivalence if a measurement instrument produces equivalent results, regardless of some unrelated properties of the test subjects.
# . Absence of measurement equivalence would imply some degree of distortion of the results ("bias").  A test instrument could be perceived as "unfair", e.g. an IQ test that favors males by including "gender-biased test items"
# . What is the effect of such a bias?  Would we draw incorrect conclusions?
# . Can we detect whether such bias is present in our data?



# EXERCISE  1.4
# 
# . Generate data using the same model as in Ex.  1.3, but include a direct effect of "y2" to "x3" 
#  (magnitude = .2).  Simulate a data set that includes 500 subjects.
# . Fit a MIMIC model as in Ex. 1.3 (including both y-covariates).  
#   Review model fit and parameter estimates.
# . Request modification indexes.  Is the direct effect of y2 on x3 detected?

# . Rewrite the model to include all direct effects of the y-covariates on all x-variables, 
#   but fix the regression weights to zero.  
#   Re-fit the model and request modification indexes.
# . Re-fit the model including the direct effect to account for the uniform DIF. 
#   What happens to model fit and model parameters?


# EXERCISE  1.4:  Solution






# EXERCISE 2 EUROBAROMETER Public health attitudes

#  See the course handout for the diagram
# Eurobarometer 72.3 (October 2009) contained questions about attitudes towards alcohol use.
# In this exercise a random sample of 1000 records from the full dataset will be used.
# The sample is restricted to respondents from Belgium, the Netherlands, Italy and Germany. 
# The dataset is called ZA4977_sem.dat (comma separated values, variable names on the first line).
# 
# First set up a measurement model involving only the latent variables and their indicators. 
# You may want to do an EFA model to iteratively work your way towards a final measurement model (include variable n171 if you do), 
# or you may start out with the measurement model as shown.
# 
# In the final step, introduce the covariates and build the MIMIC model as shown in the graph. 
# You have now built a MIMIC model.




# EXERCISE 2 Solution STEP 1 E/CFA

list.files()
f<-"D:/QASS/SEM/Lab/ZA4977_sem.dat"
s<-read.csv(f,sep=",")







# EXERCISE 2 Solution STEP 2 
# 
# --  removing non-significant factor loadings and low factor loadings that are
# --  as expected (i.e. loading on the "wrong" factor)






# EXERCISE 2 Solution STEP 3

# -- removing item n171 because it seems like a "trouble maker" 
# (see the MI's, largest MI's involve n171)
# n171 ~~ n174 : 77.059 (expected correlation: -.27)
# n171 ~~ n176 : 71.198 (expected correlation:  .25)
# -- an alternative is to allow these error covariances






# EXERCISE 2 Solution STEP 4 Final measurement model

# there is one large MI (44) for an error covariance between n173 and n174 (cancers
# and asthma). The expected correlation between the residuals is a moderately
# strong one (0.21).
# I don't see a strong theoretical basis for allowing this error covariance, so I'm not
# going to introduce it. We didn't see this error covariance when doing the analysis on
# the original file (which also contained some records from Greece). So maybe this is
# a country-specific thing?
# Also note the moderately large MI (24.913) for n166 ~~ n167 and the strong expected
# correlation (.664). The two items are worded as follows:
# n166: Alcohol advertising targeting young people should be banned in all EU member states
# n167: Selling and serving alcohol to people under 18 years should be banned in all EU member states
# So these items are quite similar (both about banning something for young people), so
# I will introduce the error covariance


    
                                  
 # EXERCISE 2 Solution STEP 5 SEM model
                                  





 # EXERCISE 2 Solution STEP 6 Adding control variables
 

 
 
                                  
                                  


