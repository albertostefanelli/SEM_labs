


setwd("C:/Users/ahu.alanya/Desktop/SEM_2020/Week2/AA_w2")
getwd() 

## Install packages
# install.packages("lavaan")
# install.packages("lavaan.survey")
# install.packages("semPlot")
# install.packages("corrplot")
# install.packages("psych")
# install.packages("ggplot2")
# install.packages("semTools")

#load the packages, needed one time per session  
library(semPlot)
library(lavaan)
library(semTools)



#########################
## GENERATING DATA
#########################

population.model<-'content  =~ 0.5*x1 + 0.6*x2 + -0.8*x3 + -0.9*x4 
style 	=~ 0.2*x1 + 0.2*x2 +  0.2*x3 +  0.2*x4 
content ~~1*content 
style   ~~ 1*style 
content ~~ 0*style' 

set.seed(1235)
myData<-simulateData(population.model,sample.nobs=500,standardized=TRUE)
# simsem package in R is another alternative and it is more precise.



#EXERCISE  1.1 Analyzing generated data

# . Analyze the data using the correct analysis model (i.e. including the style factor)
# . Review the output: are the parameter values as expected (factor loadings, factor covariances, etc.)?
# . Analyze the data using an incorrect analysis model by leaving out the style factor.  Compare model fit.
# Has the model fit significantly deteriorated? Compare factor loadings across models.  Is there evidence of bias on the estimates?

#
#
#
#
#
#
#
#
#



#EXERCISE  1.1 Solution

# analyzing according to the correct model by fixing factor variances to 1. 

analysis.model1<-'content  =~ NA*x1 + x2 + x3 + x4 
style =~ NA*x1 + l1*x1 + l1*x2 + l1*x3 + l1*x4 
content ~~ 1 * content
style ~~ 1 * style 
content ~~ 0 * style'
acq.fit1 <- cfa(analysis.model1,data=myData)
semPaths(acq.fit1)
summary(acq.fit1, fit.measures = TRUE, standardized=TRUE)




# analyzing by fixing first loading to 1 and freely estimating the factor variances 

analysis.model1<-'content  =~ 1*x1 + x2 + x3 + x4 
style =~ 1*x1 + l1*x1 + l1*x2 + l1*x3 + l1*x4 
content ~~ 0 * style'
acq.fit1 <- cfa(analysis.model1,data=myData)
semPaths(acq.fit1)
summary(acq.fit1, fit.measures = TRUE, standardized=TRUE)





# analyzing according to an incorrect analysis model 
analysis.model2<-'content  =~ x1 + x2 + x3 + x4' 
acq.fit2 <- cfa(analysis.model2,data=myData)
summary(acq.fit2, fit.measures = TRUE, standardized=TRUE)
fitMeasures(acq.fit2)
#Note:  Model 1 includes the style factor; Model 2 does not include the style factor.
#Model comparison
anova(acq.fit2,acq.fit1)


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
 
# . Use the covariance matrix above to fit the model depicted in the figure
# . Does the model fit the data well?
# . Ask lavaan to calculate these indirect and total effects. Compare to your own solution
# For those interested in
# . Inspect the regression parameters and the R-squared values of the latent variables
# . Manually calculate the indirect effect of Industrialization on Political Democracy in 1965 via Pol.  Dem. in 1960 (a*b) and its S.E. using Sobel's formula:
# . Manually calculate the total effect of Industrialization on Political Democracy in 1965



#
#
#
#
#
#
#
#
#
#
#
#



#   EXERCISE  1.2 Solution Political Democracy SEM

pol.democ.sem.model<-'pd60  =~ y1 + (l21)*y2 + (l31)*y3 + (l41)*y4 
pd65 =~ y5 + (l21)*y6 + (l31)*y7 + (l41)*y8
indus =~ x1 + x2 + x3 
y1 ~~ y5
y2 ~~ y6 
y3 ~~ y7 
y4 ~~ y8 
y2 ~~ y4 
y6 ~~ y8
pd65 ~ (b)*pd60 + (c)*indus 
pd60 ~ (a)*indus
indirect := a*b 
total := (a*b)+c'

pol.democ.sem.fit <- sem(model=pol.democ.sem.model, sample.cov=pol.democ.sem.cov,sample.nobs=75)
summary(pol.democ.sem.fit, standardized=TRUE, fit.measures=TRUE)

# Solution Using matrix notation
a <- inspect(pol.democ.sem.fit,"est")$beta["pd60","indus"]
b <- inspect(pol.democ.sem.fit,"est")$beta["pd65","pd60"] 
a.se <- inspect(pol.democ.sem.fit,"se")$beta["pd60","indus"] 
b.se <- inspect(pol.democ.sem.fit,"se")$beta["pd65","pd60"]
# Manual calculation of indirect effect
a * b
# Manual calculation of S.E. of the indirect effect
sqrt(a^2*b.se^2 + b^2*a.se^2)
# Manual calculation of the total effect
a * b + inspect(pol.democ.sem.fit,"est")$beta["pd65","indus"]



# SEM MODEL WITH A PREDICTOR

# EXAMPLE 1.2.1 PSYCHO-SOCIAL HEALTH

# Data: Umstattd-Meyer, Janke and Beaujeen (2013) measured psychosocial and physical health, 
# and personal mobility 
# Fit the SEM model to the data (see slides for the diagram)
# Are both psychological and physical health predictive of personal mobility?

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


#
#
#
#
#
#
#
#
#
#
#
#



# EXAMPLE 1.2.1 PSYCHOsoCIAL HEALTH  Solution

mobility.model <- ' PsychoSocLV=~ Dep.1 +Dep.2 +Dep.3 + SocActivity
PsyHealthLV=~ Falls + Chronic + TotActivity
PersMobility ~ PsychoSocLV + PsyHealthLV'
mobility.fit <- sem(mobility.model, sample.cov=mobility.cov, sample.nobs=6053)
semPaths(mobility.fit)
summary(mobility.fit, fit.measures=TRUE, standardized=TRUE)


# EXAMPLE 1.2.2 Alternative model- PSYCHOsoCIAL HEALTH  Solution
# If you fix factor covariance to 0
# Model fit gets worse if you dont allow a significant factor covariance.

mobility.model <- ' PsychoSocLV=~ Dep.1 +Dep.2 +Dep.3 + SocActivity
PsyHealthLV=~ Falls + Chronic + TotActivity
PersMobility ~ PsychoSocLV + PsyHealthLV
PsyHealthLV ~~ 0*PsychoSocLV
'
mobility.fit2 <- sem(mobility.model, sample.cov=mobility.cov, sample.nobs=6053)
semPaths(mobility.fit2)
summary(mobility.fit2, fit.measures=TRUE, standardized=TRUE)

anova(mobility.fit2, mobility.fit)

###############
# MIMIC MODELS
###############
 
# . MIMIC stands for "Multiple Indicators, Multiple Causes"
# . Simple example:  one factor measured by 4 indicators, and influenced by 2 causes
# . Generate some data according  to a population model:
population.model<-'f =~ .7*x1 + .6*x2 + - .5*x3 + .4*x4 
f ~ .2*y1 + .3*y2'

mimic.data<-simulateData(population.model,sample.nobs=500,standardized=TRUE)

# Typically, the measurement model is developed first, after which covariates are added.


# # EXERCISE 1.3
# 
# a.  Fit the measurement model and review the fit statistics. Also review the parameter estimates.
# How close are they to the population values?
# b.  Add one covariate/predictor and watch for changes in fit statistics and parameter estimates.
# c.  Add the second covariate/predictor and again see if fit statistics or parameter estimates change. 
# d.  Calculate the degrees of freedom for each of these models manually.
# e.  Repeat steps a-c a few times with different data sets.
# Is there a trend for the fit statistics to change (improve or deteriorate) when including covariates?

#
#
#
#
#
#
#
#
#
#





# EXERCISE  1.3:  Solution MIMIC (a-c)

model1<-'f =~ x1 + x2 + x3 + x4' 
fit1<-cfa(model1,data=mimic.data) 
model2<-'f =~ x1 + x2 + x3 + x4
f ~ y1'
fit2<-cfa(model2,data=mimic.data)
model3<-'f =~ x1 + x2 + x3 + x4 
f ~ y1 + y2'
fit3<-cfa(model3,data=mimic.data)

summary(fit1,standardized=TRUE, fit.measures=TRUE)
summary(fit2,standardized=TRUE, fit.measures=TRUE)
summary(fit3,standardized=TRUE, fit.measures=TRUE)
parameterEstimates(fit3)

anova(fit1,fit2)
anova(fit3,fit2)

# EXERCISE  1.3: Solution (d)

# . Degrees of freedom for Model 3:
# -  pieces of information (with 6 observed variables) = p(p+1)/2=21
# -  measurement part:  3 factor loadings, 4 residual variances.  Total = 7.
# -  structural part:  2 regressions, 1 residual factor variance.  Total = 3.
# -  exogenous variables: 2 variances, 1 covariance.  Total = 3.
# 
# . df = 21 - (7 + 3 + 3) = 8.


# EXERCISE  1.3: Solution (e)

# we will only look at RMSEA values for models 1 and 3
nsim <-100 # set up 100 simulations
rmsea<-rep(NA,nsim)
set.seed(114254) 
for (i in 1:nsim){
  mimic.data<-simulateData(population.model,sample.nobs=500,standardized=TRUE)
  fit1<-cfa(model1,data=mimic.data)
  fit3<-cfa(model3,data=mimic.data)
  # calculate difference in RMSEA (positive values indicate a better fit of Model 3)
  rmsea[i]<-inspect(fit1,"fit")["rmsea"]- inspect(fit3,"fit")["rmsea"]
} 
mean(rmsea)

## [1] 0.002048203

hist(rmsea,breaks=10) 

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

population.model<-'f =~ .7*x1 + .6*x2 + - .5*x3 + .4*x4 
f ~ .2*y1 + .3*y2
x3 ~ .2*y2'
set.seed(114254)
mimic.data<-simulateData(population.model,sample.nobs=500,standardized=TRUE)


###
model1<-'f =~ x1 + x2 + x3 + x4 
f ~ y1 + y2'
fit1 <- cfa(model=model1,data=mimic.data)
summary(fit1,fit.measures=TRUE, stand=TRUE, mod=TRUE) 

###
model1b <- 'f =~ x1 + x2 + x3 + x4
f ~ y1 + y2
x1 ~ 0*y1 + 0*y2 
x2 ~ 0*y1 + 0*y2 
x3 ~ 0*y1 + 0*y2 
x4 ~ 0*y1 + 0*y2'
fit1b <- cfa(model=model1b,data=mimic.data)
summary(fit1b,fit.measures=TRUE, stand=TRUE, mod=TRUE)
compareFit(fit1, fit1b)

###
model2<-'f =~ x1 + x2 + x3 + x4 
f ~ y1 + y2
x3 ~ y2'
fit2 <- cfa(model=model2,data=mimic.data)
# summary(fit2,fit.measures=TRUE, stand=TRUE, mod=TRUE)
anova(fit1,fit2)




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


#
#
#
#
#
#
#
#
#
#






# EXERCISE 2 Solution STEP 1 E/CFA


setwd("C:/Users/ahu.alanya/Desktop/SEM_2020/Week2/SEM_lab_Week2_toledo_2020")

list.files()
dir()
library(rio)

# Fix this function for a .dat file
#s <- read.csv(ZA4977_sem, header = TRUE)

#import from the interface if the function does not work,
# and save as a data frame
s <- data.frame(import("ZA4977_sem.dat"))

model<- 'health =~ NA*n171 + n172 + n173 + n174 + n175 + n176 + n165 + 0*n166 + n167
attitude =~ NA*n171 + n172 + n173 + n174 + 0*n175 + n176 + n165 + n166 + n167
health ~~ 1*health
attitude ~~ 1*attitude'
fit<-cfa(model,data=s)
summary(fit,fit.measures=TRUE,standardized=TRUE, modindices=TRUE)
#extract modification indices
fit.mi <- modindices(fit)
fit.mi
ls(fit.mi)
fit.mi[order(fit.mi$mi,decreasing=TRUE),]


# EXERCISE 2 Solution STEP 2 
# 
# --  removing non-significant factor loadings and low factor loadings that are
# --  as expected (i.e. loading on the "wrong" factor)

model<-'health =~ NA*n171 + n172 + n173 + n174 + n175 + n176 + 0*n165 + 0*n166 + 0*n167
attitude =~ 0*n171 + 0*n172 + 0*n173 + 0*n174 + 0*n175 + 0*n176 + n165 + n166 + n167
health ~~ 1*health
attitude ~~ 1*attitude'
fit<-cfa(model,data=s)
summary(fit,fit.measures=TRUE,standardized=TRUE, modindices=TRUE)
#extract modification indices
fit.mi <- modindices(fit)
fit.mi
ls(fit.mi)
fit.mi[order(fit.mi$mi,decreasing=TRUE),]


# EXERCISE 2 Solution STEP 3

# -- removing item n171 because it seems like a "trouble maker" 
# (see the MI's, largest MI's involve n171)
# n171 ~~ n174 : 77.059 (expected correlation: -.27)
# n171 ~~ n176 : 71.198 (expected correlation:  .25)
# -- an alternative is to allow these error covariances

model<-'health =~ NA*n172 + n173 + n174 + n175 + n176 + 0*n165 + 0*n166 + 0*n167
attitude =~ 0*n172 + 0*n173 + 0*n174 + 0*n175 + 0*n176 + n165 + n166 + n167
health ~~ 1*health
attitude ~~ 1*attitude'
fit<-cfa(model,data=s)
summary(fit,fit.measures=TRUE,standardized=TRUE, modindices=TRUE)
#extract modification indices
fit.mi <- modindices(fit)
fit.mi
ls(fit.mi)
fit.mi[order(fit.mi$mi,decreasing=TRUE),]


# EXERCISE 2 Solution STEP 4 Final measurement model

# there is one large MI (44) for an error covariance between n173 and n174 (cancers
# and asthma). The expected correlation between the residuals is a moderately
# strong one (0.21).
# I don't see a strong theoretical basis for allowing this error covariance, so I'm not
# going to introduce it. Maybe this is a country-specific thing?
# Also note the moderately large MI (24.913) for n166 ~~ n167 and the strong expected
# correlation (.664). The two items are worded as follows:
# n166: Alcohol advertising targeting young people should be banned in all EU member states
# n167: Selling and serving alcohol to people under 18 years should be banned in all EU member states
# So these items are quite similar (both about banning something for young people), so
# I will introduce the error covariance


model<-
  'health =~ NA*n172 + n173 + n174 + n175 + n176 + 0*n165 + 0*n166 + 0*n167
attitude =~ 0*n172 + 0*n173 + 0*n174 + 0*n175 + 0*n176 + n165 + n166 + n167
health ~~ 1*health
attitude ~~ 1*attitude
n166 ~~ n167'
fit<-cfa(model,data=s)
summary(fit,fit.measures=TRUE,standardized=TRUE, modindices=TRUE)

#The model fit is not really good (we can obtain a better fit 
# if we allow an error covariance between n173 and n174 (RMSEA=0.035) 
#but without theoretical basis and no convincing post-hoc explanation, this seems like overfitting the model

# Predicting factor scores
f.scores <- predict(fit)
                                  
                                  
 # EXERCISE 2 Solution STEP 5 SEM model
                                  
 model<- 'health =~ NA*n172 + n173 + n174 + n175 + n176 + 0*n165 + 0*n166 + 0*n167
attitude =~ 0*n172 + 0*n173 + 0*n174 + 0*n175 + 0*n176 + n165 + n166 + n167
 health ~~ 1*health
 attitude ~~ 1*attitude
 n166 ~~ n167
 attitude ~ health'
 fit<-sem(model,data=s)
 summary(fit,fit.measures=TRUE,standardized=TRUE, modindices=FALSE)
 
 
 # EXERCISE 2 Solution STEP 6 Adding control variables
 
 model<- 'health =~ NA*n172 + n173 + n174 + n175 + n176 + 0*n165 + 0*n166 + 0*n167
attitude =~ 0*n172 + 0*n173 + 0*n174 + 0*n175 + 0*n176 + n165 + n166 + n167
health ~~ 1*health
attitude ~~ 1*attitude
n166 ~~ n167
attitude ~ health + male + age + ctry_nl + ctry_it + ctry_de
health ~ male + age + ctry_nl + ctry_it + ctry_de'
 fit<-sem(model,data=s)
 summary(fit,fit.measures=TRUE,standardized=TRUE, modindices=FALSE)
 
 
                                  
                                  


