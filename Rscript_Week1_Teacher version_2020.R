

# Set your working directory
getwd()
setwd("C:\\Users\\ahu.alanya\\Desktop\\SEM_2020\\Week1\\AA_W1")
 

## Install packages
install.packages("lavaan")
install.packages("lavaan.survey")
install.packages("semPlot")
install.packages("ggcorrplot")
install.packages("semTools")

# install.packages("corrplot")
# install.packages("psych")
# install.packages("ggplot2")

#load the packages, needed one time per session  
library(semPlot)
library(lavaan)
library(ggcorrplot)
library(semTools)


## Example data sets
data(package = "lavaan")
# example(cfa)


## First, look at the data
# This dataset contains 472 rows (subjects), and 5 variables (x1, x2, x3, x4, x5).
ds<-read.table(file="session1.csv", sep=",",header=TRUE)
nrow(ds) # number of subjects 
ncol(ds) # number of variables 
names(ds) # names of variables
attach(ds)
View(ds)

# Check the distributions of the manifest vars
hist(x3, col="green")

     
# Inspect the Covariance matrix
cov(ds) # covariance matrix
corr <- round(cor(ds),1)
fitted(fit)
print(cov(x1,x2)) #estimating covariance between x1 and x2 directly
print(cor(x1,x2))
# Visualize correlations
ggcorrplot(corr)


## ONE FACTOR MODEL

model<-'f =~ NA*x1 + x2 + x3 + x4 + x5
f~~1*f' 
fit <- cfa(model, data=ds)
summary(fit, standardized=T, fit.measures=T)

#Parameter estimates and fit measures
summary(fit, fit.measures = TRUE, standardized=TRUE,rsquare=T) #Rsquares are squared factor loadings. 
parameterEstimates(fit)  #Shows parameters by operator
resid(fit, type = "standardized")
fitted(fit)
fitMeasures(fit)
fitMeasures(fit, c("cfi", "rmsea", "srmr"))  #selection of fit measures
fitMeasures(fit, "bic") # gives only one
inspect(fit) # gives model matrices, the free parameters are nonzero integers
#Theta matrix shows variances/covariances of the measurement error



#  Modification Indices 
# "lhs"=left-hand-side, "op"=operator, "rhs"=right-hand-side, and "est.std"=standardized estimates
# two extra columns (sepc.lv and sepc.all) contain standardized values for the epc's (expected paramater change). 
# In the first column (sepc.lv), standardization is based on the variances of the (continuous)
# latent variables. In the second column (sepc.all), standardization is based on both the variances of both
# (continuous) observed and latent variables. (Residual) covariances are standardized using (residual) 
# variances. 
# sepc.nox: standardizing all but exogenous observed variables.  If a variable has at least one arrow pointing to it,
# it is endogenous, otherwise it is exogenous. 

summary(fit, modindices=TRUE)
modificationIndices(fit)
modindices(fit) # prints all MI hard to read
modindices(fit,sort=T)
mi<-inspect(fit,"mi")
mi.sorted<- mi[order(-mi$mi),]  # sort from high to low
mi.sorted[1:5,] # only display some large MI values

mi <- modindices(fit) #you can also select by operator
mi[mi$op == "=~",]


#Draw a path diagram of the model
library(semPlot)
semPaths(fit,"model","stand",style="LISREL",rotation=1, edge.color="black",edge.label.cex=1,mar=c(10,1,2,1))



## EXERCISE 0 Alternative model

#Question: How would override the default in R and freely estimate the loading of x1,
# fix variance of f to 1 instead.
#Does the model fit change? What changes in the output from summary?

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
#




#EXERCISE 0 ANSWER
model.alternative<-'f =~ NA*x1 + x2 + x3 + x4 + x5 
f ~~ 1*f'
fit.revised0 <- cfa(model.alternative, data=ds)
summary(fit.revised0, modindices=TRUE, standardized=TRUE)
anova(fit,fit.revised0)
semPaths(fit.revised0,"model","stand",style="LISREL",rotation=1, edge.color="black",edge.label.cex=1,mar=c(10,1,2,1))

# semTools functions can be used for model comparison
install.packages("semTools")
library(semTools)
compareFit(fit,fit.revised0,nested=T)
# Model with the lowest AIC is the best fitting model
# unlike other fit indices, aic can be computed for models with zero degrees of freedom (just-identifed models)




#EXERCISE  1.1

# Modify model based on a review of:
# MI's in combination with EPC's (Expected Value Change)  --Normaaly both  need to be "substantial"
# Theory or the source of the data  (e.g. review the content of the test  items)

# Modifying a CFA moves it away from a strictly confirmatory model
# The more modifications,  the more exploratory the model becomes
# Maybe this model was not ready  for a confirmatory modeling strategy? 

# Modify the model by including the error covariance, refit the model
# Review the parameter estimates and compare to the first model
# Review the fit statistics (global and local)
# Verify that the new model indeed fits the data better (perform  a chi-squared  difference test)

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




#EXERCISE  1.1 ANSWER

model.revised <- 'f =~ x1 + x2 + x3 + x4 + x5 
x2 ~~ x3'
fit.revised <- cfa(model.revised, data=ds)
summary(fit.revised, fit.measures = TRUE, standardized=TRUE)
anova(fit,fit.revised) # compare nested models with anova
compareFit(fit, fit.revised,nested=T)
parameterEstimates(fit.revised)

#EXERCISE  1.2

# Consider modifying the model further based on the MIs, revise the model further.
# Review the parameter estimates and compare to the two revised models.
# Compare	RMSEA	SRMR	CFI	TLI	AIC for Revised m1 and Revised m2							

#
#
#
#
#
#
#
#

#





#EXERCISE  1.2 ANSWER
model.revised2 <- 'f =~ x1 + x2 + x3 + x4 + x5 
x2 ~~ x3
x4 ~~ x5'
fit.revised2 <- cfa(model.revised2, data=ds)
summary(fit.revised2, fit.measures = TRUE, standardized=TRUE)
anova(fit.revised,fit.revised2) # compare nested models with anova
compareFit(fit.revised, fit.revised2,nested=T)
parameterEstimates(fit.revised)





## TWO FACTOR MODEL

# . Social psychological experiment by Reisenzein (1986) on "helping behavior"
# 
# -  Hypothetical story  about  a person collapsing and lying on a subway  floor
# -  Half the subjects  are told the person was drunk,  the other  half that the person was ill
# -  Do feelings of sympathy and anger mediate  the likelihood of helping the victim?
# 
# . Focus is on latent variables "sympathy" and "anger"
# 
# -  x1 "How much sympathy would you feel for that person?"  (1=none at all, 9=very much)
# -  x2 "I would feel pity  for this person"  (1=none at  all, 9=very much)
# -  x3 "How much concern would you feel for this person?"  (1=none at  all, 9=very much)
# -  x4 "How angry  would you feel at  that person?"  (1=not at  all, 9=very much)
# -  x5 "How irritated would you feel by that person?"  (1=not at  all, 9=very much)
# -  x6 "I would feel aggravated by that person"  (1=not at  all, 9=very much so)
# head(HolzingerSwineford1939)

#hint: you can use CTRL+Shift+C in Rstudio for commenting out a block of code.

#Data
reis.lower<-'
6.982
4.686	6.047		
4.335	3.307	5.037	
-2.294	-1.453	-1.979	5.569
-2.209	-1.262	-1.738	3.931 5.328
-1.671	-1.401	-1.564	3.915 3.601 4.977'

# convert to a full symmetric covariance with names
reis.cov<-getCov(reis.lower,names=c("x1","x2","x3","x4","x5","x6"))
cor(reis.cov)

#EXERCISE  1.3

# Write the  Two-factor model and fit the model
# Review the model parameters and model fit.
# Request the model-implied covariance  matrix  using lavaan's  inspect  function.
# Compare it to the covariance matrix from the data
# number of cases 138


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
#



#EXERCISE  1.3 ANSWER

reis.model<-'sympathy =~ x1 + x2 + x3 
anger =~ x4 + x5 + x6'
reis.fit<-cfa(reis.model,sample.cov=reis.cov,sample.nobs=138) 
inspect(reis.fit,"cov.ov")  # sigma.hat
fitted(reis.fit)
summary(reis.fit, fit.measures = TRUE, standardized=TRUE)
modificationindices(reis.fit)


inspect(reis.fit,"est")$lambda # unstandardized loadings matrix
inspect(reis.fit,"est")$psi  # factor variace covariances
inspect(reis.fit,"est")$theta  #error variances
t() # function for transposing a matrix.
#calculation of implied cov matrix using matrix operations in R
inspect(reis.fit,"est")$lambda %*% inspect(reis.fit,"est")$psi %*%
  t(inspect(reis.fit,"est")$lambda) +
  inspect(reis.fit,"est")$theta



#EXERCISE  1.4
# . Do we really need two factors? Fit one factor model and verify that two factor model is appropriate.
# . Review the model parameters and model fit.
# chi2	Df	RMSEA	SRMR	CFI	TLI	AIC















#EXERCISE  1.4 ANSWER
reis.model2<-'sympathy =~ x1 + x2 + x3 + x4 + x5 + x6'
reis.fit2<-cfa(reis.model2,sample.cov=reis.cov,sample.nobs=138) 
fitMeasures(reis.fit2)
parameterEstimates(reis.model2)
fitMeasures(reis.fit, c("chisq", "df", "pvalue", "cfi", "rmsea", "srmr", "aic", "bic"))  #selection of fit measures
fitMeasures(reis.fit2, c("chisq", "df", "pvalue", "cfi", "rmsea", "srmr", "aic", "bic"))  
anova(reis.fit,reis.fit2)


## SECOND-ORDER CFA

# . Factor analysis assumes that relatively few underlying latent variables may underlie a large number of indicators
# . This idea can be extended:  more general and abstract latent variables may determine the "first-order" latent variables
# . We will analyze data from Marsh and Hocevar (1985) on "Self-concept" for 251 fifth-graders in Sydney, Australia. Their publication contains summary data (means, standard deviations and correlations) which we can use to replicate (parts of) their analysis.

# Data
# 
# . Self-Description Questionnaire (SDQ), designed to measure four non-academic  aspects:
# -  Physical  Ability
# -  Physical  Appearance
# -  Relations  with Peers
# -  Relations  with Parents
# 
# . and three  academic  aspects:
# -  Reading
# -  Mathematics
# -  General  School
# 
# . Each  aspect  is represented by four variables, each  being  the  total response  to  2 items  designed  to measure  the same SDQ dimension.
# . We will focus on the non-academic aspects for fifth graders.  The question is whether  these four aspects are four dimensions  of a more general, "non-academic self-concept"  factor?
# 
# Data - correlation matrix

lower<-'
1.00
.31 1.00
.52 .45 1.00
.54 .46 .70 1.00
.15 .33 .22 .21 1.00
.14 .28 .21 .13 .72 1.00
.16 .32 .35 .31 .59 .56 1.00
.23 .29 .43 .36 .55 .51 .65 1.00
.24 .13 .24 .23 .25 .24 .24 .30 1.00 
 .19	.26	.22	.18	.34	.37	.36	.32	.38	1.00
.16	.24	.36	.30	.33	.29	.44	.51	.47	.50 1.00
.16	.21	.35	.24	.31	.33	.41	.39	.47	.47 .55 1.00
.08	.18	.09	.12	.19	.24	.08	.21	.21	.19 .19 .20 1.00
.01 -.01 .03 .02 .10 .13 .03 .05 .26 .17 .23 .26 .33 1.00
.06 .19 .22 .22 .23 .24 .20 .26 .16 .23 .38 .24 .42 .40 1.00
.04 .17 .10 .07 .26 .24 .12 .26 .16 .22 .32 .17 .42 .42 .65 1.00'

sd<-c(1.84,1.94,2.07,1.82,2.34,2.61,2.48,2.34,1.71,1.93,2.18,1.94,1.31,1.57,1.77,1.47)

marsh.cov<-getCov(lower,sds=sd,names=c("phyab1","phyab2","phyab3","phyab4","appear1", 
                                       "appear2","appear3","appear4","peerrel1","peerrel2",
                                       "peerrel3","peerrel4","parrel1","parrel2","parrel3",
                                       "parrel4"))



# Specifying the second order factor model
# Make sure you specify the correct number of observations!

marsh.model<-'phys =~ phyab1 + phyab2 + phyab3 + phyab4 
appear =~ appear1 + appear2 + appear3 + appear4
peerrel =~ peerrel1 + peerrel2 + peerrel3 + peerrel4 
parrel =~ parrel1 + parrel2 + parrel3 + parrel4 
selfConcept =~ phys + appear + peerrel + parrel'
marsh.fit<-cfa(model=marsh.model,sample.cov=marsh.cov,sample.nobs=251)
summary(marsh.fit, fit.measures = TRUE, standardized=TRUE)

#EXERCISE  1.5 Results from the second order CFA

# a.  Review the model output using the summary  function.  Does the model fit well?
# b.  Review the  R? values  of the  first-order latent variables. Which  first-order factor  is explained  best  by the second-order  factor?
# c.  Request  model modification  indices and explore model modifications.

#
#
#
#
#
#
#

#EXERCISE  1.5  ANSWER
# When do we use second order model?
# David Kenny: The major uses of a second-order are as follows:  
# First, one has a construct but finds that it is multi-dimensional but by creating a second-order factor one can preserve the construct.  
# Second, if a set of latent variables all cause the same construct, their collinearity may difficult to separate their effects, but by having the causality work through a single second-order factor, the collinearity is reduced. Third, by having just one latent variable instead of many, a second-order model is more parsimonious.   
# Note that first-order factors have disturbances which should not be viewed as measurement error. The disturbances reflect variance in the first-order factor not explained by the second order factor. 
# The disturbance variance should be non-trivial and statistically significant. If not, then the first-order factor essentially correlates perfectly with second-order factor and the two are no different.  Heywood cases and negative disturbance variances are conceptually problematic. The researcher needs to check to be sure that they do not exist.


summary(marsh.fit, fit.measures = TRUE, standardized=TRUE, rsquare=T)
modindices(marsh.fit,sort=T)

# alternatively
inspect(marsh.fit,"r2")
mi<-inspect(marsh.fit,"mi")
mi.sorted<-mi[order(-mi$mi),]  # sort from high to low
mi.sorted[1:5,] # only display some large MI values


#
#
#
#
#
#
#
#

# EXERCISE  1.6  Comparison to a first-order CFA model
# 
# a.  Omit  the  second-order latent variable "selfConcept" from  the  initial  second-order CFA  model  and re-estimate it.
# b.  How many  degrees of freedom are lost?  Why?
# c.  Statistically compare  model  fit:  does the  second-order factor  model  fit significantly worse than the first-order  factor  model?

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



#EXERCISE  1.6 ANSWER
marsh.model2<-'phys =~ phyab1 + phyab2 + phyab3 + phyab4 
appear =~ appear1 + appear2 + appear3 + appear4
peerrel =~ peerrel1 + peerrel2 + peerrel3 + peerrel4 
parrel =~ parrel1 + parrel2 + parrel3 + parrel4'

marsh.fit2<-cfa(model=marsh.model2,sample.cov=marsh.cov,sample.nobs=251)
summary(marsh.fit2, fit.measures = TRUE, standardized=TRUE)
modificationIndices(marsh.fit2, sort=T)

anova(marsh.fit,marsh.fit2)
compareFit(marsh.fit,marsh.fit2,nested=T)
#note the degrees of freedom difference between the two models. Second order model is more parsimonious.Why?
#Because first-order factor covariances are not estimated. compare the following output. 
parameterEstimates(marsh.fit)
parameterEstimates(marsh.fit2)





############################################
#EXERCISE  1.7 Holzinger-Swineford model
############################################

# . Fit the model  as depicted in the diagram. Fix  the factor loading of variables x1,   x4,   and   x7 to
# 1   so   as   to   scale   the   latent variables spt   ("spatial"),   vrb   ("verbal"),   and   spd   ("speed"). 


#HolzingerSwineford1939 dataset
# . id : identifier
# . sex : gender
# . ageyr : age (year  part)
# . agemo : age (months part)
# . school : school (Pasteur / Grant-White)
# . grade : grade
# . x1 : visual perception
# . x2 : cubes
# . x3 : lozenges
# . x4 : paragraph comprehension
# . x5 : sentence  completion
# . x6 : word meaning
# . x7 : speeded addition
# . x8 : speeded counting  of dots
# . x9 : speeded discrimination straight and curved  capitals 


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


#EXERCISE  1.7 ANSWER
hs.model<-'spatial =~ x1 + x2 + x3
verbal =~ x4 + x5 + x6
speed =~ x7 + x8 + x9'
hs.fit<-cfa(model=hs.model,data=HolzingerSwineford1939)
summary(hs.fit)

#EXERCISE  1.8:  Holzinger-Swineford model
# a.  Fit  the same model, but  fix the factor variances  to 1 and freely estimate  all factor loadings.  Are there any differences in model fit? Are there any differences in factor loadings that were also freely estimated in 1.7?
# b.  Review the modification  indices.  Can the model fit be improved?
# c.  Allow variable x9 to load on "spatial"  and refit the model. Has the model fit been improved?  Is there a theoretical basis to defend this particular model change?
# d.  Fit a second order CFA. Set the second factor variance equal to 1 and freely estimate  the factor loadings from the  first order  
# latent variables on the  second order  latent variable. What has happened to the model fit?  Why?

#
#
#
#
#
#
#
#
#



#EXERCISE  1.8a SOLUTION

hs.model2<-'spatial =~ NA*x1 + x2 + x3 
verbal =~ NA*x4 + x5 + x6
speed =~ NA*x7 + x8 + x9 
spatial ~~ 1*spatial 
verbal ~~ 1*verbal
speed ~~ 1*speed'
hs.fit2<-cfa(model=hs.model2,data=HolzingerSwineford1939)
summary(hs.fit2, fit.measures = TRUE, standardized=TRUE)







#EXERCISE  1.8b SOLUTION
mi<-inspect(hs.fit,"mi")
mi.sorted<-mi[order(-mi$mi),]  # sort from high to low
mi.sorted[1:5,] # only display some large MI values







#EXERCISE  1.8c SOLUTION
hs.model3<-'spatial =~ x1 + x2 + x3 + x9 
verbal =~ x4 + x5 + x6
speed =~ x7 + x8 + x9'
hs.fit3<-cfa(model=hs.model3,data=HolzingerSwineford1939)
summary(hs.fit3, fit.measures = TRUE, standardized=TRUE)
anova(hs.fit,hs.fit3)
compareFit(hs.fit,hs.fit3,nested=T)







#EXERCISE  1.8d SOLUTION
hs.model4<-'spatial =~ x1 + x2 + x3 + x9 
verbal =~ x4 + x5 + x6
speed =~ x7 + x8 + x9
general =~ NA*spatial + verbal + speed 
general ~~ 1*general'
hs.fit4<-cfa(model=hs.model4,data=HolzingerSwineford1939)
summary(hs.fit4, fit.measures = TRUE, standardized=TRUE)
anova(hs.fit4,hs.fit3)
compareFit(hs.fit3,hs.fit4,nested=T)

























