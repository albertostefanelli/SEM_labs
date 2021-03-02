
setwd("C:/Users/ahu.alanya/Desktop/SEM_Labs_2019/Week3/AA_w3")
getwd() 

## Install packages
#install.packages("lavaan")
#install.packages("lavaan.survey")
#install.packages("semPlot")
# install.packages("corrplot")
# install.packages("psych")
# install.packages("ggplot2")
# install.packages("semTools")

#load the packages, needed one time per session  
library(semPlot)
library(lavaan)
library(semTools)


############################
## MGSEM
###########################

# Lavaan syntax 

ds<-read.table("session3") 
model<-'f =~ x1 + x2 + x3 + x4'
View(ds)
hist(ds$x4)
mean(ds$x4)


# 1. CONFIGURAL EQUIVALENCE
## Add the "meanstructure" argument to add means/intercepts
fit1<-cfa(model,data=ds,group="group", meanstructure=T)

# show selected fit measures
selected <- c("chisq", "df", "rmsea", "cfi")
fitMeasures(fit1, selected)
summary(fit1, standardized=T, modindices=T, rsquare=T, fit.measures=T)

#Extract MI's from modindices function
mi <- modindices(fit1)
mi[mi$op == "=~",]
mi <- modindices(fit1, sort = T, free.remove = F)
mi[mi$op == "=~",]


###


# 2. METRIC EQUIVALENCE: set the factor loadings equal across groups
fit2<-cfa(model,data=ds,group="group",group.equal=c("loadings"), meanstructure=T)

# show selected fit measures
selected <- c("chisq", "df", "rmsea", "cfi")
fitMeasures(fit2, selected)
summary(fit2, standardized=T, modindices=T, rsquare=T, fit.measures=T)


# extract the list of parameters with operators
# this is easier to read and see the equality constraints, 
# shows how parameters are labelled. 
# The ones with the same labels are set equal between the groups.
fit2.parameters <- parameterestimates(fit2)
fit2.parameters
#extract modification indices
fit2.mi <- modindices(fit2)
fit2.mi
# subset MI for loadings only
loadings <- fit2.mi[fit2.mi$op=="=~",]
loadings[order(loadings$mi,decreasing=TRUE),]


summary(fit2, standardized=T, modindices=T, rsquare=T, fit.measures=T)


####


# 3. SCALAR EQUIVALENCE: set the factor loadings and the intercepts equal across groups
fit3<-cfa(model,data=ds,group="group",group.equal=c("loadings","intercepts"), meanstructure=T)
anova(fit1,fit2)
anova(fit2,fit3)
fitmeasures(fit3, fit.measures=selected)
summary(fit3, standardized=T, modindices=T, rsquare=T, fit.measures=T)
# Need to set free.remove to FALSE to be able to get Modification Indices for intercepts.
mi3 <- modindices(fit3, sort = T, free.remove = F)
mi3[mi3$op == "~1",]



# 4. METRIC --PARTIAL EQUIVALENCE
# metric equivalence: set the factor loadings equal across groups, but free one loading
# you can do this in the model specification or by group.partial argument.
model<-'f =~ NA*x1 + x2 + x3 + x4
f~~1*f'

fit2b<-cfa(model,data=ds,group="group",group.equal=c("loadings"),group.partial="f=~x2",meanstructure=T)
summary(fit2b, standardized=T, modindices=T, rsquare=T, fit.measures=T)

# show selected fit measures

fitMeasures(fit1, selected)
fitMeasures(fit2, selected)
fitMeasures(fit2b, selected)
fitMeasures(fit3, selected)


# extract the list of parameters with operators
fit2b.parameters <- parameterestimates(fit2b)
fit2b.parameters
#extract modification indices
fit2b.mi <- modindices(fit2)
fit2b.mi
# subset MI for loadings only
loadings <- fit2b.mi[fit2.mi$op=="=~",]
loadings[order(loadings$mi,decreasing=TRUE),]

summary(fit2, standardized=T, modindices=T, rsquare=T, fit.measures=T)



# Alternative Lavaan Syntax 

# configural equivalence
model.conf.eq<-'f =~ x1 + x2 + x3 + x4' 
fit.conf.eq<-cfa(model.conf.eq,data=ds,group="group")

# metric equivalence: set the factor loadings equal across groups 
model.metr.eq<-'f =~ c(l1,l1)*x1 + c(l2,l2)*x2 + c(l3,l3)*x3 + c(l4,l4)*x4' 
fit.metr.eq<-cfa(model.metr.eq,data=ds,group="group")

# scalar equivalence: set the factor loadings and the intercepts equal across groups
model.scal.eq<-'f =~ c(l1,l1)*x1 + c(l2,l2)*x2 + c(l3,l3)*x3 + c(l4,l4)*x4 
x1 ~ c(t1,t1)*1
x2 ~ c(t2,t2)*1 
x3 ~ c(t3,t3)*1 
x4 ~ c(t4,t4)*1
f ~ c(a1,a2)*1 
a1 == 0

'



fit.scal.eq<-cfa(model.scal.eq,data=ds,group="group") 
summary(fit.scal.eq, standardized=T, modindices=T, rsquare=T, fit.measures=T)

# This specification gives the same fit as fit3
fitMeasures(fit.scal.eq, selected)
fitMeasures(fit3, selected)


##########################################
# Lavaan syntax - partial equivalence
##########################################
# 
# . Results suggest that setting all factor loadings equal across groups is too restrictive
# . Use model modification indexes to determine which factor loadings should be freed
# . Specify which part  of the model should not be constrained equal across groups using the group.partial parameter in the cfa function



#SemTools function showing delta cfi for fixing/freeing different parameters
#Better to look at CFI since CHi2 is sensitive to sample size
#First you need to run the measurementInvariance function. 
#Then decide where it breaks down. And use partialInvariance function to get the MIs
autoinvariance <- measurementInvariance(model, 
                      data = ds, 
                      group = "group", strict=T)
partMI.metric <- partialInvariance(autoinvariance, type="metric")
partMI.metric <- partMI.metric$results  #sort by clicking on the free.cfi column 
#note that conclusions are different compared to monindices(), not sure why!


# Specifiying the partial invariance model 
fit2b<-cfa(model,data=ds,group="group",group.equal=c("loadings"), group.partial=c("f =~ x2"))
anova(fit1,fit2b)

fitMeasures(fit2b, selected)



# . Based on "model 2b", check whether  intercepts are equal across groups
fit3b<-cfa(model,data=ds,group="group",group.equal=c("loadings","intercepts"), group.partial=c("f =~ x2"))
anova(fit1,fit3b)

fitMeasures(fit3b, selected)

partMI.scalar <- partialInvariance(autoinvariance, type = 'scalar')
partMI.scalar <- partMI.scalar$results  #sort by clicking on the free.cfi column 



fit3c<-cfa(model,data=ds,group="group",group.equal=c("loadings","intercepts"), group.partial=c("f =~ x2","x3 ~ 1"))
anova(fit1,fit3c)
fitMeasures(fit2b,selected)
fitMeasures(fit3c,selected)
#If you compare cfi, the change is lower than 0.01.The partial model fits. 
fitMeasures(fit1,"cfi")-fitMeasures(fit2b,"cfi")
summary(fit3c, standardized=T, modindices=T, rsquare=T, fit.measures=T)


y <- data.frame(predict(fit3c))


# Alternative specification. Loadings and intercepts kept  equal, except  for loading of x2 and intercept of x3 ("fit3c")
model.partial.scal.eq<-'f  =~ c(l1,l1)*x1 + c(l21,l22)*x2 + c(l3,l3)*x3 + c(l4,l4)*x4 
x1 ~ c(t1,t1)*1
x2 ~ c(t2,t2)*1
x3 ~ c(t31,t32)*1 
x4 ~ c(t4,t4)*1
f ~ c(a1,a2)*1 
a1 == 0'
fit.partial.scal.eq<-cfa(model.partial.scal.eq,data=ds,group="group")


# Comparing means

# . Once (partial) measurement equivalence  is demonstrated, the latent means can be compared
# . Use the summary  function  for object  fit3c to check difference in latent means 
# . The latent mean of the first group is set to 0; the latent mean of the second group is free to deviate.
# . The part  you need to look at is called "Intercepts", and you want to look at  the intercept of f
# . The latent mean is different in Group  2 than  in Group  1 as the intercept of f in Group  2 equals 0.169 (p=2.44e-07)
# . If we had not corrected  for partial invariance,  we would have estimated the difference in latent means as 0.044 (p=0.21)


###############################
#### EXERCISE  1  

# . Use file "exS3_1" to perform  a two-group CFA (2 factors  with 4 indicators each)
# . The variable  names are x1-x8, with x1-x4 loading on factor 1, and x5-x8 loading on factor  2
# . No cross-loadings  are assumed
# 
# 1.1 Statistically test  for measurement equivalence and compare  the latent means of both  factors across the 2 groups; are they different?
# 1.2 Statistically test  whether the  variance  of each factor is equal  across groups  (do this  in the  model that results  from Ex.  1.1 so that any measurement non-equivalence is accounted  for)
# 1.3 Statistically test  whether  the covariance between the 2 factors is equal across groups (based on the model from 1.2)
# 1.4 Build a MIMIC model in which the group variable  is used as independent variable  and test  whether  you reach the same conclusions as with the MGCFA  approach  regarding  measurement equivalence and difference in means


#EXERCISE  1 Solution

ds<-read.table("exS3_1")
View(ds)

# write the model
model<-'f1 =~ x1 + x2 + x3 + x4 
f2 =~ x5 + x6 + x7 + x8'


#fit the model - configural
fit1<-cfa(model,data=ds,group="group")
selected <- c("chisq", "df", "rmsea", "cfi")
summary(fit1, standardized=T, modindices=T, rsquare=T, fit.measures=T)

#fit the model -loadings
fit2<-cfa(model,data=ds,group="group",group.equal=c("loadings"))
summary(fit2, standardized=T, modindices=T, rsquare=T, fit.measures=T)

fitMeasures(fit1, selected)
fitMeasures(fit2, selected)
anova(fit1,fit2)


#fit the model -intercepts
fit3<-cfa(model,data=ds,group="group",group.equal=c("loadings","intercepts"))
summary(fit3, standardized=T, modindices=T, rsquare=T, fit.measures=T)


fitMeasures(fit2, selected)
fitMeasures(fit3, selected)
anova(fit1,fit2)


# extract fit measures
fitMeasures(fit1, selected)
fitMeasures(fit2, selected)
fitMeasures(fit3, selected)




#EXERCISE  1.1 Solution

# 1.1 Statistically test  for measurement equivalence and compare  the latent means of both  factors across the 2 groups; are they  different?
# Use summary function to detect largest MIs - x3 and x6 appear to need different
# intercepts across groups

# check where the problem lies with the model that does not satisfy the CFI difference criteria. 
# use different SemTools
x <- data.frame(lavTestScore(fit3))
autoinvar <- measurementInvariance(model, group="group", data=ds)
partmi <- partialInvariance(autoinvar, type="scalar")
partmi <- partmi$results
# They are in line p22 refers to the parameter x3~1 (intercept) in the first group
# so that equality needs to be revised.

## TIP 
## How to extract modindices that are relevant to group equality
#partial metric invariance 
modindices(fit3)
parameterestimates(fit3)

#Modified partial invariance model
fit3b<-cfa(model,data=ds,group="group",group.equal=c("loadings","intercepts"), group.partial=c("x3 ~ 1","x6 ~ 1"))
anova(fit1,fit3b)

fitMeasures(fit3b, selected)
fitMeasures(fit2, selected)

# Check overall model fit of specification 3b
fitMeasures(fit3b, selected)
#extract modification indices
fit3b.mi <- modindices(fit3b)
# subset MI for loadings only
loadings <- fit2.mi[fit2.mi$op=="=~",]
loadings[order(loadings$mi,decreasing=TRUE),]

summary(fit3b, standardized=T, modindices=T, rsquare=T, fit.measures=T)














##############################################################################
# To get modindicies for intercepts you need to fix them to fix values.
##############################################################################

Df = read.table("exS3_1")
Model <- '
f1 =~ NA * x1 + x2 + x3 + x4
f2 =~ NA * x5 + x6 + x7 + x8
f1 ~~ 1 * f1
f2 ~~ 1 * f2
'
Model <- '
f1 =~ x1 + x2 + x3 + x4
f2 =~ x5 + x6 + x7 + x8
'

Model <- '
f1 =~ x1 + x2 + x3 + x4
f2 =~ x5 + x6 + x7 + x8
x1 + x2 + x3 + x4 ~ 0 * 1
x5 + x6 + x7 + x8 ~ 0 * 1
'
# Constrain intercepts to fixed values

ConfFit = cfa(Model, data = Df, group = "group")
summary(ConfFit, standardized = T)
fitMeasures(ConfFit, selected)

summary(
  ConfFit,
  standardized = T,
  modindices = T,
  rsquare = T,
  fit.measures = T
)

head(modindices(ConfFit, sort. = T))
#############################################
###############################################








#EXERCISE  1.2 Solution
# 1.2 Statistically test  whether the  variance  of each factor  is equal across groups  
# do this  in the  model that results  from Ex.1.1 so that any measurement non-equivalence is accounted for)
# you cannot specify this with measurementInvariance()

model3 <- "f1 =~ c(l1,l1)*x1+ c(l2,l2)*x2 + c(l3,l3)*x3 + c(l4,l4)*x4 
f2 =~ c(l5,l5)*x5 + c(l6,l6)*x6 + c(l7,l7)*x7 + c(l8,l8)*x8
x1 ~ c(t1,t1)*1 
x2 ~ c(t2,t2)*1
x3 ~ c(t31,t32)*1      
# free intercept for x3 across groups 
x4 ~ c(t4,t4)*1 
x5 ~ c(t5,t5)*1
x6 ~ c(t61,t62)*1     
# free intercept for x6 across groups 
x7 ~ c(t7,t7)*1
x8 ~ c(t8,t8)*1
f1 ~ c(a11,a12)*1 
f2 ~ c(a21,a22)*1
a11 == 0               # set intercept of f1 to 0 in first group 
a21 == 0               # set intercept of f2 to 0 in first group
f1 ~~ c(psi11,psi11)*f1   # equal variance of factor 1 across groups 
f2 ~~ c(psi22,psi22)*f2   # equal variance of factor 2 across groups" 
fit4<-cfa(model3,data=ds,group="group")
summary(fit4, standardized=T, modindices=T, rsquare=T, fit.measures=T)

anova(fit3b,fit4)
fitMeasures(fit3b, selected)
fitMeasures(fit4, selected)



#It doesnt set factor means equal since they are set to 0 for identification. You can manually specify the model to override this.
fit4a<-cfa(model,data=ds,group="group",group.equal=c("loadings","intercepts", "means"), group.partial=c("x3 ~ 1","x6 ~ 1"))
summary(fit4a, standardized=T, modindices=T, rsquare=T, fit.measures=T)
fitMeasures(fit4a, selected)
fitMeasures(fit4, selected)


fit4c<-cfa(model,data=ds,group="group",group.equal=c("loadings","intercepts", "lv.variances"), group.partial=c("x3 ~ 1","x6 ~ 1"))
summary(fit4c, standardized=T, modindices=T, rsquare=T, fit.measures=T)



fitMeasures(fit4, selected)
fitMeasures(fit4.1, selected)

fit4.1<-cfa(model,data=ds,group="group",group.equal=c("loadings","intercepts", "lv.variances","means"), group.partial=c("x3 ~ 1","x6 ~ 1"))




















#EXERCISE  1.3 Solution
# 1.3 Statistically test  whether  the covariance between the 2 factors is equal across groups (based on the model from 1.2)
model2<-"f1 =~ c(l1,l1)*x1+ c(l2,l2)*x2 + c(l3,l3)*x3 + c(l4,l4)*x4 
f2 =~ c(l5,l5)*x5 + c(l6,l6)*x6 + c(l7,l7)*x7 + c(l8,l8)*x8
x1 ~ c(t1,t1)*1 
x2 ~ c(t2,t2)*1
x3 ~ c(t31,t32)*1     
# free intercept for x3 across groups x4 ~ c(t4,t4)*1
x5 ~ c(t5,t5)*1
x6 ~ c(t61,t62)*1      
# free intercept for x6 across groups 
x7 ~ c(t7,t7)*1
x8 ~ c(t8,t8)*1
f1 ~ c(a11,a12)*1 
f2 ~ c(a21,a22)*1
a11 == 0                  # set intercept of f1 to 0 in first group 
a21 == 0                  # set intercept of f2 to 0 in first group
f1 ~~ c(psi11,psi11)*f1   # equal variance of factor 1 across groups 
f2 ~~ c(psi22,psi22)*f2   # equal variance of factor 2 across groups 
f1 ~~ c(psi1,psi1)*f2  # constrain the factor covariance to be equal"

fit5<-cfa(model2,data=ds,group="group") 

fitMeasures(fit4, selected)
fitMeasures(fit5, selected)

summary(fit5, standardized=T, modindices=T, rsquare=T, fit.measures=T)

















#EXERCISE  1.4 Solution
# 1.4 Build a MIMIC model in which the group variable is used as independent variable 
# and test whether you reach the same conclusions as with the MGCFA approach regarding
# measurement equivalence and difference in means

# Test for measurement non-equivalence (only intercepts of indicators are checked)

#first create a dummy variable
ds$grpB <- ifelse(ds$group=="B",1,0)
table(ds$group)
table(ds$grpB)

# Then, write the model
model<-'f1 =~ x1 + x2 + x3 + x4
f2 =~ x5 + x6 + x7 + x8
f1 ~ grpB
f2 ~ grpB
x1 ~ 0*grpB # fixing regression coefficients to 0, setting intercepts equal
x2 ~ 0*grpB 
x3 ~ 0*grpB 
x4 ~ 0*grpB 
x5 ~ 0*grpB 
x6 ~ 0*grpB 
x7 ~ 0*grpB 
x8 ~ 0*grpB'
fit.mimic<-cfa(model,data=ds)
mi<-inspect(fit.mimic,"mi") 
fitmeasures(fit.mimic, selected)
summary(fit.mimic, standardized=T, modindices=T, rsquare=T, fit.measures=T)


# MIs for the direct effects of grpB on the x-variables. 
# Conlcusions are similar for x3 and x6 
mi.sorted<-mi[order(-mi$mi),]  # sort from high to low
mi.sorted[1:3,] # only display some large MI values










#EXERCISE  1.5 Solution
# Try to set the factor means equal for the groups for factor2. 
# Remember this factor mean was significantly different between the groups
# according to multi group analysis. 


model2<-'f1 =~ x1 + x2 + x3 + x4
f2 =~ x5 + x6 + x7 + x8
f1 ~ grpB
f2 ~ grpB
x1 ~ 0*grpB # fixing regression coefficients to 0, setting intercepts equal
x2 ~ 0*grpB 
x3 ~ grpB 
x4 ~ 0*grpB 
x5 ~ 0*grpB 
x6 ~ grpB 
x7 ~ 0*grpB 
x8 ~ 0*grpB'
fit.mimic2<-cfa(model2,data=ds)
fitmeasures(fit.mimic2, selected)
summary(fit.mimic2)


#compare the fit between the two mimic models
fitmeasures(fit.mimic, "cfi")-fitmeasures(fit.mimic2, "cfi")


#######################################################
#### EXERCISE 2  Major  Depression - Gender analysis

# . On page 272 of Brown's book "Confirmatory  Factor  Analysis for Applied Research" (2006), correlation matrices  and mean and standard deviation  vectors  are given for 375 Females  and 375 Males
# . Nine variables  (mdd1-mdd9)  are supposed to measure a single latent variable  "Major Depression".  One error covariance  between  mdd1 and mdd2 is assumed  for substantive reasons
# . Read in the summary  data  and fit the 8 models that Brown presents  on page 273
# . Review the fit of each of the models and compare  to Brown's results


# Females, n=375
lower1<-'1
0.616   1
0.315   0.313   1
0.349   0.332   0.261   1
0.314   0.25    0.27    0.327   1
0.418   0.416   0.298   0.328   0.317   1
0.322   0.313   0.096   0.117   0.13    0.14    1
0.409   0.415   0.189   0.314   0.303   0.281   0.233   1
0.318   0.222   0.051   0.115   0.14    0.15    0.217   0.222   1'

sds1<-c(1.717,2.015,2.096,2.212,2.132,2.005,2.062,2.156,1.791) 
mean1<-c(4.184,3.725,1.952,3.589,2.256,3.955,3.869,3.595,1.205)

cov1<-getCov(lower1,names=paste("mdd",1:9,sep=''),sds=sds1)


# Males, n=375
lower2<-'1

0.689	1			
0.204	0.218	1		
0.335	0.284	0.315	1	
0.274	0.32	0.153	0.265	1	
0.333	0.333	0.221	0.364	0.268	1


0.258	0.211	0.114	0.139	0.185	0.132	1	
0.319	0.346	0.176	0.207	0.231	0.279	0.146	1
0.316	0.269	0.111	0.14	0.117	0.131	0.263	0.163   1'
sds2<-c(1.598,2.018,2.094,2.232,2.108,2.113,2.286,2.174,1.788)
mean2<-c(4.171,3.685,1.739,3.357,2.235,3.661,3.421,3.517,1.259)

cov2<-getCov(lower2,names=paste("mdd",1:9,sep=''),sds=sds2)


#EXERCISE 2 - Solution: fitting the model in both  groups  separately

model1<-'f =~ mdd1 + mdd2 + mdd3 + mdd4 + mdd5 + mdd6 + mdd7 + mdd8 + mdd9 
mdd1 ~~ mdd2'

# Single group analysis - Females
fit1.females<-cfa(model1,sample.cov=cov1,sample.mean=mean1,sample.nobs=375)

# Single group analysis - Males
fit1.males<-cfa(model1,sample.cov=cov2,sample.mean=mean2,sample.nobs=375)


#EXERCISE 2 - Solution: Measurement Equivalence

# Configural Equivalence
fit2<-cfa(model1,sample.cov=list(cov1,cov2),sample.mean=list(mean1,mean2), sample.nobs=list(375,375))

# Metric Equivalence (equal factor loadings)
fit3<-cfa(model1,sample.cov=list(cov1,cov2),sample.mean=list(mean1,mean2), sample.nobs=list(375,375),group.equal=c("loadings"))

# Scalar Equivalence (equal factor loadings + intercepts)
fit4<-cfa(model1,sample.cov=list(cov1,cov2),sample.mean=list(mean1,mean2), sample.nobs=list(375,375),group.equal=c("loadings","intercepts"))

# Equal indicator error variances
fit5<-cfa(model1,sample.cov=list(cov1,cov2),sample.mean=list(mean1,mean2), sample.nobs=list(375,375),group.equal=c("loadings","intercepts","residuals"))


#EXERCISE 2 - Solution: Population heterogeneity

# Equal Factor variance
fit6<-cfa(model1,sample.cov=list(cov1,cov2),sample.mean=list(mean1,mean2), sample.nobs=list(375,375), group.equal=c("loadings","intercepts","residuals","lv.variances"))
summary(fit6, standardized=T, modindices=T, rsquare=T, fit.measures=T)
anova(fit5, fit6)

# Equal Latent Mean
fit7<-cfa(model1,sample.cov=list(cov1,cov2),sample.mean=list(mean1,mean2), sample.nobs=list(375,375), group.equal=c("loadings","intercepts","residuals","lv.variances","means")) 




######################################################
####  EXERCISE 3.  Holzinger-Swineford data

# This exercise uses the HolzingerSwineford1939  dataset that comes with lavaan.
# 
# 3.1. Fit  the 3-factor model in each school separately (Grant-White and Pasteur). x1-x3 and x9 should load on the Spatial  factor;  x4-x6 on the Verbal  factor;  and x7-x9 on the Speed factor.
# 3.2. Fit  the model in both  groups simultaneously  and verify that  the sum of the Chi-squared  values from 3.1. equals the Chi-squared value in 3.2. (the  same should also hold for the df)
# 3.3. Test  for metric  equivalence;  if necessary,  build a model with partial metric  equivalence.
# 3.4. Test  for scalar equivalence;  if necessary,  build a model with partial scalar equivalence.
# 3.5a.  Compare  the latent means across the two groups:  which are statistically different and which are not?
# 3.5b.   If measurement  non-equivalence is observed:   what  if measurement  non-equivalence had  not  been accounted  for? Would the same conclusions with regard  to latent means have been attained?
# 3.6. Are all 3 factor  variances  equal across groups?











#EXERCISE 3.1 Solution
# 3.1. Fit  the 3-factor model in each school separately (Grant-White and Pasteur). x1-x3 and x9 should load on the Spatial factor; 
# x4-x6 on the Verbal  factor;  
# and x7-x9 on the Speed factor.

data(package="lavaan")
ds <-data.frame(HolzingerSwineford1939)

 
model<-'spatial =~ x1 + x2 + x3 + x9 
verbal =~ x4 + x5 + x6
speed =~ x7 + x8 + x9'
fit1.GW <- cfa(model,data=subset(HolzingerSwineford1939,school=="Grant-White"))
fit1.P  <- cfa(model,data=subset(HolzingerSwineford1939,school=="Pasteur"))

summary(fit1.GW)
summary(fit1.P)

# . Request  the summary  for both  model outputs and review them
# . Note that the model fits well in het Grant-White school, but  not well in the  Pasteur school. 
















#EXERCISE 3.2 Solution
# 3.2. Fit  the model in both  groups simultaneously  and verify that  the sum of the Chi-squared  
#  values from 3.1. equals the Chi-squared value in 3.2. (the  same should also hold for the df)
fit2 <- cfa(model,data=HolzingerSwineford1939,group="school")
summary(fit2, fit.measures=T)



# show selected fit measures
selected <- c("chi2", "df", "rmsea", "cfi")
fitMeasures(fit2, selected)
# extract the list of parameters with operators
fit2.parameters <- parameterestimates(fit3)
fit2.parameters
#extract modification indices
fit2.mi <- modindices(fit3)
fit2.mi
# subset MI for regression coefficient only
regcoeff <- fit2.mi[fit2.mi$op=="~",]
regcoeff[order(regcoeff$mi,decreasing=TRUE),]


















#EXERCISE 3.3 Solution
# 3.3. Test  for metric  equivalence;  if necessary,  
# build a model with partial metric  equivalence.
fit3 <- cfa(model,data=HolzingerSwineford1939,group="school",
            group.equal=c("loadings"))
anova(fit2,fit3)
summary(fit3)
lavTestScore(fit3) 
parameterestimates(fit3)
summary(fit3)


#EXERCISE 3.4 Solution
# 3.4. Test  for scalar equivalence;  if necessary,  
# build a model with partial scalar equivalence.
fit4 <- cfa(model,data=HolzingerSwineford1939,group="school", 
            group.equal=c("loadings","intercepts"))
anova(fit3,fit4)

# . Scalar equivalence  does not hold, review modification  indexes to pin point the problematic item(s)
fitMeasures(fit4, selected)
#extract modification indices
fit4.mi <- modindices(fit4)
fit4.mi
summary(fit4)
lavTestScore(fit4)


# . Relax equality  constraint on the intercept of x3
fit4b <- cfa(model,data=HolzingerSwineford1939,group="school", 
             group.equal=c("loadings","intercepts"), 
             group.partial=c("x3~1"))

anova(fit3,fit4b)
fitMeasures(fit4b, selected)
#extract modification indices
fit4b.mi <- modindices(fit4b)
fit4b.mi
summary(fit4b)
lavTestScore(fit4b)


# . Relax equality  constraint on the intercept of x3 and x7
fit4c <- cfa(model,data=HolzingerSwineford1939,group="school", 
             group.equal=c("loadings","intercepts"), 
             group.partial=c("x3~1","x7~1"))

anova(fit3,fit4c)

summary(fit4c)
















#EXERCISE 3.5a/b Solution

# 3.5a. Compare  the latent means across the two groups:  
# which are statistically different and which are not?
# 3.5b. If measurement  non-equivalence is observed:  
# what  if measurement  non-equivalence had  not  been accounted  for?
# Would the same conclusions with regard  to latent means have been attained?



#EXERCISE 3.5 Solution

#Taking  into account measurement non-equivalence (partial measurement equivalence): 
summary(fit4c)
# Not taking  into account measurement non-equivalence
summary(fit4)


#EXERCISE 3.6 Solution

# 3.6. Are all 3 factor  variances  equal across groups?
fit5 <- cfa(model,data=HolzingerSwineford1939,group="school",
            group.equal=c("loadings","intercepts","lv.variances"), 
            group.partial=c("x3~1","x7~1"))

anova(fit4c,fit5)

fitmeasures(fit4c, fit.measures=selected)
fitmeasures(fit5, fit.measures=selected)




##########################
#### EXERCISE 4.  MGSEM
# 
# This exercise builds on ex. 3
# We might hypothesize that  students'  abilities increase as they move to the next grade.  So we would expect to see a positive  effect of "grade"  on each of the  three  latent variables. But  is this  positive  effect observed  in both  schools? And is the effect equal in both  schools? Or would students in one school progress less quickly than  students in the other  school?
# 
# 4.1. Extend  the model from ex. 3 that  takes into account any measurement non-equivalence by including the variable  "grade".  Regress the three  latent variables  on this covariate.  Allow the regression parameters to be different across both  schools.
# 4.2.  Test  whether it is possible to constrain each of the  three  regression  parameters to be equal across the two schools. If this is not possible, develop a model which holds as many regression parameters equal across groups as possible.
# 4.3. To verify the robustness  of findings from 4.2, test  for measurement equivalence (loadings and intercepts of the indicator  variables)  of the measurement instrument by grade


# Solution 4.1
# 4.1. Extend  the model from ex. 3 that  takes into account any measurement non-equivalence by including the variable  "grade".  
# Regress the three  latent variables  on this covariate.  Allow the regression parameters to be different across both  schools.

model1.sem<-"spatial =~ x1 + x2 + x3 + x9 
verbal =~ x4 + x5 + x6
speed =~ x7 + x8 + x9 
spatial ~ grade
verbal ~ grade 
speed ~ grade"
fit1.sem <- sem(model1.sem,data=HolzingerSwineford1939,group="school", 
                group.equal=c("loadings","intercepts"), 
                group.partial=c("x3~1","x7~1"))
summary(fit1.sem, fit.measures=T, standardized=T, modindices=T)
table(HolzingerSwineford1939$grade)

# Solution 4.2
# 4.2.  Test  whether it is possible to constrain each of the  three  regression  parameters to be equal across 
# the two schools. If this is not possible, develop a model which holds as many regression parameters equal across groups as possible.

fit2.sem <- sem(model1.sem,data=HolzingerSwineford1939,group="school", 
                group.equal=c("loadings","intercepts","regressions"), 
                group.partial=c("x3~1","x7~1"))

summary(fit2.sem, fit.measures=T, standardized=T, modindices=T)

anova(fit1.sem,fit2.sem)

fitmeasures(fit1, fit.measures=selected)
fitmeasures(fit2, fit.measures=selected)


# Solution 4.3
# 4.3. To verify the robustness  of findings from 4.2, test  for measurement equivalence 
# (loadings and intercepts of the indicator  variables)  of the measurement instrument by grade

model<-"spatial =~ x1 + x2 + x3 + x9 
verbal =~ x4 + x5 + x6
speed =~ x7 + x8 + x9"

# Subset the data, removing any missing grades
hs<-subset(HolzingerSwineford1939,grade != "NA")

# Measurement equivalence - Configural Equivalence
fit3.ce <- cfa(model,data=hs,group="grade")

# Measurement equivalence - Metric Equivalence
fit3.me <- cfa(model,data=hs,group="grade", group.equal=c("loadings"))

# Measurement equivalence - Scalar Equivalence
fit3.se <- cfa(model,data=hs,group="grade", group.equal=c("loadings","intercepts"))
summary(fit3.se, standardized=T,fit.measures=T)











