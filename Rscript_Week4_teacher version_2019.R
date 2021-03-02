

setwd("C:/Users/ahu.alanya/Desktop/SEM_Labs_2019/Week4/AA_w4")
getwd() 
dir()

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


##############################
## non-normal continuous data
##############################

# Lavaan syntax 

ds <- read.table("NONML.DAT",header=FALSE,sep=" ", col.names=c("x1","x2","x3","x4","x5"))
View(ds)


# EXERCISE  1 - CFA, non-normal continuous data

# . The file "NONML.DAT"contains 5 columns (no header)
# . Read in this file and name the variables x1-x5
# 
# 1.1 Estimate a simple 5 indicator CFA (1 latent variable), without any error covariances.  
# Use ML estimation, then re-estimate using MLM. Compare the output.
# 1.2 Include an error covariance between x1 and x3 and re-estimate the model. Perform a chi-squared difference test.







# EXERCISE  1 - Solution 1.1. 
model <- 'f =~ x1 + x2 + x3 + x4 + x5'
table(ds$x1)
hist(ds$x1)

#some nonnormality tests in R
# Shapiro-Wilks test
shapiro.test(ds$x1)
# qq-plot: you should observe a good fit of the straight line
qqnorm(ds$x1)
qqline(ds$x1)

#model
fit.ml1 <- cfa(model,data=ds,meanstructure=T)
fit.mlm1 <- cfa(model,data=ds,estimator="MLM",meanstructure=T)
summary(fit.mlm1, fit.measures=T, standardized=T, modindices=T)
summary(fit.ml1, fit.measures=T, standardized=T, modindices=T)




# EXERCISE  1 - Solution 1.2.
model2 <- 'f =~ x1 + x2 + x3 + x4 + x5 
x1 ~~ x3'
fit.ml2 <- cfa(model2,data=ds,meanstructure=T)
fit.mlm2 <- cfa(model2,data=ds,estimator="MLM", meanstructure=T)
summary(fit.mlm2, fit.measures=T, standardized=T, modindices=T)

selected <- c("chisq", "df", "rmsea", "cfi","chisq.scaled", "df", "cfi.scaled", "rmsea.scaled")
selected1 <- c("chisq", "df", "rmsea", "cfi", "df")
fitMeasures(fit.mlm1, selected)
fitMeasures(fit.ml1, selected1)
fitMeasures(fit.mlm1)

anova(fit.mlm1,fit.mlm2) # Chi2 cannot be compared for ML vs MLS but for (ML vs ML) or (MLM vs MLM)
lavTestLRT(fit.mlm1,fit.mlm2)
anova(fit.ml1,fit.ml2)



#####################
# Categorical data
#####################

# WLSMV (Weighted Least Squares providing a Mean and Variance corrected chi2 value)
# cfa(..., ordered=c("y1","y2",...,"y6"),  estimator="WLSMV")
# Note that when ordered=... is specified, WLSMV is selected as default
# estimator, so estimator="WLSMV" can be omitted in the call

# EXERCISE  2 CFA categorical data
# 
# File BINARY.DAT contains data for 6 variables (y1-y6) which measure alcohol dependence in a sample of 750 participants
# . Read in this file (use function read.fwf)
# . Fit a simple one-factor CFA model (no error covariances) using WLSMV estimation
# . Review the model output (and compare to Brown, p. 393)
# . Mplus shows the tetrachoric correlation matrix by default. lavaan does not.   
#   Request it using in- spect(fit,"sampstat")$cov and compare  to the Mplus output shown in Brown, p. 393

ds.alc <- read.fwf("BINARY.DAT", widths=rep(1,6),header=FALSE, col.names=paste("y",1:6,sep=""))
table(ds.alc$y1)

model <- 'f =~ NA*y1 + y2 + y3 + y4 + y5 + y6 
f ~~ 1*f'
fit <- cfa(model,data=ds.alc,ordered=paste("y",1:6,sep=""),estimator="WLSMV")
summary(fit, fit.measures=T, standardized=T, modindices=T)
modindices(fit,free.remove = F)
fitMeasures(fit)  #robust CFI is printed here, look for  cfi.scaled and rmsea.scaled 

fit.theta <- cfa(model,data=ds.alc,ordered=paste("y",1:6,sep=""),estimator="WLSMV",parameterization="theta")
summary(fit.theta, fit.measures=T, standardized=T, modindices=T)
parameterEstimates(fit.theta)


######################
# NOTE ON LAVAAN SYNTAX
######################
# 
# New features and user-visible changes: 
#   
#   - robust RMSEA and CFI values are now computed correctly, following 
# Brosseau-Liard, P. E., Savalei, V., and Li, L. (2012), and 
# Brosseau-Liard, P. E. and Savalei, V. (2014); in the output of 
# fitMeasures(), the 'new' ones are called cfi.robust and rmsea.robust, 
# while the 'old' ones are called cfi.scaled and rmsea.scaled 
# 
# - SRMR is now displayed in the summary(, fit.measures = TRUE) output in 
# the categorical case 
# 
# - in the summary() output, a dot (.) is added in front of the names of 
# endogenous intercepts, covariances and variances; this is mostly for 
# teaching purposes, to distinguish between for example residual and plain 
# variances; the '.' prefix was the least obtrusive way I could think of; 
# feedback about this is welcome 
# 
# - the inspect/lavInspect() function will now always return a nested list 
# in the multiple group setting 
# 
# - the inspect/lavInspect() function with the "free" argument will now 
# show a header with equality constraints (if any) 
# 
# - GLS/WLS (and friends) now work when fixed.x = TRUE 
# 
# - a new argument conditional.x (TRUE/FALSE) can be used with all 
# estimators (ML, GLS, (D)WLS) 
# 
# - a two-way interaction between observed variables can now be specified 
# in the model syntax by using a colon, for example: y ~ x1 + x2 + x1:x2 
# and a product term will be created automatically 

# Released on CRAN: 24 February 2017
# New features and user-visible changes:
#   factor scores (computed by lavPredict()) are now complete, even if the items contain missing values
# Bartlett factor scores now handle singular lambda and theta matrices
# mplus2lavaan() function gains a run=FALSE argument (so it acts only as a syntax converter)
# new function lavOptions() shows the default options used by the sem/cfa/lavaan functions; all these options are now described in a single man page (see ?lavOptions)
# new functions semList(), cfaList() and lavaanList() allow for fitting the same model on multiple datasets
# the (often many) warnings about empty cells in bivariate cells (when categorical data is used) are now replaced by a single warning, and lavInspect(fit, "zero.cell.tables") can be used to see these tables

###################################
# Multiple Group CFA
###################################
# 
# . Measurement invariance
# 
# -  Factor loadings and thresholds are fixed or freed in tandem
# -  If a factor loading and corresponding  threshold(s) are freed, then the scale parameter needs to be fixed to 1
# 
# . Syntax
# y | c(label1,label1)*t1 # set threshold equal across 2 groups
# y | c(label1,label2)*t1 # set threshold free across 2 groups
# u3 ~*~ c(1,1)*u3 # fix scale of variable "u3" to 1 in both groups

# EXERCISE 4
# 
# . File "exS4_1" contains responses to 4 categorical variables (y1-y4), each having four response options.
# . The variable "group" denotes group membership (G1 and G2)
# 4.1. Fit a one-factor  model in both  groups simultaneously and test configural equivalence
# 4.2. Test for measurement non-equivalence (setting factor loadings and thresholds equal across groups)
# 4.3. If necessary, relax certain equality constraints and re-fit the model





# EXCERCISE 4.1.Solution: Fit a one-factor  model in both  groups simultaneously and test configural equivalence

ds<-read.table("exS4_1")
View(ds)
table(ds$y1)
model<-'f =~ y1 + y2 + y3 + y4'
fit<-cfa(model,data=ds,group="group",ordered=c("y1","y2","y3","y4"))
fitMeasures(fit)
selected <- c("chisq", "df", "rmsea", "cfi","chisq.scaled", "df", "cfi.scaled", "rmsea.scaled")
fitMeasures(fit, selected)
summary(fit, fit.measures=T, standardized=T, modindices=T)
modindices(fit, free.remove=F)


# Results using theta parameterization
# How does the free and fixed parameters change between theta and delta parameterization?
# When do you need to use theta parameterization? 
# When you want to test equality in residual variances you need to use theta parameterization. 
#
fit.theta<-cfa(model,data=ds,group="group",ordered=c("y1","y2","y3","y4"), parameterization="theta")
summary(fit.theta, fit.measures=T, standardized=T, modindices=T)


parameterEstimates(fit)
parameterEstimates(fit.theta)



# EXERCISE 4.2 Solution: Test for measurement non-equivalence (setting factor loadings and thresholds equal across groups)

fit2<-cfa(model,data=ds,group="group",ordered=c("y1","y2","y3","y4"), group.equal=c("loadings","thresholds"))
summary(fit2,mod=T)
fitMeasures(fit2)

fit2.1<-cfa(model,data=ds,group="group",ordered=c("y1","y2","y3","y4"), group.equal=c("loadings","thresholds"),parameterization = "theta")
summary(fit2.1,mod=T)
modindices(fit2, free.remove=F)

x <-lavTestScore(fit2.1)
x


# Note that modification indices for equality constraints on freely estimated parameters 
# are only available using the function lavTestScore()
# check the parameter number for identifying the troublesome constraint
# (the output uses the original parameter numbering for the unrestricted model. 
#The parameter number would be equal in the restricted model output.)

?lavTestScore()
lavTestScore(fit2,add=newpar)
summary(fit2, fit.measures=T, standardized=T, modindices=T)
anova(fit,fit2)

fitMeasures(fit, selected)
fitMeasures(fit2, selected)
modindices(fit2,free.remove = F)
modificationIndices(fit2)
parameterEstimates(fit2)

#difftest from lavaan does not work well use the following instead to calculate different in fit measures.
#note that lavTestLRT uses the scaled chi2
lavTestLRT(fit2, fit)
anova(fit2, fit)

fitMeasures(fit2, "cfi") - fitMeasures(fit, "cfi")







# EXERCISE 4.3 Solution: If necessary, relax certain equality constraints and re-fit the model
model2 <- 'f =~ y1 + y2 + y3 + y4 
y3 ~*~ c(1,1)*y3'
fit3<-cfa(model2,data=ds,group="group",ordered=c("y1","y2","y3","y4"), group.equal=c("loadings","thresholds"),
          group.partial=c("f=~y3", "y3|t1", "y3|t2", "y3|t3"))
summary(fit3,mod=T)
fitMeasures(fit3, selected)
fitMeasures(fit2, selected)
compareFit(fit2,fit3)

#Alternative
#measurementInvarianceCat is no longeravailable measEq.syntax() can be used instead. It has more functionality.
############################################################################################
### TIP: Alternative specification using measEq.syntax() get the syntax and fit the model.
fit.loadingseq <- measEq.syntax(configural.model = model,data=ds, 
                        ordered=c("y1","y2","y3","y4"),
                        parameterization = "theta",
                        ID.cat = "Wu.Estabrook.2016",
                        group = "group",
                        group.equal = c("loadings"))
## print lavaan syntax to the Console
cat(as.character(fit.loadingseq))
## print a summary of model features
summary(fit.loadingseq)

compareFit()
#https://www.rdocumentation.org/packages/semTools/versions/0.5-1/topics/measEq.syntax
###########################################################################################







# EXERCISE 5 Measurement Invariance with categorical variables. 

# This example is from Hirschfeld, and von Brachel's tutorial Data is from an online survey on sexual compulsivity scale which is available on http://personality-testing.info/_rawdata/. 
# The scale consists of ten items regarding descriptions about sexual behaviour, 
# e.g "I think about sex more than I would like to". 
# The items are measured on a four-category likert scale ranging from "not at all like me" to "very much like me". 
# 
# Subset the data so that you have only codes 1 and 2 for gender variables for the analysis
# 5.1. Fit a one-factor  model of sexual compulsivity in both  groups simultaneously and test configural equivalence
# 5.2. Test for measurement non-equivalence (setting factor loadings and thresholds equal across groups)
# 5.3. If necessary, relax certain equality constraints and re-fit the model.

# EXERCISE 5.1 - Solution
# 5.1. Fit a one-factor  model of sexual compulsivity in both  groups simultaneously and test configural equivalence

##Categorical indicators
download.file("http://personality-testing.info/_rawdata/SCS.zip","SCS.zip")
unzip("SCS.zip")
scs <- read.csv("SCS/data.csv")
addmargins(table(scs$Q1))
addmargins(table(scs$Q2))
addmargins(table(scs$Q3))
addmargins(table(scs$Q4))
table(scs$gender)
hist(scs$Q4)
addmargins(table(scs$gender))
scs <- subset(scs, gender == "1" | gender == "2")
scs_model <- 'scs =~ Q1 + Q2 + Q3 + Q4 + Q5 + Q6 + Q7 + Q8 + Q9 + Q10'
table(scs$Q1)
hist(scs$Q1)
View(scs)
table(is.na(scs$Q3))


# Both groups
scs_model_fit <- cfa(scs_model, ordered = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10"), data=scs, missing="listwise")
summary(scs_model_fit, fit.measures = TRUE)

semPaths(scs_model_fit, "std")




# EXERCISE  5.2 - Solution
# 5.2. Test for measurement non-equivalence (setting factor loadings and thresholds equal across groups)

config <- cfa(scs_model, ordered = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10"), group = "gender", data=scs)
Scaler<- cfa(scs_model, ordered = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10"), group = "gender", group.equal = c("loadings", "thresholds"), data=scs)
lavInspect(config, "zero.cell.tables")

fitMeasures(config, selected)
fitMeasures(metric, selected)
fitMeasures(Scaler, selected)
modindices(Scaler)
summary(Scaler)



# Compare models:
anova(config, metric)
anova(Scaler, metric)
measurementInvariance(scs_model, data=scs, group="gender")

#difftest from lavaan does not work well use the following instead to calculate different in fit measures.
#note that lavTestLRT uses the scaled chi2
lavTestLRT(config,metric,Scaler)
# And look at the CFI difference whether it is >0.01
fitMeasures(config, "cfi") - fitMeasures(metric, "cfi")                                                                                                               #or
fitMeasures(config, c("cfi","cfi.scaled")) - fitMeasures(metric, c("cfi","cfi.scaled")) 



# Solution 5.3. If necessary, relax certain equality constraints and re-fit the model








#####################################
# Missing data
#####################################

# . Default  = listwise deletion
# . Best option:  Direct  ML (Full Information ML)
# . Second best option:  Multiple imputation
# . See Brown, Chapter 9
# . Libraries, Syntax:

# lavaan supports direct ML (FIML)
# cfa(model,data=...,missing="direct")

# MI: use "amelia" function from the Amelia library to generate m imputed datasets
# out<-amelia(input.dataset,m=5)  # m=5: 5 imputed datasets will be stored in "out"
# analyze with "runMI" from the semTools library (which in turn relies on lavaan)
# out.mi<-runMI(model,out$imputations,chi="all",fun="cfa",estimator="ML")

# 
# Missing data - EXAMPLE
# 
# . The file "cfamiss.dat" contains 5 variables (a subject  identifier, s1-s4). The four "s"-variables measure 1 latent trait ("esteem"). For substantive reasons,  s2 and s4 have correlated error variances.
# . Fit the model using FIML
# . Fit the model using MI (generate 5 imputed  datasets)
# 

#########################################
# Missing data  - Direct ML (FIML)
#########################################

ds<-read.table("cfamiss.dat",col.names=c("id","s1","s2","s3","s4"),na=9)
model <- "esteem =~ s1 + s2 + s3 + s4 
s2 ~~ s4"

View(ds)
table(ds$s4)
#how much information is missing? 
table(is.na(ds$s4))
table(is.na(ds$s1))
table(is.na(ds$s2))
190/(460+190) # about 30% missing!


fit0 <- cfa(model,data=ds, missing="listwise")
fit1 <- cfa(model,data=ds,missing="direct", bootstrap = 500)
fit2 <- cfa(model,data=ds,missing="fiml")
summary(fit0,fit.measures=T)
summary(fit1,fit.measures=T)
summary(fit2,fit.measures=T)

# Note the differences in Chisq and SEs
selected <- c("chisq", "df", "rmsea", "cfi")
fitMeasures(fit0, selected)
fitMeasures(fit1, selected)
fitMeasures(fit2, selected)

# Get missing data patterns and covariance coverage similar
# to that found in Mplus output.
inspect(fit1, 'patterns') 
inspect(fit1, 'coverage')


#------------------------------------------------------------------
#  Imputation and model estimation with runmI
#------------------------------------------------------------------

# run lavaan and imputation in one step
#install.packages("mice")

# you need to have this version of lavaan for the code to work 
# install.packages("lavaan", repos="http://www.da.ugent.be", type="source")

# install.packages("devtools") # if necessary, for the next line of syntax to work
# devtools::install_github("simsem/semTools/semTools")

install.packages("lavaan", repos = "http://www.da.ugent.be", type = "source")
install.packages("devtools")
devtools::install_github("simsem/semTools/semTools")


library(lavaan)
library(semTools)
library(mice)
library(Amelia)
#install.packages("Amelia")
#install.packages("mice")




