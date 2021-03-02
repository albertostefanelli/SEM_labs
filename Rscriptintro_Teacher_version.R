


# LEARNING OBJECTIVES
# getting familiar with R
# packages, objects, functions, getting help, handling data, operations


# print the current directory
getwd()
dir() 
ls()


# change to the new directory
setwd()
setwd("C:/Users/ahu.alanya/Desktop/SEM_2020/Week1/SEM_lab_Week1_toledo_2020_v1/R intro")
getwd()
dir()



# EXERCISE 1
# i)   Open a new folder on your PC e.g. "SEM_intro" 
# ii)  Copy paste R intro data sets on toledo into this folder
# ii)  Change your working directory to be that folder
# iii) Save your Rscript
# iv)  Print the files in your current working directory


# Calculate 3 + 4
3 + 4
2**2
2^2




# Assign values to objects/variables
i <- 25
x <- 45
y <- "sem" 
z <- c(1, "sem", 2.5)
var_logical <- TRUE
ex.data <- data.frame(rnorm(10),runif(10), 1:10 )
colnames(ex.data) <- c("R1","R2","R3")

# Print out the value
x
y
z
str(z)
class(ex.data)
head(ex.data)
View(ex.data)

#create a new object/variable
k <- x+i
ex.data$R4 <- ex.data$R1+ex.data$R2
head(ex.data)
R4
ex.data$R4
attach(ex.data)
R4
mean(R4)
plot(R4)
plot(R1,R4)
length(R4) 
hist(R4)
boxplot(R4)


# LEARNINGS 1
# R works with objects that could be anything from a single number to a data frame.
# R uses functions (like commands in Stata) to run operations. 
# You can get help on a function by running ?fun()
# use str(), summarize(), class() to understand what kind of object you are dealing with
# data.frame() function creates a data file
# Need to use dollar sign with the data frame name to refer to variables
# Alternatively you can attach() the data file.OR packages that override this rule. 


# EXERCISE 2
# i)   Create a data frame called "DF" with two variables, 
# age which runs from 15 to 30, 
# and weight which runs from 60 to 65. 
# ii)  Add a new variable to the data frame age*weight
# iii) Plot age and weight

DF <- data.frame(15:30, 50:65)
colnames(DF) <- c("age","weight")
DF$age
attach(DF)

DF$age_weight <- age*weight
View(DF)

plot(age)

# Loading data 
# install packages you need to use
install.packages("foreign") 
install.packages("dplyr")
install.packages("lavaan", dependencies = TRUE)
install.packages("xlsx")
install.packages("Hmisc")
install.packages("haven")   # to read in SAS,SPSS,Stata files
install.packages("readxl")  # to read in xlsx files


# load the necessary libraries
library("foreign") 
library("dplyr")
library("lavaan")
library("xlsx")
library("haven") # stores varible labels as well
library("readxl")
search()

# data sets available in R
data() 
mydata <- data.frame(mtcars)
head(mydata)
View(mtcars)
library(Hmisc)
contents(mtcars)  # no variable labels




# load a foreign dataset
# You can also use package rio or the interface.

install.packages("rio")
library(rio)


read.table()
View()
df <- read.table("lifeexpect.txt", header = TRUE)
df_beer <- import("Beer.sav")

df_beer$missing <- "NA"


library(xlsx)
df <- read.xlsx("cars_dataframe.xlsx", sheetIndex = 1)
library(foreign)
df.spss <- read.spss("Beer.sav", to.data.frame=TRUE)
dat.dta <- read_dta("lifeexpect.dta", encoding = NULL)  #haven
df.spss <- read_sav("Beer.sav", user_na = FALSE) #haven




# EXERCISE 3
# i) load voter.txt
#Calculate mean of variable pop
#Summarize variable frac


# ii) load cars_dataframe.xlsx
# plot distance
# summary(speed)







###  Data manipulation & subsetting & operators  ###
voter <- read.table("voter.txt", header = TRUE)
mean(voter$pop)
table(voter$frac)

voter$frac2[voter$frac< 30] <- "low" 
voter$frac2[voter$frac>=30] <- "high" 
table(voter$frac2)

voter$ct.states[voter$candidat=="Clinton"] <- 1
voter$ct.states[voter$candidat!="Clinton"] <- 0
# create avariable
voter$ct.low[voter$candidat=="Clinton" & voter$frac> 30] <- 1





# EXERCISE 4
# Use Seatbelts data set from R
# Attach the data set in R 
# Create a new variable which is sum of front and rear
# Create a new categorical variable from DriversKilled which shows years 
# with over 150 deaths and below 150 deaths above150 and below150
# What happened to rows with 150 deaths? 



















#solution
df <-  data.frame(Seatbelts)
head(df)
attach(df)
df$frtot <- front + rear
df$cat_drive[DriversKilled>150] <-  "above150"
df$cat_drive[DriversKilled<150] <-  "below150"
table(df$cat_drive, useNA="always")











#Frequency tables
mytable <- table(voter$inc)
mytable # print table
mytable <- table(voter$inc,voter$candidat)
# income will be rows, candidate will be columns 
prop.table(mytable, 1) # row percentages
prop.table(mytable, 2) # column percentages



# EXERCISE 5 
# use ChickWeight data from R
# generate a table showing how many chick following each of the 4 diets
mydata <- data.frame(ChickWeight)













###   Missing values  ####

#Remove missing values in a variable
x <- c(1,2,NA,3)
mean(x) # returns NA
mean(x, na.rm=TRUE) # returns 2
is.na(voter$ct.states)

#Excluding Missing Values from Analyses
#Create new dataset without missing data
voter2 <- na.omit(voter)
is.na(voter2$ct.states)

# Recoding Values to Missing
# recode all values with 1 to missing 
 voter[voter==1] <- NA
 

# Advanced Handling of Missing Data: Multiple Imputation: 
# e.g., Amelia II, Mice, and mitools

 





