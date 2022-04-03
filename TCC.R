#Undergraduate thesis - Data wrangling and statitical aproach

library(readxl)
library(plyr)
library (readr)
library(tidyverse)
library(dplyr)
library(lubridate)
library(tidyr)
library(naniar)
library(Hmisc)
library(fastDummies)
      
##Import data from https://covid.saude.gov.br/, previously dowloaded and treated in excel.
      
data_gov <- read_excel("C:\\Users\\Dell\\Downloads\\HIST_PAINEL_COVIDBR_2020.xlsx")

#examine data

view(data_gov)  

str(data_gov)

#tranform colum to date form

data_gov$date <- as.Date(data_gov$date)

#select only weekdays (b3 doesnt open during weekends)

data_gov %>% mutate(weekdays = wday(date, label = TRUE)) %>%
  filter(!wday(date) %in% c(1, 7))-> data_gov

## examine data

view(data_gov)

str(data_gov)



####import data from companies (previously dowloaded from economatica), change column names and missing values


                                                                      
data_economatica <- read.csv("C:\\Users\\Dell\\Downloads\\Economatica-8900701234-20211108202136.csv",
                             col.names = c("Code", "date", "DR", "MTB","MCAP"),
                             na.strings = "-")

# Code = Company ticker
# DR = Daily stock return
# MTB = Market-to-book ratio
# MCAP = Daily market capitalization


#examine data
str(data_economatica)

head(data_economatica)

glimpse(data_economatica)

view(data_economatica)

#changing code column 

data_economatica$Code <- sub("<XBSP>", "", data_economatica$Code)

# Convert character column to factor

data_economatica$Code <- as.factor(data_economatica$Code)

## create new column with an id for each company

data_economatica  <- transform(data_economatica,                                 
                   ID = as.numeric(factor(Code)))

#convert character column to date

data_economatica$date <- as.Date(data_economatica$date)

# Natural logarithm of daily market capitalization 

data_economatica$LMCAP = log(data_economatica$MCAP)

# puting DR in percentual

data_economatica$DR <- data_economatica$DR/100

#Import data from companies (previously dowloaded from economatica)



data_economatica2 <- read.csv("C:\\Users\\Dell\\Downloads\\Economatica-8900701234-20211105140346 (1).csv",
                              col.names = c("Ativo", "Name", "Class", "Code", "Subsector_B3"),
                              na.strings = "-")
#examine data
str(data_economatica2)

head(data_economatica2)


#Convert character column to factor

data_economatica2$Subsector_B3 <- as.factor(data_economatica2$Subsector_B3)


####Merging company data

data_company <- merge(data_economatica, data_economatica2, by="Code")

#examine data
str(data_company)

view(data_company)

#Select data to show only ID, date, return of stock, log daily market Capitalization, Market-to-Book ratio and subsector,
#and filter date to show only weekdays.

data_company %>%  select(ID, date, DR, LMCAP, MTB, Subsector_B3) %>% 
  mutate(weekdays = wday(date, label = TRUE)) %>%
  filter(!wday(date) %in% c(1, 7))-> data_company

str(data_company)

##Merging company and Covid data

Data <- merge(data_company, data_gov, by ="date")

## Examine data set

str(Data)

view(Data)

# Drop uninportant columns

Data <- Data[-c(7:9,11,12,14)]

# Rearrange rows  

Data %>% arrange(ID) -> Data 

#Reorder columns

Data <- Data[, c(2,1,6,3,4,5,7,8)]

view(Data)


## drop missing values (select only companies with available data for the period)


Data %>% drop_na(LMCAP, MTB) -> Data


## drop missing values from holidays
#26/02, 10/04, 21/04, 01/05, 11/06, 07/09, 12/10 -> 7 observations, include the first one.

Data %>% na.omit(DR) -> Data
view(Data)

#Create Dummy sector Variables 

#Data <- dummy_cols(data, select_columns = "Subsector_B3")



### Examine final data set

str(Data)
head(Data)
view(Data1)

####Correlation Matrix
Data1 <- Data[, c(4:8)]
Data1 <- Data1[, c(1,4,5,2,3)]
Data.cor <- round(cor(Data1),3)
Data.cor
# Hide upper triangle
upper<-Data.cor
upper[upper.tri(Data.cor)]<-""
upper<-as.data.frame(upper)
upper
#export corrrelation matrixx
library(xtable)
print(xtable(upper), type="html")


##Descriptive statistics
summary(Data1)
library(psych)
describe(Data1)  #mean, median, SD, max, min, Skeweness, Kurtosis



#write.csv(Data, "C:\\Users\\Dell\\Downloads\\TCC.csv")


#Unobserverd Heterogeineity 

library(gplots)

plotmeans(DR ~ ID, main = "Heterogeineity across companies", data = Data)  #There is no heterogeneity across companies

plotmeans(DR ~ date, main = "Heterogeineity across time", data = Data)  # There is heterogeneity across time
# we will use other test to verify heterogeneity

################### Data is ready for estimation #############

library(ggplot2)



# Analize first wave of coronavirus in Brazil (why the choice of the time series)

ggplot(data_gov, aes(date, Newcases)) + geom_line() + labs(x= "Data" , y ="Casos Diarios", 
                                                            title = "Casos diários de Covid-19")

ggplot(data_gov, aes(date, Newdeaths)) + geom_line() + labs(x= "Data" , y ="Mortes Diarias", 
        title = "Mortes diárias por Covid-19") 

#+ theme(axis.text.x = element_text(angle = 90, hjust = 1))


############## Panel Data Analysis

library(plm)


########### Panel data analysis for Daily Growth in total Covid Cases

## set data as panel data

pdata <- pdata.frame(Data, index = c("ID", "date"))
view(pdata)

# Pooled Ordinary Least Square estimator

pooling <- plm(DR ~ lag(LMCAP, k=-1) + lag(MTB, k=-1) + lag(DGTCC,k=-1), data=pdata, model= "pooling")
summary(pooling)

# Fixed effects or within estimator

fixed <- plm(DR ~ lag(LMCAP, k=-1) + lag(MTB, k=-1) + lag(DGTCC,k=-1), data=pdata, model= "within")
summary(fixed)

summary(fixef(fixed)) # intercepts

# Random effects estimator
random <- plm(DR ~ lag(LMCAP, k=-1) + lag(MTB, k=-1) + lag(DGTCC,k=-1), data=pdata, model= "random")
summary(random)


# LM test for random effects versus OLS

plmtest(pooling, type = "bp") ## p-value < 0.05, random effects model is a better choice
                              ## there is evidence of significant differences across countries(heteroske), therefore 
                              ##I can't run a simple OLS regression.

# LM test for fixed effects versus OLS
pFtest(fixed, pooling)  ## p-value < 0.05, the fixed effects model is a better choice

# Hausman test for fixed versus random effects model
phtest(fixed, random )   #p-value < 0.05, the fixed effects model is a better choice



#test for cross section dependence
pcdtest(fixed, test = c("lm"))
pcdtest(fixed, test = c("cd"))  ### in both tests, p-value < 0.05, there is cross-sectional dependence

#Test for serial correlation
pbgtest(fixed)   # p-value < 0.05, there is serial correlation = autocorrelation
##Lijung Box or Portementeau for example)

#Testing for unit roots/stationarity
library(tseries)

adf.test(pdata$DR, k=2)  ## p-value < 0.05, no unit roots present.(cant take the first difference)

# Testing for heteroskedasticity
library(lmtest)
bptest(DR ~ lag(LMCAP, k=-1) + lag(MTB, k=-1) + lag(DGTCC,k=-1) , data = pdata, studentize=F) ##p-value < 0.05, presence of heteroskedasticity


#To deal with autocorrelation, hetroskedasticity and cross sectional dependence in my data i will
#use Panel Corrected Standard Error (PCSE) regression, since in my data N>T.

#counting number of data available for each company

Data %>% count(ID, sort = TRUE) ### 210 companies with 163 observations 

# Dealing with unbalanced Data  = drop companies with less than 163 ID observations.
# Extract largest balanced subset of the data.

Data %>% group_by(ID) %>%
  filter(n() >= 2) %>% 
  ungroup() -> Data

view(Data)
str(Data)
library("pcse")

lm <- lm(DR ~ lag(LMCAP,1) + lag(MTB,1) + lag(DGTCC,1), data=Data)
summary(lm)
                   
lmpcse <- pcse(lm, Data$ID, Data$date, pairwise = TRUE)
summary(lmpcse)



gc()
memory.limit(9999999999)
lmpcse <- pcse(lm, Data$ID, Data$date)
gc() 



########## Panel data analysis for Daily Growth in total covid death Cases

attach(Data)
Y <- cbind(DR)
X <- cbind(LMCAP, MTB, DGTDCC)


## set data as panel data

pdata <- pdata.frame(Data, index = c("ID", "date"))
view(pdata)


# Descriptive statistics



# Pooled Ordinary Least Square estimator

pooling <- plm(DR ~ lag(LMCAP, k=-1) + lag(MTB, k=-1) + lag(DGTCC,k=-1), data=pdata, model= "pooling")
summary(pooling)

# Fixed effects or within estimator

fixed <- plm(DR ~ lag(LMCAP, k=-1) + lag(MTB, k=-1) + lag(DGTCC,k=-1), data=pdata, model= "within")
summary(fixed)

summary(fixef(fixed)) # intercepts

# Random effects estimator
random <- plm(DR ~ lag(LMCAP, k=-1) + lag(MTB, k=-1) + lag(DGTCC,k=-1), data=pdata, model= "random")
summary(random)


# LM test for random effects versus OLS

plmtest(pooling, type = "bp") ## p-value < 0.05, random effects model is a better choice
## there is evidence of significant differences across countries(heteroske), therefore 
##I can't run a simple OLS regression.

# LM test for fixed effects versus OLS
pFtest(fixed, pooling)  ## p-value < 0.05, the fixed effects model is a better choice

# Hausman test for fixed versus random effects model
phtest(fixed, random )   #p-value < 0.05, the fixed effects model is a better choice



#test for cross section dependence
pcdtest(fixed, test = c("lm"))
pcdtest(fixed, test = c("cd"))  ### in both tests, p-value < 0.05, there is cross-sectional dependence

#Test for serial correlation
pbgtest(fixed)   # p-value < 0.05, there is serial correlation = autocorrelation


#Testing for unit roots/stationarity
library(tseries)

adf.test(pdata$DR, k=2)  ## p-value < 0.05, no unit roots present.(cant take the first difference)

# Testing for heteroskedasticity
library(lmtest)
bptest(DR ~ lag(LMCAP, k=-1) + lag(MTB, k=-1) + lag(DGTCC,k=-1) , data = pdata, studentize=F) ##p-value < 0.05, presence of heteroskedasticity



#
rm(list = ls())





