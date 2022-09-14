##Mike Yang
##HW01
##AGEC 936

##HOE04

install.packages("tidybayes")
install.packages("tidyverse")
library(tidybayes)
library(tidyverse)

urlToRead <- "http://www2.census.gov/programs-surveys/popest/tables/2010-2011/state/totals/nst-est2011-01.csv"
testFrame <- read.csv(url(urlToRead))

cs <- testFrame
str(cs)

head(cs)

cs<-cs[-1:-8,]
head(cs)
summary(cs)

cs<-cs[,1:5]
head(cs)
tail(cs)

nrow(cs)
cs <-cs[-54:-58,]
tail(cs)

cs <- cs[-(nrow(cs)-1),]
tail(cs)

colnames(cs)
colnames(cs) <- c("State","censusapril2010", "popbaseapril2010", "popjuly2010", "popjuly2011")
head(cs)

summary(cs)
head(cs)

example <- "The Cat in the Hat"
substr( example, start = 1, stop = 3)

substr( example, start = 16, stop = 18)

example1 <- "I Love the Cat in the Hat"
substr( example1, start = 16, stop = 18)

nchar(example)
nchar(example1)

substr( example, start = (nchar(example) - 2), stop = nchar(example))
substr( example1, start = (nchar(example1) - 2), stop = nchar(example1))

cs <- cs
cs$State <- substr(cs$State, 2, nchar(cs$State) )
head(cs)

cs$censusapril2010 <- gsub( pattern = ",", replacement = "", x = cs$censusapril2010)
cs$censusapril2010 <- as.numeric(cs$censusapril2010)
head(cs)

summary(cs)

cs$popbaseapril2010 <- as.numeric(gsub(",", "", cs$popbaseapril2010))
cs$popjuly2010 <- as.numeric(gsub(",", "", cs$popjuly2010))
cs$popjuly2011 <- as.numeric(gsub(",", "", cs$popjuly2011))

head(cs)
tail(cs)

rownames(cs) <- NULL
head(cs)
tail(cs)

cs$State <- ifelse(test = cs$State == "uerto Rico", yes = "Puerto Rico", no = cs$State)
tail(cs)

sortedStates <-cs[order(cs$popjuly2011),]
head(sortedStates)

sortedStates <- cs[order(-cs$popjuly2011), ]
head(sortedStates)

##Assignment 01. Cleaning/Munging Data frames and Functions and Distributions
##AGEC 936
## Mike Yang

##Part I:

#Q1
mhktemp0 <- read.csv('tas_1901_2016_MHK.csv')
#read data set

head(mhktemp0)
summary(mhktemp0)

mhktemp<-mhktemp0[,1:3]
#dropped 4th, 5th columns

colnames(mhktemp) <- c("AVTinC","Year", "Month")
#named first 3 columns "AVTinC","Year", "Month"

mhktemp$Month <- str_trim(mhktemp$Month)

AVTinF <- c((mhktemp$AVTinC * 1.8 ) +32)
#created AVTinF vector

mhktemp$AVTinF <- AVTinF
#added AVtinF vector to data set

mhktemp = mhktemp[,c(3,2,1,4)]
#reordered columns

summary(mhktemp)

#Q2

avgtemp <- mhktemp%>%
group_by(Month) %>%
summarise(avg_C = mean(AVTinC))

#group it by Month and find mean for each month

coldest_month <- avgtemp$Month[which.min(avgtemp$avg_C)]
coldest_month

## Answer: January

##Q3

SD_Month <- mhktemp %>%
  group_by(Month) %>%
  summarise(SD_F = sd(AVTinF))

#group by Month and find each month's Standard Deviation 

SD_Month

Highest_SD_Month <-SD_Month$Month[which.max(SD_Month$SD_F)]
Highest_SD_Month

##Answer: January

##Q4

hist(mhktemp[mhktemp$Month=="Feb Average", "AVTinF"], 
     main = "Feb 1901-2016",
     xlab = "Average Temperature F")

hist(mhktemp[mhktemp$Month=="Aug Average", "AVTinF"], 
     main = "Aug Temperature 1901-2016",
     xlab = "Average Temperature F")

##Answer: Both dist are close to normal dist and Aug has longer tail

##Q5

##Answer: Since histogram is closed to normal dist,
##there is about 95% chance avg fed weather will be within (25,40)


##part 2
##Q1
readcensusdata <- function(file.url)
  {
  data <- read.csv(file.url)
  return(data)
  }

statedata <- readcensusdata(file.url = 'https://www2.census.gov/programs-surveys/popest/tables/2010-2011/state/totals/nst-est2011-01.csv')

##Q2
cleancensus <- function(file)
{
  cs <- file
  cs<-cs[-1:-8,]
  cs<-cs[,1:5]
  cs <-cs[-54:-58,]
  cs <- cs[-(nrow(cs)-1),]
  colnames(cs) <- c("State","censusapril2010", "popbaseapril2010", "popjuly2010", "popjuly2011")
  cs$State <- substr(cs$State, 2, nchar(cs$State) )
  cs$censusapril2010 <- gsub( pattern = ",", replacement = "", x = cs$censusapril2010)
  cs$censusapril2010 <- as.numeric(cs$censusapril2010)
  cs$popbaseapril2010 <- as.numeric(gsub(",", "", cs$popbaseapril2010))
  cs$popjuly2010 <- as.numeric(gsub(",", "", cs$popjuly2010))
  cs$popjuly2011 <- as.numeric(gsub(",", "", cs$popjuly2011))
  rownames(cs) <- NULL
  cs$State <- ifelse(test = cs$State == "uerto Rico", yes = "Puerto Rico", no = cs$State)
  return( cs )
}  

  statepop <- cleancensus( file = statedata )
  summary(statepop)

##Q3
  hist(statepop$popjuly2011/1000000, main = "Population in July 2011",
       xlab = "Population per state(M)")
  
  states <- statepop[statepop$popjuly2011<=5000000, "State"]
  
  states
  
##Q4
  hist(statepop$popjuly2011, plot = FALSE)$counts[[2]]
  
  

##Q5
  #In US most of states has small number of population compare to a few large population states such as CA
  #And about 90% of states has less the 10M population and a few has more than 10M
  #Distribution does not follow normal dist, and not symmetric.
  #it might be symmetric if we drop a few big states from data set
  
  
  ##Q6
  
  cdf <- function(vector , number)
  {
    prob <- length(vector[vector<=number])/length(vector)
    return(prob)
  }

  ##7
  
  cdf(vector = c(1,2,3,4,5), number = 2)
  
  
  ##8
  
  cdf(vector = statepop$popjuly2011, number = mean(statepop$popjuly2011) )

  ##9
  
  #50% since it is normal dist
  
  ##10
  ndist <- rnorm(10000, mean = mean(statepop$popjuly2011), sd = sd(statepop$popjuly2011))
  cdf(ndist,mean(statepop$popjuly2011))
  
  ##11
  norsample52 <- rnorm(52,mean(statepop$popjuly2011),sd(statepop$popjuly2011))
  cdf(norsample52, mean(statepop$popjuly2011) )  
  
  ##yes, there are discrepancies between the percentage.
  ##Because US population is not normally distributed 
