library(party)
library(dplyr)
library(ggplot2)
setwd("/Users/ben/Documents/Data Science/Assignment/ASDM/TASK1")
data <- read.delim("adult.data", sep=",", header=FALSE)
names(data) <- c("age", "workclass","fnlwgt", "education", "educationNo", "martialstatus",
                 "occupation", "relationship", "race", "sex", "capitalgain", "capitalloss",
                 "hrsperweek", "nativecountry","salary")

data2 <- data[,c(1:3,5:7,9:15)]
data2[data2 == " ?"] <- "Na"
Missing <- subset(data2, data2 == "Na")
data3 <- data2[!grepl("Na",data2$workclass),]
Missing <- subset(data3, data3 == "Na")
data4 <- data3[!grepl("Na",data3$occupation),]
Missing <- subset(data4, data4 == "Na")
data5 <- data4[!grepl("Na",data4$nativecountry),]
Missing <- subset(data5, data5 == "Na")

dim(data5)
names(data5)
head(data5)
tail(data5)
summary(data5)
str(data5) 

## AGE
par(mfrow=c(1,1))
summary(data5$age)
hist(data5[,1], xlab = "Age", main = "Histogram of Ages",ylim=c(0,5000))
axis(side=1, at=seq(0,100, 10))
axis(side=2, at=seq(0,5000, 1000))

data6 <- subset(data5, data5$salary == " <=50K")
data7 <- subset(data5, data5$salary == " >50K")

summary(data6$age)
summary(data7$age)
hist(data6[,1], xlab = "Age", main = "Histogram of Ages who earn <=$50k",ylim=c(0,5000))
axis(side=1, at=seq(0,100, 10))
axis(side=2, at=seq(0,5000, 1000))
hist(data7[,1], xlab = "Age", main = "Histogram of Ages who earn >$50k",ylim=c(0,5000))
axis(side=1, at=seq(0,100, 10))
axis(side=2, at=seq(0,4500, 1000))

chisq.test(data5$age, data5$salary)

## SALARY
par(mfrow=c(1,1))
data5[data5 == " <=50K"] <- "1"
data5[data5 == " >50K"]  <- "2"
data5$salary <- as.integer(data5$salary)
salaryBP <- table(data5$salary)
names(salaryBP) <- c("<=50k", ">50k")
barplot(salaryBP, main = "Barplot of Salaray of the Individuals", ylim=c(0,25000), 
        col=grey.colors(2))

##Capital Gain
par(mfrow=c(1,1))
data8 <- data5[c(9,13)]
data8 <- data8[!grepl("0",data8$capitalgain),]
CGless <- subset(data8, data8$salary == "1")
CGmore <- subset(data8, data8$salary == "2")
par(mfrow=c(1,2), main = "capital gain and loss boxplots w/ zero values")
boxplot(CGless[,1], main = "<=50k", ylim=c(0,10000))
boxplot(CGmore[,1], main = ">50k", ylim=c(0,10000))

## Capital loss
par(mfrow=c(1,1))
data10 <- data5[c(10,13)]
data10 <- data10[!grepl("0",data10$capitalloss),]
CLless <- subset(data10, data10$salary == "1")
CLmore <- subset(data10, data10$salary == "2")
par(mfrow=c(1,2), main = "capital gain and loss boxplots w/ zero values")
boxplot(CLless[,1], main = "<=50k", ylim=c(0,5000))
boxplot(CLmore[,1], main = ">50k", ylim=c(0,5000))

## Hours per week 
par(mfrow=c(1,1))
hist(data5$hrsperweek)
summary(data5$hrsperweek)

## Sex
data11 <- data5[c(8,13)]
Male <- data11[!grepl("2", data11$sex),]
Female <- data11[!grepl("1", data11$sex),]

MaleD <- table(Male[c(2)])
MaleD <- prop.table(MaleD)*100
FemaleD <- table(Female[c(2)])
FemaleD <- prop.table(FemaleD)*100

par(mfrow=c(1,2))
barplot(MaleD, ylim = c(0,100), main = "Male Income Barplot", 
        col=grey.colors(2))
barplot(FemaleD, ylim = c(0,100), main = "Female income Barplot", 
        col=grey.colors(2))

chisq.test(data5$sex, data5$salary)

## Education
par(mfrow=c(1,1))
data12 <- data5[c(4)]
data12_5 <- data5[c(4)]
data12 <- table(data12)
data12 <- prop.table(data12)*100
data12_5 <- order(table(data12_5), decreasing = TRUE)

data13<- c(32.6238313, 22.1404416, 16.7230290, 5.3942046, 4.3332670,
           3.4745707, 3.3419535, 2.7186526, 1.8466945, 1.7969631, 1.5085207,
           1.2499171, 1.2432863, 0.9548438, 0.5006299, 0.1491944)
data14 <- matrix(data= NA, nrow=16, ncol=2)
barplot(data13, ylim = c(0,50), col = grey.colors(3), main = "Barplot of Education")
axis(1, )
xlab(label)


