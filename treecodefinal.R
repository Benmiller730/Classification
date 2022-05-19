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

write.csv(data5,"Task1_Data.csv", quote = FALSE, row.names = FALSE)

## Tree
data5$salaryF <- as.factor(data5$salary) 
str(data5)

set.seed(2599)
pd <- sample(2, nrow(data5),replace=TRUE, prob=c(0.20,0.80))

train <- data5[pd==1,] 
validate <- data5[pd==2,] 

tree <- ctree (salaryF ~ age + educationNo + capitalgain + capitalloss + hrsperweek, data = train)

tree
plot(tree, type="simple")

tab <- table(predict(tree), train$salaryF)
sum(diag(tab))/sum(tab) 

