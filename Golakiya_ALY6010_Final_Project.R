print("ALY6010 ISha's Final Project")

# Installing and Downloading packages
install.packages("tidyverse")
install.packages("ggalt")  #for dumbbell plots
install.packages("countrycode")                       
install.packages("rworldmap")
install.packages("gridExtra")
install.packages("broom")
install.packages("ggplot2")
install.packages("reader")
install.packages("magrittr")
install.packages("plyr")
install.packages("moments")
library(corrplot)
library(formattable)
library(plyr)
library(moments)
library(tidyverse)
library(ggalt)
library(countrycode)
library(rworldmap)
library(naniar)
library(gridExtra)
library(broom)
library(ggplot2)
library(reader)
library(magrittr)
setwd("/Users/HP/Downloads")
diabete <- read.csv("diabetes.csv")

class(diabete)
str(diabete)  #getting an idea of data set
summary(diabete)
head(diabete,5)
cat("Number of missing value:", sum(is.na(diabete)), "\n")
vis_miss(diabete)

diabete$BloodPressure <- as.numeric(diabete$BloodPressure)
diabete$BloodPressure
class(diabete$BloodPressure)
class(diabete$BMI)
diabete$BMI <- as.numeric(diabete$BMI)
diabete$BMI
class(diabete$Age)
diabete$Age <- as.numeric(diabete$Age)
class(diabete$Age)
diabete$Age
diabete$Pregnancies <- as.numeric(diabete$Pregnancies)
class(diabete$Pregnancies)
diabete$Glucose <- as.numeric(diabete$Glucose)

class(diabete$Glucose)
#removing unrealistic values
diab <- diabete[(diabete$BloodPressure > 0) & (diabete$BMI > 0),]
diab
summary(diab)
str(diab)

mean(diab$Age)
sd(diab$Age)

view(diabete)
# Confidence Interval 
CI(diabete$Age, ci=0.95)
describe(diab, na.rm = TRUE, skew = TRUE, ranges = TRUE, trim = 1, type = 3, check = TRUE, fast = NULL, quant = FALSE, IQR = FALSE, omit = FALSE)


# Histogram
p1 <- ggplot(diab, aes(x=Pregnancies)) + ggtitle("Number of times pregnant") +
  geom_histogram(aes(y = 100*(..count..)/sum(..count..)), binwidth = 1, colour="black", fill="white") + ylab("Percentage")
p2 <- ggplot(diab, aes(x=Glucose)) + ggtitle("Glucose") +
  geom_histogram(aes(y = 100*(..count..)/sum(..count..)), binwidth = 5, colour="black", fill="white") + ylab("Percentage")
p3 <- ggplot(diab, aes(x=BloodPressure)) + ggtitle("Blood Pressure") +
  geom_histogram(aes(y = 100*(..count..)/sum(..count..)), binwidth = 2, colour="black", fill="white") + ylab("Percentage")
p4 <- ggplot(diab, aes(x=SkinThickness)) + ggtitle("Skin Thickness") +
  geom_histogram(aes(y = 100*(..count..)/sum(..count..)), binwidth = 2, colour="black", fill="white") + ylab("Percentage")
p5 <- ggplot(diab, aes(x=Insulin)) + ggtitle("Insulin") +
  geom_histogram(aes(y = 100*(..count..)/sum(..count..)), binwidth = 20, colour="black", fill="white") + ylab("Percentage")
p6 <- ggplot(diab, aes(x=BMI)) + ggtitle("Body Mass Index") +
  geom_histogram(aes(y = 100*(..count..)/sum(..count..)), binwidth = 1, colour="black", fill="white") + ylab("Percentage")
p7 <- ggplot(diab, aes(x=DiabetesPedigreeFunction)) + ggtitle("Diabetes Pedigree Function") +
  geom_histogram(aes(y = 100*(..count..)/sum(..count..)), colour="black", fill="white") + ylab("Percentage")
p8 <- ggplot(diab, aes(x=Age)) + ggtitle("Age") +
  geom_histogram(aes(y = 100*(..count..)/sum(..count..)), binwidth=1, colour="black", fill="white") + ylab("Percentage")
grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, ncol=2)
p8

#summary
summary(diab)

#Relation of all the numeric values (correlation matrix)
cor_data <- cor(diab[,setdiff(names(diab), 'Outcome')])
cor_data


corrplot(cor_data)


#Relation between Independent and dependent variable
attach(diab)

par(mfrow=c(2,4))
boxplot(Pregnancies ~ Outcome, main="No. of Pregnancies vs. Diabetes", 
        xlab="Outcome", ylab="Pregnancies")
boxplot(Glucose~Outcome, main="Glucose vs. Diabetes", 
        xlab="Outcome", ylab="Glucose")
boxplot(BloodPressure~Outcome, main="Blood Pressure vs. Diabetes", 
        xlab="Outcome", ylab="Blood Pressure")
boxplot(SkinThickness~Outcome, main="Skin Thickness vs. Diabetes", 
        xlab="Outcome", ylab="Skin Thickness")
boxplot(Insulin~Outcome, main="Insulin vs. Diabetes", 
        xlab="Outcome", ylab="Insulin")
boxplot(BMI~Outcome, main="BMI vs. Diabetes", 
        xlab="Outcome", ylab="BMI")
boxplot(DiabetesPedigreeFunction~Outcome, main="Diabetes Pedigree Function vs. Diabetes", xlab="Outcome", ylab="DiabetesPedigreeFunction")
boxplot(Age~Outcome, main="Age vs. Diabetes", 
        xlab="Outcome", ylab="Age")

shapiro.test(young$Glucose)

youngch <- sample_n(young, 20)
elderm <- sample_n(elder,20)


formattable(diab,  list(
  Glucose = color_tile("white", "Orange"),
  BloodPressure = formatter("span", style = x ~ ifelse(x <= "130", style(color = "green", font.weight = "bold"),NA)),
  Outcome = formatter("span", 
                      style = x ~ style(color = ifelse(x, "green", "red")),
                      x ~ icontext(ifelse(x, "ok", "remove"), ifelse(x, "1", "0"))),
  Age = formatter("span", style = x ~ ifelse(x >= "45", style(color = "blue", font.weight = "bold"),NA)),
  Pregnancies = formatter("span", 
                          style = x ~ style(color = ifelse(x > "1", "red", "grey")))
))
#converssion 
diab$Outcome
out <- ifelse(diab$Outcome == '1', 1, 0)
out

HWN <- data.frame(Preg = diab$Pregnancies,Age = diab$Age, out = out)
HWN
plot(HWN$Age,HWN$Preg,main= "Age vs Preg",pch = 3,xlab = "Age", ylab = "Preg", col = "Red")
abline(lm(Preg~Age, data = HWN),col="black")


#regression
HWN <- data.frame(blood = diab$BloodPressure,BMI = diab$BMI, out = out)
HWN
plot(HWN$BMI,HWN$blood,main= "Age vs Preg",pch = 3,xlab = "Age", ylab = "Preg", col = "Red")
abline(lm(blood~BMI, data = HWN),col="black")

sum<-lm(BMI~blood+out, data=HWN)
sum
summary(sum)
t.test(HWN$blood, HWN$BMI, var.equal = F, alternative = "two.sided")

Yes<-subset(HWN,out=="1",select=c("blood","BMI","out"))
Yes <- head(Yes,500)
No <- subset(HWN,out=="0",select=c("blood","BMI","out"))
No <- head(No,500)
# Common regression line
regone <- plot(Yes$blood,Yes$BMI,col="blue", main = "One Regression line",pch = 3, xlab = "Blood Pressure", ylab = "BMI")
points(No$blood,No$BMI,col="red",pch = 2)
abline(lm(BMI~blood, data = HWN),col="black")
legend("topleft",
       pch = c(3, 2),
       c("Patient of Diabete", "Not a Patient of Diabete"),
       col = c("blue", "red"))

regtwo <- plot(HWM$blood,HWM$BMI,col="blue",main="Two regression line",pch = 3)
points(female$Bweight,female$Hweight,col="red",pch = 2)
abline(a=-0.419,b=4.752,col="black", lw = 2)
abline(a=(-0.41-0.82),b=4.752,col="grey",lw = 2)
legend("topleft",
       pch = c(3, 2),
       c("Male", "Female"),
       col = c("BLue", "Red"))
legend("bottomright",
       lw = c(2, 2),
       c("Male regression", "Femaleregression"),
       col = c("black", "grey"))







