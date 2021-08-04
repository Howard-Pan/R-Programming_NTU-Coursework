# R-Programming_NTU-Coursework
This is the coursework and assignments from the course "Data Analysis" in NTU by using R programming. 

## Assignemnt 2
### Q1
#### (a.)
library(foreign);
setwd("~/Desktop/Data/Assignment 2");
mydata= read.dta("WAGE1.DTA")
#### (b.)
tail(mydata,8)
#### (c.)
mean(mydata$wage)
[1] 5.896103; 
max(mydata$wage)
[1] 24.98; 
min(mydata$wage)
[1] 0.53; 
median(mydata$wage)
[1] 4.65
#### (d.)
boxplot(mydata$wage, main="A Simple Box Plot", ylab="Hourly Wage", col=blues9)
#### (e.)
mydata$female[mydata$female == 0] <- "Male";
mydata$female[mydata$female == 1] <- "Female";
counts<-table(mydata$female) ;
counts; 
barplot(counts, main = "Data Analytics Barplot", xlab = "Sex", ylab="Frequency", ylim=c(0,350),col="blue")
#### (f.)
plot(mydata$educ,mydata$wage, main="wage vs. education", xlab = "years of education", ylab = "hourly wage", col="green")

### Q2
#### (a.)
hours=mydata[,2]; 
table(hours); 
(753-325)/753
[1] 0.5683931
#### (b.)
#### We can check the obeservations and varialbes in the "Environment"
table(mydata[,6]); 
(381+44+51+14+57+46)
[1] 593; 
593/753
[1] 0.7875166; 
b=mydata[mydata[,6]>=12 & mydata[,2]>0,]; 
#### We can see the observations from the upper right corner "environment", and the number is 356.
356/593
[1] 0.6003373; 
#### (c.)
mydata[mydata[,6]>=16 & mydata[,11]<=16,]; 
c=mydata[mydata[,6]>=16 & mydata[,11]<=16,]; 
#### We can see the observations from the upper right corner "environment", and the number is 57.
57/753
[1] 0.07569721
#### (d.)
753-57 (Total-"Women>=16, Husband<=16")
[1] 696; 
d<-c(57, 696); 
barplot(d, main = "Comparison", ylab = "Numbers",ylim =c(0,700), names.arg = c("Women>=16, Husband<=16","Others"), col = "darkred")
#### (e.)
lm(formula= inlf~ nwifeinc+educ+kidslt6+exper+I(exper^2), data = mydata); 
fit.marry<-lm(formula= inlf~ nwifeinc+educ+kidslt6+exper+I(exper^2), data = mydata); 
summary(fit.marry); 
inlf=(-0.1288083)+(-0.0052223)*nwifeinc+0.0447794*educ+(- 0.1696127)*kidslt6+0.0421868*exper+(-0.0008764)*I(exper^2); 
(-0.1288083)+(-0.0052223)*20+0.0447794*12+(-0.1696127)*3+0.0421868*6+(- 0.0008764)*36
[1] 0.0168308

### Q3
