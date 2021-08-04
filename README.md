# R-Programming_NTU-Coursework
This is the coursework and assignments from the course "Data Analysis" in NTU by using R programming. 

## Assignemnt 2


### Q1
#### (a.)
> library(foreign)  
setwd("~/Desktop/Data/Assignment 2")  
mydata= read.dta("WAGE1.DTA")
#### (b.)
> tail(mydata,8)
#### (c.)
> mean(mydata$wage)  
[1] 5.896103  
> max(mydata$wage)  
[1] 24.98  
> min(mydata$wage)  
[1] 0.53  
> median(mydata$wage)  
[1] 4.65
#### (d.)
> boxplot(mydata$wage, main="A Simple Box Plot", ylab="Hourly Wage", col=blues9)
#### (e.)
> mydata$female[mydata$female == 0] <- "Male"  
mydata$female[mydata$female == 1] <- "Female"  
counts<-table(mydata$female)  
counts  
barplot(counts, main = "Data Analytics Barplot", xlab = "Sex", ylab="Frequency", ylim=c(0,350),col="blue")  
#### (f.)
> plot(mydata$educ,mydata$wage, main="wage vs. education", xlab = "years of education", ylab = "hourly wage", col="green")  


### Q2
#### (a.)
> hours=mydata[,2]  
table(hours)  
(753-325)/753  
[1] 0.5683931  
#### (b.)
##### We can check the obeservations and varialbes in the "Environment"
> table(mydata[,6])  
(381+44+51+14+57+46)  
[1] 593  
593/753  
[1] 0.7875166  
b=mydata[mydata[,6]>=12 & mydata[,2]>0,]  
##### We can see the observations from the upper right corner "environment", and the number is 356.
> 356/593  
[1] 0.6003373  
#### (c.)
> mydata[mydata[,6]>=16 & mydata[,11]<=16,]  
c=mydata[mydata[,6]>=16 & mydata[,11]<=16,]  
##### We can see the observations from the upper right corner "environment", and the number is 57.
> 57/753  
[1] 0.07569721  
#### (d.)
> 753-57 (Total-"Women>=16, Husband<=16")  
[1] 696  
d<-c(57, 696)  
barplot(d, main = "Comparison", ylab = "Numbers",ylim =c(0,700), names.arg = c("Women>=16, Husband<=16","Others"), col = "darkred")  
#### (e.)
> lm(formula= inlf~ nwifeinc+educ+kidslt6+exper+I(exper^2), data = mydata)  
fit.marry<-lm(formula= inlf~ nwifeinc+educ+kidslt6+exper+I(exper^2), data = mydata)  
summary(fit.marry)  
inlf=(-0.1288083)+(-0.0052223)*nwifeinc+0.0447794*educ+(- 0.1696127)*kidslt6+0.0421868*exper+(-0.0008764)*I(exper^2); 
(-0.1288083)+(-0.0052223)*20+0.0447794*12+(-0.1696127)*3+0.0421868*6+(- 0.0008764)*36  
[1] 0.0168308  


### Q3
#### (a.)
> Wage_Points_Assists_Avgmin=NBA_Salary[,c(2,11,13,15)]  
pairs(Wage_Points_Assists_Avgmin)  
##### We could see that if we randomly pick two of the variables, the relationship between two of them would mostly be positive.
#### (b.)
> Wage_Points=lm(formula = wage ~ points, data = NBA_Salary)  
lm(Wage_Points)  
summary(Wage_Points)  
##### The value ofvmultiple R square is 0.4317, and for adjusted R square is 0.4296, which means that for variable "points", it could explain nearly 43% of the dependent variable "wage". And normally for R square around 50%, we could consider as good.
#### (c.)
> confint(Wage_Points, level=0.9)  
##### 90% confidence interval is between 98.72424 and 124.6092, meaning there is 90% of chances that our true value will lie within this range. Another interpretation is that if we have 100 samples, 90 samples' confidence interval will include the true value, and the other 10 samples will not.
#### (d.)
> plot(NBA_Salary$points,NBA_Salary$wage, xlab = "Points",ylab = "Wage" ,col="red")  
abline(Wage_Points, col="green" , lwd=5)  
#### (e.)
> E=lm(wage~ points+avgmin+forward+center+exper+black, data=NBA_Salary)  
#### (f.)
> Include_guard=lm(wage~ points+avgmin+forward+center+exper+black+guard, data=NBA_Salary)  
##### We can add variable "guard" in the regression, however, the coefficient will turn out to be "NA". And the reason is that "guard" is linearly dependent on "forward" and "center", the linear equation ought to be (1= forward+center+guard), you could only be one of them and we already include "forward" and "center" in our regression. Another example would be dummy variable "gender", if female=1 and male=0, when we run the regression on gender, we would only put either female or male but not both, the reason is that you could only be either one of them, thus female and male are linearly dependent.
#### (g.)
> summary(E)  
##### From the above summary of E,we could discover that varaible "black" is not statistically significant, so it is unlikly to say that there is racial salary discrimination.
#### (h.)
> step(E, direction = "both")  
##### From the AIC result of (wage ~ points + avgmin + forward + center + exper + black), we could know that it is better to remove variable "black" as it is shown on the first row. And from the AIC result of (wage ~ points + avgmin + forward + center + exper), we know that it is better not to remove any variable. And this result is consistent with the summary of E from question (e.). They both show that "black" is not an important variable.
#### (i.)
> H=lm(wage ~ points + avgmin + forward + center + exper, data = NBA_Salary)  
summary(H)  
##### In (e.), its R-square is 0.5616, adjusted R-square is 0.5515, as for (h.)its R-square is 0.5606, adjusted R-square is 0.5522. R-square decreases as we remove one of the variables, but it does not capture the quality of how we improve the model. For the quality of model improvement, we should put our focus on adjusted R-square, since adjusted R-square increases, we could know that removing "black" will make our model better, and this result is consistent with AIC result.
#### (j.)
> first_observation=data.frame(points=16, avgmin=37.23, forward=0, center=0, exper=4)  
predict(H, first_observation, interval = "prediction",level = 0.95)  

### Q4
#### (a.)
> library(foreign)  
setwd("~/Desktop/Data/Assignment 2")  
mydata= read.csv("admission.csv")  
pairs(mydata)  
#### (b.)
> boxplot(gre ~ rank, data = mydata, col="Blue")  
#### (c.)
> rank_1<-mydata[mydata[,4]==1, ]  
##### 61 are from ranked 1 institution, we can know that by looking at the upper right hand side.
rank_1_rejected<- mydata[mydata[,4]==1 & mydata[,1]==0, ]  
##### 28 are from ranked 1 institution and rejected, we can know that by looking at the upper right hand side.
> 28/61  
[1] 0.4590164  
##### Nearly 46% of them are rejected.
#### (d.)
> rank_4<-mydata[mydata[,4]==4, ]  
##### 67 are from ranked 4 institution, we can know that by looking at the upper right hand side.
> rank_4_rejected<-mydata[mydata[,4]==4 & mydata[,1]==0, ]  
##### 55 are from ranked 4 institution, we can know that by looking at the upper right hand side.
> 55/67  
[1] 0.8208955  
##### Nearly 82% of them are rejected.
#### (e.)
> Reg_admit_gpa<- lm(admit~gpa, data = mydata)   
Reg_admit_gpa  
##### The coefficient is 0.2183, which means if there is an 1 unit increase in GPA, on average,the chances of getting admitted will increase by 21.83%.
#### (f.)
> plot(mydata$gpa,mydata$admit, xlab = "GPA",ylab = "Admit" ,col="red")  
abline(Reg_admit_gpa, col="green" , lwd=5)  
##### We can see that for "admit"=1, people with lower GPA are unlikely to get admitted. However, we can still see that people with higher GPA will somehow also be rejected. From the regression line, we can see there is a positive relation between "admit" and "GPA".
#### (g.)
> Reg_admit_gpa_gre<-lm(admit~ gpa+ gre, data=mydata)   
Reg_admit_gpa_gre  
newdata= data.frame(gpa=3.6, gre=700)   
predict(Reg_admit_gpa_gre, newdata, interval="prediction", level=0.95)  
##### The probability of being accepted is around 41.15%.

### Q5
#### (a.)
> library(foreign)  
library(rpart)  
setwd("~/Desktop/Data/Assignment 2")  
Admission= read.csv("admission.csv")  
Admission$admit[Admission$admit==0]<- "Not_Admit"  
Admission$admit[Admission$admit==1]<- "Admit"  
rtree<-rpart(admit~., data= Admission, minsplit=20, cp=0.05 )   
rpart.plot(rtree)  
##### From this decision tree, we could see the first node is GPA, 48% of the samples are above 3.4, 52% are below 3.4, and the chances of not getting admitted is 78%. For those who had GPA above 3.4, and rank <2, admission rate is 75%. For those who had GPA above 3.4, and rank >2, the chances of not getting admitted would be 64%.Since we set (cp=0.05 and minsplit =20), GRE seems to be not that important compare to other variables, so there is no node for GRE. But if we set cp=0.01, then we would see the node of GRE.
#### (b.)
> newdata= data.frame(gpa=3.6, gre=580, rank=2)  
predict(rtree, newdata)  
predict(rtree, newdata, type = "class")  
[1] Not_Admit  
##### Based on the above prediction, it is likely that he or she will not be admitted.

### Q6
#### (a.)
##### Both Gini index and entropy measure impurity of the nodes. And their goal is to group same observations together and looking for the most purity. The point that we are looking for is where it has the smallest Gini index or the largest entropy. Gini Index has values inside the interval [0, 0.5] whereas the interval of the Entropy is [0, 1]. 
#### (b.)
> install.packages("party")  
library(party)  
data("readingSkills")  
summary(readingSkills)  
#### (c.)
> pairs(readingSkills)  
##### From the scatter plot, we could see the relationship between either two variables, but we can't tell which is independent variable and which is dependent variable. So we can’t draw causal inference.
#### (d.)
> library(rpart)  
library(rpart.plot)  
#### (e.)
> rtree<-rpart(nativeSpeaker~ shoeSize+age+score, data= readingSkills, minsplit=20, cp=0.05 )  
rpart.plot(rtree)  
##### The first node is score, then we could see that it uses age>=8 and age>=10 to be the nodes and again, it used score to divide the group. And we can see there is no such a node for shoe size, it seems shoesize isn't significant compared to the other variables.


### Q7
> library(foreign)  
setwd("~/Downloads")  
read.csv("marry.csv")   
marry$marry<-as.factor(marry$marry)  
marry$gender<-as.factor(marry$gender)  
marry$place<-as.factor(marry$place)  
marry$education<-as.factor(marry$education)   
str(marry)  
==========================================  
> library(rpart)  
library(rpart.plot)  
rtree_gini<-rpart(marry~place+gender+education, data= marry, minsplit=0, cp=0.01, parms = list(split = "gini"))  
rpart.plot(rtree_gini)  
###### rtree_gini uses gini index to do decision tree, and its first node is education.
> rtree_information<-rpart(marry~place+gender+education, data= marry, minsplit=0, cp=0.01, parms = list(split = "information"))  
> rpart.plot(rtree_information)  
##### rtree_information uses information or entropy to do decision tree, and its first node is also education.
