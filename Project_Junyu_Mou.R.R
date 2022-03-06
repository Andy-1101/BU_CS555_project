#################preprocess the data############################################
#import data 
data <- read.csv("D:/File/BU/3rd-FALL 2021/CS 555/TermProject/games_V2.csv")
#check data type
data <- na.omit(data)
is.integer(data$turns)
is.integer(data$white_rating)
is.integer(data$black_rating)
is.integer(data$opening_ply)
#explore data: numeric 
#check the distribution
par(mfrow = c(1,4))
hist(data$white_rating,main="distribution of white_rating",col = 'red')
hist(data$black_rating,main="distribution of black_rating",col = 'green')
hist(data$opening_ply,main="distribution of opening_ply",col = 'blue')
hist(data$turns,main="distribution of turns",col='yellow')
par(mfrow = c(1,1))
#distribution of white_rating and black_rating is similar to 
#normal distribution, while opening_ply and turns are a little 
#bit right-skewed

#check boxplot
par(mfrow = c(1,4))
boxplot(data$white_rating,main="boxplot:white_rating",col='red')
boxplot(data$black_rating,main="boxplot:black_rating",col='green')
boxplot(data$opening_ply,main="boxplot:opening_ply",col='blue')
boxplot(data$turns,main="boxplot:turns",col='yellow')
par(mfrow = c(1,1)) 
#all features have outliers especially extremely large 
#outliers

#remove rows with outliers
Q <- quantile(data$white_rating)
IQR <- Q[4] - Q[2]
up <-  Q[4] + 1.5 * IQR # Upper Range  
low<- Q[2] - 1.5 * IQR # Lower Range
data <- subset(data, data$white_rating>low & data$white_rating<up)

Q <- quantile(data$black_rating)
IQR <- Q[4] - Q[2]
up <-  Q[4] + 1.5 * IQR # Upper Range  
low<- Q[2] - 1.5 * IQR # Lower Range
data <- subset(data, data$black_rating>low & data$black_rating<up)

Q <- quantile(data$opening_ply)
IQR <- Q[4] - Q[2]
up <-  Q[4] + 1.5 * IQR # Upper Range  
low<- Q[2] - 1.5 * IQR # Lower Range
data <- subset(data, data$opening_ply>low & data$opening_ply<up)

Q <- quantile(data$turns)
IQR <- Q[4] - Q[2]
up <-  Q[4] + 1.5 * IQR # Upper Range  
low<- Q[2] - 1.5 * IQR # Lower Range
data <- subset(data, data$turns>low & data$turns<up)
#ckeck the distribution and boxplot again by code above


#explore data: non-numric 
par(mfrow = c(1,2))
barplot(table(data$victory_status),main="distibution of victory_status",col='purple')
barplot(table(data$winner),main="distibution of winner",col='navy')
par(mfrow = c(1,1))
#the data is kind of unbalance, there are few samples for draw and out of time 
#for victory_status, faw samples for draw for winner 

#remove draw and out of time
data <- subset(data,winner != 'draw')
data <- subset(data,victory_status != 'outoftime')

#number of rows 
nrow(data)

#remove unrelated columns 
data <- data[,-1]
data$white <- ifelse(data$winner=='white',1,0)
################################################################################

#MLR
m <- glm(data$white~data$turns+data$white_rating+data$black_rating+data$opening_ply,
         family=binomial)
summary(m)
alpha = 0.05
z_c <- 1.96 
#overall test 
library(aod)
wald.test(b=coef(m),Sigma=vcov(m),Term = 2:5)
# p = 0 < 0.05, there is at least one beta != 0 

#test for individual parameters
#1. turns 
OR_turns <- exp(-0.0028988)#0.9714281
conf_left <- exp(-0.0028988 - z_c*0.0006025)
conf_right <- exp(-0.0028988 + z_c*0.0006025)
c(conf_left,conf_right)#0.9959286 , 0.9982836

#2. white_rating  
OR_wr <- exp(0.0040574)#1.004066
conf_left <- exp(0.0040574 - z_c*0.0001003)
conf_right <- exp(0.0040574 + z_c*0.0001003)
c(conf_left,conf_right)#1.003868 , 1.004263

#3. black_rating  
OR_br <- exp(-0.0041568)#0.9958518
conf_left <- exp(-0.0041568 - z_c*0.0001000)
conf_right <- exp(-0.0041568 + z_c*0.0001000)
c(conf_left,conf_right)#0.9956567 , 0.9960470

#4. opening_ply 
OR_op <- exp(0.0244088)#1.024709
conf_left <- exp(0.0244088 - z_c*0.0079369)
conf_right <- exp(0.0244088 + z_c*0.0079369)
c(conf_left,conf_right)#1.008892 , 1.040774


#AUC_ROC
library(pROC)
data$prob <- predict(m,type=c('response'))
g <- roc(data$white~data$prob)
g#0.7261 :looks fine
plot(1-g$specificities,g$sensitivities,type = 'l',main='ROC')
abline(a=0,b=1)



