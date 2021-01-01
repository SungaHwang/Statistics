#3-1
A <- sort(rep(c(1,2),12));A
B <- rep(c(1,1,1,1,2,2,2,2,3,3,3,3),2)
score <- c(79,78,79,76,78,79,75,76,82,80,82,84,71,73,74,74,77,74,75,74,74,71,71,72);B
df <- data.frame(A,B,score);df
str(df)
df$A <- factor(df$A);df$A
df$B <- factor(df$B);df$B
str(df)

twoanovamodel <- aov(score~ A+B+A*B, data=df)
summary(twoanovamodel)

interaction.plot(df$B, df$A, df$score)

#3-2
factor1 <- sort(rep(c(1,2,3),12))
factor2 <- rep(c(15,15,15,15,30,30,30,30,45,45,45,45),3)
value <- c(130,155,74,180,30,40,80,75,20,70,82,58,150,188,159,126,136,122,106,115,25,70,58,45,138,110,168,160,174,120,150,139,96,104,82,60)
df2 <- data.frame(factor1,factor2,value);df2
df2$factor1 <- factor(df2$factor1)
df2$factor2 <- factor(df2$factor2)
str(df2)

install.packages("EMSaov")
library(EMSaov)
EMSanova(value~factor1+factor2, data=df2, type=c("R","R"))  


interaction.plot(df2$factor1,df2$factor2,df2$value)
#3-3
city <- sort(rep(c(1,2,3),9))
design <- rep(c(1,1,1,2,2,2,3,3,3),3)
sale <- c(23,20,21,22,19,20,19,18,21,22,20,19,24,25,22,20,19,22,18,18,16,21,23,20,20,22,24)
df3 <- data.frame(city,design,sale);df3
str(df3)
df3$city <- factor(df3$city)
df3$design <- factor(df3$design)
str(df3)
twoanovamodel <- aov(sale~ city+design+city*design, data=df3)
summary(twoanovamodel)

interaction.plot(df3$design, df3$city, df3$sale)

#3-3-3
install.packages("EMSaov")
library(EMSaov)
EMSanova(sale~city+design, data=df3, type=c("R","R"))  

interaction.plot(df3$city,df3$design,df3$sale)
