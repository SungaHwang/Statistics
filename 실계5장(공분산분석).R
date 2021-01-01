#1
way<- sort(rep(c(1,2,3,4),5));way
y <- c(46.5,45.9,49.8,46.1,44.3,48.7,49,50.1,48.5,45.2,46.3,47.1,48.9,48.2,50.3,44.7,43,51,48.1,48.6);y
x <- c(13,14,12,12,14,12,10,11,12,14,15,14,11,11,10,16,15,10,12,11);x

df <- data.frame(way,y,x);df
str(df)
df$way <- factor(df$way);df$way
str(df)

ancova <- lm(y~x+way, data=df)
summary(ancova)

#2
teacher <- sort(rep(c(1,2,3),6));teacher
iq <- c(119,125,117,123,118,125,121,127,117,119,118,130,130,122,117,121,120,119);iq
score <- c(85,88,84,88,87,87,86,88,85,86,85,91,95,93,90,94,92,92);score
df2 <- data.frame(teacher,iq,score);df2
str(df2)
df2$teacher <- factor(df2$teacher);df2$teacher
str(df2)

ancova2 <- lm(score~iq+teacher, data=df2)
summary(ancova2)

#4
machine <- sort(rep(c(1,2,3),5));machine
thickness <- c(20,25,24,25,32,22,28,22,30,28,21,23,26,21,15)
strength <- c(36,41,39,42,32,40,48,39,45,44,35,37,42,34,32)
df3 <- data.frame(machine,thickness,strength);df3
str(df3)
df3$machine <- factor(df3$machine);df3$machine
str(df3)

ancova3 <- lm(strength~thickness+machine, data=df3)
summary(ancova3)

aov3 <- aov(strength~machine, data=df3)
summary(aov3)

summary.lm(aov3)

#5
group <- sort(rep(c(1,2),10))
age <- c(31,28,25,34,39,26,30,26,31,23,36,33,31,29,41,36,32,33,27,32)
score <-　c(30,0,10,40,55,20,65,5,40,0,65,50,90,25,99,60,25,80,5,99)
df4 <- data.frame(group,age,score);df4
str(df4)
df4$group <- factor(df4$group)
str(df4)

ancova4 <- lm(score~age+group, data=df4)
summary(ancova4)


aov4 <- aov(score~group, data=df4)
summary(aov4)
