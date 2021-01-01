install.packages("ggplot2")
library(ggplot2)
ggplot(data=mpg, aes(x=displ, y=hwy))+
  geom_point()+
  xlim(3,6)+
  ylim(10,30)

#1
ggplot(data=mpg, aes(x=cty,y=hwy))+geom_point()
#2
ggplot(data=midwest,aes(x=poptotal,y=popasian))+
  geom_point()+
  xlim(0,500000)+
  ylim(0,10000)

#평균 막대 그래프
library(dplyr)
df_mpg<-mpg %>% 
  group_by(drv) %>% 
  summarise(mean_hwy=mean(hwy))
df_mpg

ggplot(data=df_mpg,aes(x=reorder(drv,-mean_hwy),y=mean_hwy))+geom_col()

#빈도 막대 그래프
ggplot(data=mpg, aes(x=drv))+geom_bar()

#히스토그램이랑 비슷한 모양
ggplot(data=mpg, aes(x=hwy))+geom_bar()

#1
library(dplyr)
mpg
df<-mpg %>% 
  filter(class == "suv") %>% 
  group_by(manufacturer) %>%
  summarise(mean_cty = mean(cty)) %>% 
  arrange(desc(mean_cty)) %>% 
  head(5)
df

library(ggplot2)
ggplot(data=df,aes(x=reorder(manufacturer,-mean_cty), y=mean_cty))+geom_col()

#2
ggplot(data=mpg, aes(x=class))+geom_bar()

#시계열
library(ggplot2)
library(dplyr)
ggplot(data=economics,aes(x=date,y=unemploy))+geom_line()

#1
head(economics)
ggplot(data=economics,aes(x=date,y=psavert))+geom_line()

#상자그림
ggplot(data=mpg,aes(x=drv,y=hwy))+geom_boxplot()

#1
class_mpg<-mpg %>% 
  filter(class %in% c("compact","subcompact","suv"))

ggplot(data=class_mpg, aes(x=class, y=cty))+geom_boxplot()
