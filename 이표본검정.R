sleep
head(sleep)
str(sleep)

#탐색적 분석
table(sleep$group)
with(sleep,tapply(extra, group, sd))
boxplot(extra~group,
        data=sleep)

#등분산성 가정
var.test(extra~group, data=sleep)

#독립 검정
t.test(extra~group, data=sleep, alternative=c("two.sided"),
       var.equal=T, conf.level=0.95)

#두개의 짝을 이룰 때
#less(왼쪽 꼬리), greater(오른쪽 꼬리)
t.test(extra~group, data=sleep, alternative=c("two.sided"),
       var.equal=T, conf.level=0.95, paried=T)
t.test(extra~group, data=sleep, alternative=c("greater"),
       var.equal=T, conf.level=0.95, paried=T)

x1 <- c(51.4,52.0,45.5,54.5,52.3,50.9,52.7,50.3,53.8,53.1)
x2 <- c(50.1,51.5,45.9,53.1,51.8,50.3,52.0,49.9,52.5,53.0)

diff_x <- x1-x2
diff_x
mean_diff_x <- mean(diff_x)
mean_diff_x

sd_diff_x <- sd(diff_x)
sd_diff_x

t_x <- mean_diff_x/(sd_diff_x/sqrt(length(diff_x)))
t_x

t.test(x1,x2,
       alternative = c("greater"),
       paired=T,
       conf.level = 0.95)

#두 모집단의 모비율 차이 검정
prop<- c(0.33,0.41)
n <- c(500,600)
y <- prop*n;y

prop.test(x=y,
          n=n,
          correct=F,
          alternative=c("two.sided"))
