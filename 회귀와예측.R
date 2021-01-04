#회귀와 예측

#단순선형회귀
## 회귀식
model <- lm(PEFR ~ Exposure, data=lung)
model

##적합값과 잔차
fitted <- predict(model)
resid <- residuals(model)

# 다중선형회귀
## ex)킹 카운티 주택 정보
head(house[, c("AdjSalePrice", "SqFtTotLiving", "SqFtLot", "Bathrooms", 
               "Bedrooms", "BldgGrade")])

house_lm <- lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms + 
                 Bedrooms + BldgGrade,  
               data=house, na.action=na.omit)

## 모형 평가
summary(house_lm)

## 모형 선택 및 단계적 회귀
house_full <- lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms + 
                   Bedrooms + BldgGrade + PropertyType + NbrLivingUnits + 
                   SqFtFinBasement + YrBuilt + YrRenovated + NewConstruction,
                 data=house, na.action=na.omit)

step_lm <- stepAIC(house_full, direction="both")
step_lm

lm(AdjSalePrice ~  Bedrooms, data=house)

## 가중회귀
house$Year = year(house$DocumentDate)
house$Weight = house$Year - 2005

house_wt <- lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms + 
                 Bedrooms + BldgGrade,
               data=house, weight=Weight, na.action=na.omit)
round(cbind(house_lm=house_lm$coefficients, 
            house_wt=house_wt$coefficients), digits=3)


# 회귀에서의 요인변수
## 가변수 표현
head(house[, 'PropertyType'])

prop_type_dummies <- model.matrix(~PropertyType -1, data=house)
head(prop_type_dummies)

lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms + 
     Bedrooms +  BldgGrade + PropertyType, data=house)

## 다수의 수준을 갖는 요인변수들
table(house$ZipCode)

zip_groups <- house %>%
  mutate(resid = residuals(house_lm)) %>%
  group_by(ZipCode) %>%
  summarize(med_resid = median(resid),
            cnt = n()) %>%
  arrange(med_resid) %>%
  mutate(cum_cnt = cumsum(cnt),
         ZipGroup = factor(ntile(cum_cnt, 5)))
house <- house %>%
  left_join(select(zip_groups, ZipCode, ZipGroup), by='ZipCode')

## 예측변수 간 상관
step_lm$coefficients

update(step_lm, . ~ . -SqFtTotLiving - SqFtFinBasement - Bathrooms)

## 교란변수
lm(AdjSalePrice ~  SqFtTotLiving + SqFtLot + 
     Bathrooms + Bedrooms + 
     BldgGrade + PropertyType + ZipGroup,
   data=house, na.action=na.omit)

## 상호작용과 주효과
lm(AdjSalePrice ~  SqFtTotLiving*ZipGroup + SqFtLot + 
     Bathrooms + Bedrooms + 
     BldgGrade + PropertyType,
   data=house, na.action=na.omit)


# 가정 검정: 회귀 진단
## 특잇값
house_98105 <- house[house$ZipCode == 98105,]
lm_98105 <- lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms + 
                 Bedrooms + BldgGrade, data=house_98105)

sresid <- rstandard(lm_98105)
idx <- order(sresid, decreasing=FALSE)
sresid[idx[1]]
resid(lm_98105)[idx[1]]

house_98105[idx[1], c('AdjSalePrice', 'SqFtTotLiving', 'SqFtLot',
                      'Bathrooms', 'Bedrooms', 'BldgGrade')]

## 영향값
std_resid <- rstandard(lm_98105)
cooks_D <- cooks.distance(lm_98105)
hat_values <- hatvalues(lm_98105)
plot(hat_values, std_resid, cex=10*sqrt(cooks_D))
abline(h=c(-2.5, 2.5), lty=2)

## 이분산성, 비정규성,오차 간 상관
df <- data.frame(
  resid = residuals(lm_98105),
  pred = predict(lm_98105))
ggplot(df, aes(pred, abs(resid))) +
  geom_point() +
  geom_smooth() 

## 편잔차그림과 비선형성
terms <- predict(lm_98105, type='terms')
partial_resid <- resid(lm_98105) + terms

df <- data.frame(SqFtTotLiving = house_98105[, 'SqFtTotLiving'],
                 Terms = terms[, 'SqFtTotLiving'],
                 PartialResid = partial_resid[, 'SqFtTotLiving'])
ggplot(df, aes(SqFtTotLiving, PartialResid)) +
  geom_point(shape=1) + scale_shape(solid = FALSE) +
  geom_smooth(linetype=2) + 
  geom_line(aes(SqFtTotLiving, Terms))  

#다항회귀와 스플라인 회귀
## 다항식
lm(AdjSalePrice ~ poly(SqFtTotLiving, 2) + SqFtLot +
     BldgGrade + Bathrooms +  Bedrooms, 
   data=house_98105)

##스플라인
knots <- quantile(house_98105$SqFtTotLiving, p=c(.25, .5, .75))
lm_spline <- lm(AdjSalePrice ~ bs(SqFtTotLiving, knots=knots, degree=3) +  SqFtLot +  
                  Bathrooms + Bedrooms + BldgGrade,  data=house_98105)

## 일반화가법모형
lm_gam <- gam(AdjSalePrice ~ s(SqFtTotLiving) + SqFtLot + 
                Bathrooms +  Bedrooms + BldgGrade, 
              data=house_98105)
terms <- predict.gam(lm_gam, type='terms')
partial_resid <- resid(lm_gam) + terms
