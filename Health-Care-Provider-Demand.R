setwd()
library(plyr)
data1 <- read.csv("1.csv",header = T,sep = ",")
data2 <- read.csv("2.csv",header = T,sep = ",")
data2 <- data2[,c(-1,-2)]
data3 <- read.csv("3.csv",header = T,sep = ",")
data4 <- read.csv("4.csv",header = T,sep = ",")
data5 <- read.csv("5.csv",header = T,sep = ",")
newdata <- join(data1,data2,by=c("鄉鎮市區代碼","鄉鎮市區名稱"),type="inner")
newdata <- join(newdata,data3,by=c("鄉鎮市區代碼","鄉鎮市區名稱"),type="inner")
newdata <- join(newdata,data4,by=c("鄉鎮市區代碼","鄉鎮市區名稱"),type="inner")
newdata <- join(newdata,data5,by=c("鄉鎮市區代碼","鄉鎮市區名稱"),type="inner")
data_r <- newdata[,c(-1,-2,-3,-4)] ##去掉縣市代碼那些
str(data_r)
summary(data_r)

rm.data <- data_r[complete.cases(data_r), ] #去除有遺失值的資料 ＃剩339筆
str(rm.data)
summary(rm.data)
apply(rm.data,2,sd)

#install.packages("mice")
#library(mice)
#md.pattern(data_r)


#讓圖顯示中文字
library(showtext)
showtext_auto(enable = TRUE)

#正式開始處理資料
install.packages("car")
installed.packages("carData")
install.packages("effests")
library(alr4)


## step1-1:探索性分析＿單變數hist################################################
summary(rm.data)
hist(rm.data$照護人力需求指數,main = "照護人力需求指數長條圖",xlab = "照護人力需求指數")
hist(log(rm.data$照護人力需求指數),main = "照護人力需求指數長條圖",xlab = "log(照護人力需求指數)")
qqnorm(log(rm.data$照護人力需求指數))

par(mfrow=c(2,2))
hist(rm.data$X65歲以上人口數,main = "65歲以上人口數長條圖",xlab = "65歲以上人口數")
hist(rm.data$人口密度,main = "人口密度長條圖",xlab = "人口密度")
hist(rm.data$扶老比,main = "扶老比長條圖",xlab = "扶老比")
hist(rm.data$老化指數,main = "老化指數長條圖",xlab = "老化指數")
hist(rm.data$環境便利需求指數,main = "環境便利需求指數長條圖",xlab = "環境便利需求指數")
hist(rm.data$住宅狀況需求指數,main = "住宅狀況需求指數長條圖",xlab = "住宅狀況需求指數")
hist(rm.data$低收入戶,main = "低收入戶長條圖",xlab = "低收入戶")
hist(rm.data$社區照顧關懷據點家數,main = "社區照顧關懷據點家數長條圖",xlab = "社區照顧關懷據點家數")
hist(rm.data$醫療院所平均每家服務人數,main = "醫療院所平均每家服務人數長條圖",xlab = "醫療院所平均每家服務人數")
hist(rm.data$綜合所得稅所得總額申報中位數,main = "綜合所得稅所得總額申報中位數長條圖",xlab = "綜合所得稅所得總額申報中位數")

## step1-2:散佈圖 #################################################
#每個變項散佈圖
pairs(照護人力需求指數~.,data = data_r)
#每個x和y的散佈圖
plot(照護人力需求指數~X65歲以上人口數,data=rm.data,
             main = "照護人力需求指數與65歲以上人口數散佈圖",
             col="dark blue",cex=0.5,pch=20)
abline(lm(照護人力需求指數~X65歲以上人口數,data=rm.data), lty=1) #abline看清楚資料分布趨勢
with(rm.data, lines(lowess(照護人力需求指數~X65歲以上人口數, f=6/10, iter=1), lty=2,col=2)) #lowess配適smooth線


plot(照護人力需求指數~人口密度,data=rm.data,
             main = "照護人力需求指數與人口密度散佈圖",
             col="dark blue",cex=0.5,pch=20)
abline(lm(照護人力需求指數~人口密度,data=rm.data), lty=1) #abline看清楚資料分布趨勢
with(rm.data, lines(lowess(照護人力需求指數~人口密度, f=6/10, iter=1), lty=2,col=2)) #lowess配適smooth線

plot(照護人力需求指數~扶老比,data=rm.data,
             main = "照護人力需求指數與扶老比散佈圖",
             col="dark blue",cex=0.5,pch=20)
abline(lm(照護人力需求指數~扶老比,data=rm.data), lty=1) #abline看清楚資料分布趨勢
with(rm.data, lines(lowess(照護人力需求指數~扶老比, f=6/10, iter=1), lty=2,col=2)) #lowess配適smooth線

plot(照護人力需求指數~老化指數,data=rm.data,
             main = "照護人力需求指數與老化指數散佈圖",
             col="dark blue",cex=0.5,pch=20)
abline(lm(照護人力需求指數~老化指數,data=rm.data), lty=1) #abline看清楚資料分布趨勢
with(rm.data, lines(lowess(照護人力需求指數~老化指數, f=6/10, iter=1), lty=2,col=2)) #lowess配適smooth線

plot(照護人力需求指數~環境便利需求指數,data=rm.data,
             main = "照護人力需求指數與環境便利需求指數散佈圖",
             col="dark blue",cex=0.5,pch=20)
abline(lm(照護人力需求指數~環境便利需求指數,data=rm.data), lty=1) #abline看清楚資料分布趨勢
with(rm.data, lines(lowess(照護人力需求指數~環境便利需求指數, f=6/10, iter=1), lty=2,col=2)) #lowess配適smooth線


plot(照護人力需求指數~住宅狀況需求指數,data=rm.data,
             main = "照護人力需求指數與住宅狀況需求指數散佈圖",
             col="dark blue",cex=0.5,pch=20)
abline(lm(照護人力需求指數~住宅狀況需求指數,data=rm.data), lty=1) #abline看清楚資料分布趨勢
with(rm.data, lines(lowess(照護人力需求指數~住宅狀況需求指數, f=6/10, iter=1), lty=2,col=2)) #lowess配適smooth線


plot(照護人力需求指數~低收入戶,data=rm.data,
             main = "照護人力需求指數與低收入戶散佈圖",
             col="dark blue",cex=0.5,pch=20)
abline(lm(照護人力需求指數~低收入戶,data=rm.data), lty=1) #abline看清楚資料分布趨勢
with(rm.data, lines(lowess(照護人力需求指數~低收入戶, f=6/10, iter=1), lty=2,col=2))

plot(照護人力需求指數~社區照顧關懷據點家數,data=rm.data,
             main = "照護人力需求指數與社區照顧關懷據點家數散佈圖",
             col="dark blue",cex=0.5,pch=20)
abline(lm(照護人力需求指數~社區照顧關懷據點家數,data=rm.data), lty=1) #abline看清楚資料分布趨勢
with(rm.data, lines(lowess(照護人力需求指數~社區照顧關懷據點家數, f=6/10, iter=1), lty=2,col=2))

plot(照護人力需求指數~醫療院所平均每家服務人數,data=rm.data,
             main = "照護人力需求指數與醫療院所平均每家服務人數散佈圖",
             col="dark blue",cex=0.5,pch=20)
abline(lm(照護人力需求指數~醫療院所平均每家服務人數,data=rm.data), lty=1) #abline看清楚資料分布趨勢
with(rm.data, lines(lowess(照護人力需求指數~醫療院所平均每家服務人數, f=6/10, iter=1), lty=2,col=2))

plot(照護人力需求指數~綜合所得稅所得總額申報中位數,data=rm.data,
             main = "照護人力需求指數與綜合所得稅所得總額申報中位數散佈圖",
             col="dark blue",cex=0.5,pch=20)
abline(lm(照護人力需求指數~綜合所得稅所得總額申報中位數,data=rm.data), lty=1) #abline看清楚資料分布趨勢
with(rm.data, lines(lowess(照護人力需求指數~綜合所得稅所得總額申報中位數, f=6/10, iter=1), lty=2,col=2))


## step2-1:配適迴歸線 #################################################
mfull <- lm(照護人力需求指數 ~ ., data=rm.data)
summary(mfull)
#variance covariance matrix
as.data.frame(cor(rm.data))
as.data.frame(round(cor(rm.data),4))   ##共變異數矩陣
#residual Plots
plot(mfull)
residualPlots(mfull)

## step2-2:added-variable plots #################################################
m1 <- lm(照護人力需求指數 ~ 人口密度, data=rm.data)
r1 <- residuals(m1)
m2 <- lm(X65歲以上人口數 ~ 人口密度, data=rm.data)
r2 <- residuals(m2)
m4 <- lm(resid(m1) ~ resid(m2))
summary(m4)
plot(resid(m2),m4$fitted.values)

## effect plot
m5<- lm(formula = 照護人力需求指數 ~ X65歲以上人口數 + 人口密度 + 扶老比 + 住宅狀況需求指數 +低收入戶+綜合所得稅所得總額申報中位數,
            data = rm.data)
plot(Effect("人口密度", m5), grid=TRUE, rug=TRUE)
plot(Effect("X65歲以上人口數", m4), grid=TRUE, rug=TRUE)
plot(Effect("扶老比", m4), grid=TRUE, rug=TRUE)
plot(Effect("住宅狀況需求指數", m4), grid=TRUE, rug=TRUE)
plot(Effect("低收入戶", m4), grid=TRUE, rug=TRUE)
plot(Effect("綜合所得稅所得總額申報中位數", m4), grid=TRUE, rug=TRUE)

## step2-3:簡單線性迴歸#################################################
#y和每個x單獨做模型
m_community_care <- lm(照護人力需求指數 ~ 社區照顧關懷據點家數, data=rm.data)
summary(m_community_care)

m_income_tax <- lm(照護人力需求指數 ~ 綜合所得稅所得總額申報中位數, data=rm.data)
summary(m_income_tax)

m_65 <- lm(照護人力需求指數 ~ X65歲以上人口數, data=rm.data)
summary(m_65)

m_pop_density <- lm(照護人力需求指數 ~ 人口密度, data=rm.data)
summary(m_pop_density)

m_old_ratio <- lm(照護人力需求指數 ~ 扶老比, data=rm.data) #不顯著
summary(m_old_ratio)

m_residential <- lm(照護人力需求指數 ~ 住宅狀況需求指數, data=rm.data)
summary(m_residential)

m_low_income <- lm(照護人力需求指數 ~ 低收入戶, data=rm.data)
summary(m_low_income)

m_environment <- lm(照護人力需求指數 ~ 環境便利需求指數, data=rm.data)
summary(m_environment)

m_hospital <- lm(照護人力需求指數 ~ 醫療院所平均每家服務人數, data=rm.data)
summary(m_hospital)

m_aging_index <- lm(照護人力需求指數 ~ 老化指數, data=rm.data)
summary(m_aging_index)

## step2-4:變數選取#################################################
f <- ~ X65歲以上人口數 + 人口密度 + 扶老比 +老化指數+環境便利需求指數+ 
  住宅狀況需求指數 +低收入戶+綜合所得稅所得總額申報中位數+
  社區照顧關懷據點家數+醫療院所平均每家服務人數
#向前選取
m0 <- lm(照護人力需求指數 ~ 1, rm.data) # the base model
m.forward <- step(m0, scope=f, direction="forward")
#向後選取
m1 <- update(m0, f) #full model
m.backward <- step(m1, direction="backward")
#stepwise
m.stepwise <- step(m1, direction="both")

## step3:殘差分析#################################################
########修正的模型###################
#model1
model1log <- lm(log(照護人力需求指數) ~ X65歲以上人口數 + 人口密度 + 
                  扶老比 + 住宅狀況需求指數 + 
                  低收入戶 + 綜合所得稅所得總額申報中位數, data=rm.data)
summary(model1log)
install.packages("car")
library(car)
vif(model1log) ###共線性診斷
plot(model1log)
residualPlots(model1log) #partial residual plot
ks.test(scale(model1log$residuals),pnorm) #檢定殘差是否常態
qqnorm(model2$residuals) #常態機率圖
#model2
model2 <- lm(log(照護人力需求指數) ~ log(X65歲以上人口數) + 人口密度 + 
                  log(扶老比) + 住宅狀況需求指數 + 
                  低收入戶 + 綜合所得稅所得總額申報中位數, data=rm.data)
summary(model2)
vif(model2)
residualPlots(model2)#Tukey test 檢定是檢定殘差及所有x殘差是否是null plot ，不拒絕（p-value>0.05）才是對的
ks.test(scale(model2$residuals),pnorm) #檢定殘差是否常態
qqnorm(model2$residuals) #常態機率圖
plot(model2)
ncvTest(model2) #檢定變異數常態

