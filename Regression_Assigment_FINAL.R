setwd("C:\\Software\\xlri\\Regression\\Assignment")
library(car)
library(lmtest) 
library(tseries)
library(sandwich)
library(stats)
library(MASS)
library(faraway)

### Reading the csv file

realestateprice<-read.csv("regression_1.csv",header=TRUE, na.strings=c("*",'NA'))
str(realestateprice)
head(realestateprice)

#cor(realestateprice)
cor(realestateprice,use="complete.obs")


# Replace missing value with Mean for Age
realestateprice$Age[is.na(realestateprice$Age)]=round(mean(realestateprice$Age[!is.na(realestateprice$Age)],na.rm=FALSE),0)

##Add dummy variables for Feats
for(level in unique(realestateprice$Feats)-1){

  realestateprice[paste("Feats",   gsub("-","_", gsub(" ","_",level, fixed=TRUE), fixed=TRUE), sep = "_")] <- ifelse(realestateprice$Feats == level, 1, 0)

}
###Remove Feats from dataframe
realestateprice$Feats__1<-NULL
realestateprice$Feats<-NULL

######
# Impute missing value for tax using regression

lmtax<-lm(TAX~
        Price
        + SQ.FT
        +Age
        +NE
        +CUST
        +COR
        +Feats_7
        +Feats_6
        +Feats_5
        +Feats_3
        +Feats_4
        +Feats_2
        +Feats_1
        +Feats_0
        ,data=realestateprice
  )
lmtax

realestateprice$k<-0

for(i in 1:nrow(realestateprice))
{
  if(is.na(realestateprice$TAX[i])==TRUE)
  {
    realestateprice$k[i]=realestateprice$k[i]+1
  }
}  

# 
for (i in 1:nrow(realestateprice))
{
  if(realestateprice$k[i]==1)
  {
            realestateprice$TAX[i]= (-243.7345
              +0.3291*realestateprice$Price[i]
              +0.2851*realestateprice$SQ.FT[i]
              -5.0791*realestateprice$Age[i]
              +34.7114*realestateprice$NE[i]
              -34.1725*realestateprice$CUST[i]
              -6.3471*realestateprice$COR[i]
              +518.2073*realestateprice$Feats_7[i]
              +268.8746*realestateprice$Feats_6[i]
              +303.9216*realestateprice$Feats_5[i]
              +278.0884 *realestateprice$Feats_4[i]
              +276.0710*realestateprice$Feats_3[i]
              +246.1543 *realestateprice$Feats_2[i]
              +293.9943* realestateprice$Feats_1[i]
              +18.5099* realestateprice$Feats_0[i]
              )
  }
}

realestateprice$TAX<-round(realestateprice$TAX,0)
realestateprice$k<-NULL
write.csv(realestateprice,"Imputed Data_1.csv")

#### Building the  Linear Models

lmprice <- lm(Price~
              SQ.FT
              +Age
              +NE
              +CUST
              +COR
              +TAX
              +Feats_7
              +Feats_6
              +Feats_5
              +Feats_3
              +Feats_4
              +Feats_2
              +Feats_1
              +Feats_0
          ,data=realestateprice)

summary(lmprice)
#Detecting Multicollinearity Using Variance Inflation Factors
vif(lmprice)

#### Building the  Linear Model after removing  Multicollinearity
lmpricefit <- lm(Price~.
              -Feats_5
              -Feats_4
              -Feats_3
              -Feats_2
              ,data=realestateprice)

#Detecting Multicollinearity Using Variance Inflation Factors after removing variables
vif(lmpricefit)


# Check for heteroscedasticity using  Breusch-Pagan test
bptest(lmpricefit)

#######
### Check for Normality  using Shapiro-Wilk test

##tests the null hypothesis that the sample
#of observations are drawn from a Normal distribution
#######

shapiro.test(resid(lmpricefit))

#######
### Remedies of Hetroskedasticity & Normality using box cox
########


lamdabx<-boxcox(lmpricefit)
trans_df = as.data.frame(lamdabx)
optimal_lambda = round(trans_df[which.max(lamdabx$y),1],4)
optimal_lambda

lmprice_model_cox <- lm((((Price ^ optimal_lambda) - 1) / optimal_lambda) ~
                        SQ.FT
                        +Age
                        +NE
                        +CUST
                        +COR
                        +TAX
                        +Feats_7
                        +Feats_6
                        +Feats_1
                        +Feats_0
                        ,data = realestateprice)

summary(lmprice_model_cox)
bptest(lmprice_model_cox)
shapiro.test(resid(lmprice_model_cox))
plot(lmprice_model_cox)
