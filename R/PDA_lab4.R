##### Panel Data: Analysis and Applications for the Social Sciences, 2022
##### Lab 4. FE- and RE-models (part 1)

# install.packages("haven")
# install.packages("plm")
# install.packages("dplyr")
# install.packages("lmtest")

library(haven)
library(plm)
library(dplyr)
library(lmtest)

panel<-read_dta("./data/PDA_lab4.dta")
panel <- na.omit(panel)
head(panel, 10) # nested data

# run a least-squares dummy-variable model  
LSDV <- lm(lncrime~lnpolice + lndensity + factor(county_number), data = panel)
summary(LSDV)

# change a base (reference) category
countyref <- relevel(factor(panel$county_number), ref = "2")
LSDV_2 <- lm(lncrime ~ lnpolice + lndensity + countyref, data = panel)
summary(LSDV_2)

# run a fixed-effects model (only intercepts are different). Within-group transformation
fe <- plm(lncrime ~ lnpolice + lndensity, data = panel, index = c("county_number", "year"), 
          effect = "individual", model = "within")
summary(fe)
# extract county effects
summary(fixef(fe))

############################### How to get a FE-model coefficient in multiple regression? ############################### 
# For ease of understanding, the code is divided into steps

#Step 1. Partialling out 
panel$x_res <- lm(lnpolice ~ lndensity + factor(county_number), data = panel)$res 
panel$y_res <- lm(lncrime ~ lndensity + factor(county_number), data = panel)$res 

#Step 2. Group (country) variances of X ("clean" lnpolice)
var1 <- summarize(group_by(panel, county_number), var(x_res))
var1

#Step 3. Run separate regressions (for each county) 
beta1 <- group_by(panel, county_number) %>% 
  summarize(coef(lm(y_res ~ x_res))[2])
beta1

#Step 4. Weighted sum of coefficients from separate regression models. 
# Use conditional variances as weights
m1 <- as.data.frame(merge(beta1, var1, by ="county_number"))
m1
colnames(m1) <- c("county", "beta1", "var(x_res)")
m1$coef <- (m1$beta1*m1$`var(x_res)`)/sum(m1$`var(x_res)`)
sum(m1$coef)

#########################################################################################################################

### Do we need a fixed-effects model? 
# run a pooled model by using plm package
pooled <- plm(lncrime ~ lnpolice + lndensity, data=panel, 
              index=c("county_number", "year"), model="pooling")
summary(pooled)

pFtest(fe, pooled)

# run a random-effects model (only intercepts are different)
re <- plm(lncrime ~ lnpolice + lndensity, data=panel, 
          index=c("county_number", "year"), model="random")
summary(re)

# Breush-Pagan test. Do we need a random-effects model?  
plmtest(pooled, type=c("bp"))

# Hausman test: FE VS RE original
phtest(fe, re)

### Test whether the fixed-effects model fits well
y_pred <- LSDV$fitted 
panel1 <- data.frame(panel, y_pred) 

# subset the data by countries and find how strongly y observed and predicted are correlated
merged <- panel1 %>% group_by(county_number)%>% 
  summarize(cor(lncrime, y_pred))%>% merge(panel1, ., by="county_number")

# test whether results are robust to excluding observations with small correlations
merged$new <- ifelse(abs(merged$`cor(lncrime, y_pred)`)<0.1,1,0)
fe2 <- plm(lncrime ~ lnpolice + lndensity, merged[merged$new == 0,], 
           index=c("county_number", "year"), effect = "individual", model = "within")
summary(fe2)

### Heteroskedasticity adjustment 
coeftest(fe2, vcov = vcovHC, type = "HC3")
