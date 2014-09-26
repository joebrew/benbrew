# MAKE A FOLDER IN YOUR WORKINGDIRECTORY NAMED assignment3

##########
# SET WORKING DIRECTORY
##########

if ( Sys.info()["sysname"] == "Linux" ){
  setwd("/home/joebrew/Documents/benbrew/learn")
} else {
  setwd("C:/Users/BrewJR/Documents/benbrew/learn")
}

##########
# READ IN FLORIDA DATA ON LOW BIRTH WEIGHT, POPULATION CHARACTERISTICS
########## 
dat <- read.csv("lbw.csv")

########## 
# 1. Make a column called percent_lbw - this should be the percent of births which are lbw for each county
########## 
dat$percent_lbw <- (dat$lbw_births/dat$live_births)*100

########## 
# 2. Make a column called percent_black - this should be the percentage of each county's residents which are black
########## 
dat$percent_black <- (dat$black/dat$total_pop)*100

########## 
# 3.  Make a column called percent_white - this should be the percentage of each county's residents which are white
########## 
dat$percent_white<- (dat$white/dat$total_pop)*100

########## 
# 4. Make a column called percent_other - this should be the percentage of each county's residents which are white
########## 

dat$percent_other <- (dat$other/dat$total_pop)*100
##########
# 5. MODEL BABY
##########
#histograms

#histogram showing the distribution of the percentage of the African
#American population.

hist(dat$percent_black, col="red", xlab="Percent African American",
     main="Distribution of African American Population As a Percentage 
     of the Total Population", xlim=c(0,60))

#histogram showing the distribution of the percentage of the white 
#population.

hist(dat$percent_white, col="red", xlab="Percent White", 
     main="Distribution of White Population as a 
     Percentage of the Total Population", 
     xlim=c(0,100))

#Historgram of the poverty rate. 
hist(dat$percent_other, col="red", xlab="Poverty Rate",
     main="Distribution of the Poverty Rate", xlim=c(0,10))

# plot for lbw and black

my_colors <- colorRampPalette(c("blue", "red"))(60)
dat$color_black <- my_colors[ceiling(dat$percent_black)]

plot(dat$percent_poverty, dat$percent_lbw, 
     main="Correlation between Poverty and LBW",
     ylab="Low Birth Weight", xlab="Percent African American", 
     pch=16, col=adjustcolor(dat$color_black, alpha.f=0.3),
     cex=(dat$total_pop/10000)^(1/3))
abline(lm(dat$percent_lbw~dat$percent_black),
       col=adjustcolor("black", alpha.f=0.3), lwd=3)

# plot for lbw and white

plot(dat$percent_white, dat$percent_lbw, 
     main="Low Birth Weight Among Whites",
     ylab="Low Birth Weight", xlab="Percent White", pch=16, col="blue",
     xlim=c(0,100), ylim=c(0,15))
abline(lm(dat$percent_lbw~dat$percent_white))
plot of chunk unnamed-chunk-1

# plot for lbw and other

plot(dat$percent_other, dat$percent_lbw, 
     main="Low Birth Weight Among Other Races",
     ylab="Low Birth Weight", xlab="Percent OtherRaces ", pch=16, col="blue",
     xlim=c(0,20), ylim=c(0,15))
abline(lm(dat$percent_lbw~dat$percent_other))
plot of chunk unnamed-chunk-1

# plot for lbw and poverty

plot(dat$percent_poverty, dat$percent_lbw, 
     main="Low Birth Weight Among Lower Incomes",
     ylab="Low Birth Weight", xlab="Poverty Rate", pch=16, col="blue",
     xlim=c(0,40), ylim=c(0,15))
abline(lm(dat$percent_lbw~dat$percent_poverty))
plot of chunk unnamed-chunk-1

#plot for poverty and black

plot(dat$percent_black, dat$percent_poverty, 
     main="Poverty and Race",
     ylab="Poverty Rate", xlab="Percent African American", pch=16, col="blue",
     xlim=c(0,100), ylim=c(0,40))
abline(lm(dat$percent_poverty~dat$percent_black))
plot of chunk unnamed-chunk-1

#plot for poverty and white

plot(dat$percent_white, dat$percent_poverty, 
     main="Poverty and Race",
     ylab="Poverty Rate", xlab="Percent White", pch=16, col="blue",
     xlim=c(0,100), ylim=c(0,40))
abline(lm(dat$percent_poverty~dat$percent_white))
plot of chunk unnamed-chunk-1

#plot for poverty and other

plot(dat$percent_other, dat$percent_poverty, 
     main="Poverty and Race",
     ylab="Poverty Rate", xlab="Percent Other Races", pch=16, col="blue",
     xlim=c(0,20), ylim=c(0,40))
abline(lm(dat$percent_poverty~dat$percent_other))

###############################################################################

# regression models


# regress lbw on black

lm1<- lm(dat$percent_lbw ~ dat$percent_black)
summary(lm1)
## 
## Call:
## lm(formula = dat$percent_lbw ~ dat$percent_black)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -1.659 -0.730 -0.181  0.570  3.315 
## 
## Coefficients:
##                   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)         8.3369     0.2339   35.64   <2e-16 ***
## dat$percent_black   0.0137     0.0134    1.03     0.31    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.02 on 65 degrees of freedom
## Multiple R-squared:  0.0159, Adjusted R-squared:  0.000792 
## F-statistic: 1.05 on 1 and 65 DF,  p-value: 0.309
# a 10 percent increase in the population of african americans in a county,
# is associated with a .10% increase in the low birth weight for that county. 

#regress lbw on poverty

lm2 <- lm(dat$percent_lbw ~ dat$percent_poverty)
summary(lm2)
## 
## Call:
## lm(formula = dat$percent_lbw ~ dat$percent_poverty)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -2.154 -0.609 -0.118  0.548  2.365 
## 
## Coefficients:
##                     Estimate Std. Error t value Pr(>|t|)    
## (Intercept)           7.2404     0.3970   18.24   <2e-16 ***
## dat$percent_poverty   0.0811     0.0237    3.42   0.0011 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.942 on 65 degrees of freedom
## Multiple R-squared:  0.153,  Adjusted R-squared:  0.14 
## F-statistic: 11.7 on 1 and 65 DF,  p-value: 0.00108
# a 10% increase in the poverty rate for a county is associated with 
# a .8% increase in the low birth rate.

#regress lbw on poverty and black

lm3 <- lm(dat$percent_lbw ~ dat$percent_black + dat$percent_poverty)
summary(lm3)
## 
## Call:
## lm(formula = dat$percent_lbw ~ dat$percent_black + dat$percent_poverty)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -2.152 -0.617 -0.119  0.557  2.374 
## 
## Coefficients:
##                     Estimate Std. Error t value Pr(>|t|)    
## (Intercept)           7.2335     0.4070   17.77   <2e-16 ***
## dat$percent_black     0.0012     0.0131    0.09    0.927    
## dat$percent_poverty   0.0804     0.0250    3.21    0.002 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.949 on 64 degrees of freedom
## Multiple R-squared:  0.153,  Adjusted R-squared:  0.126 
## F-statistic: 5.77 on 2 and 64 DF,  p-value: 0.00497
#while holding constant the effects of the poverty rate in a county, a 10% 
#increase in the African American population is associated with a .01% 
#increase in low birth weight. And while holding constant the effect of the
#african american population in a county, a 10% increase in the poverty rate
#associated with a .8% increase in low birth weight, suggesting that the 
#poverty rate has the highest association with low birth weight in our 
#data set.


#seems like black and poverty have a strong relationship, so should we do something
#about multicollinearity?

#correlation between black and poverty
cor(dat$percent_poverty,dat$percent_black)
## [1] 0.2974
#the correlation between these two variables is 30%

#regress poverty on black

lm4 <- lm(dat$percent_poverty ~ dat$percent_black)
summary(lm4)
## 
## Call:
## lm(formula = dat$percent_poverty ~ dat$percent_black)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -6.971 -3.620 -0.929  3.371 11.711 
## 
## Coefficients:
##                   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)         13.722      1.085   12.65   <2e-16 ***
## dat$percent_black    0.156      0.062    2.51    0.015 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4.71 on 65 degrees of freedom
## Multiple R-squared:  0.0884, Adjusted R-squared:  0.0744 
## F-statistic:  6.3 on 1 and 65 DF,  p-value: 0.0145
#a 10 % increase in the African American Population
#is associate with a 1.5% increase in the poverty rate.

#3d plot of lbw on poverty and black

library(rgl)
## Warning: package 'rgl' was built under R version 3.0.3
library(car)
## Warning: package 'car' was built under R version 3.0.3
## 
## Attaching package: 'car'
## 
## The following object is masked from 'package:rgl':
## 
##     identify3d
library(scatterplot3d)
## Warning: package 'scatterplot3d' was built under R version 3.0.2
scatter3d(dat$percent_lbw ~ dat$percent_poverty + dat$percent_black, 
          fit="linear",
          ylab="Low Birth Weight", xlab="Poverty", zlab="Black")
## Loading required package: mgcv
## This is mgcv 1.7-22. For overview type 'help("mgcv-package")'.