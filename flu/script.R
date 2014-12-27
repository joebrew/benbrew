library(Hmisc)
library(dplyr)
library(AER)

###
# set working directory (this is a conditional depending on whose computer is being used)
###
if ( Sys.info()["sysname"] == "Linux" ){
  public <- "/home/joebrew/Documents/benbrew/flu"
  private <- "/media/joebrew/JB/fdoh/private/ben"
} else if(Sys.info()["user"] == "BrewJR" ){
  public <- "C:/Users/BrewJR/Documents/benbrew/flu"
  private <- "E:/fdoh/private/ben"
} else if (Sys.info()["user"] == "Ben"){
  public <- "C:/Users/Ben/Documents/benbrew/flu"
  private <- "C:/Users/Ben/Documents/private/"
} else{
  private <- "C:/Users/Laila/Desktop/R"
  public <- "C:/Users/Laila/Desktop/R"  
} 

setwd(public)

###
# SOURCE SOME FUNCTIONS
###
source("functions.R")

###
# read in private data (private, stored in private directory)
###
setwd(private)
dat <- read.csv("obesity_flu_absences_merged2.csv") #2 is the version ben fixed with school grades

###
# Simple probit model
###
myprobit <- glm(cbind(dat$ab_flu, dat$pres_flu) ~ 
                  v + race + lunch_rec + year + grade, 
                data=dat,
                family=binomial("probit"))

## model summary
summary(myprobit)

###
# Instrumental variable regression
###

# Good article on R code similar to stata for IVR2
# https://diffuseprior.wordpress.com/2012/05/03/an-ivreg2-function-for-r/

myivr2 <- ivreg2(form = ab_flu ~ v + race + lunch_rec + year + grade,
       endog = "z",
       iv=c("ab_non_flu"),
       data=na.omit(dat))


myivr <- ivreg(cbind(dat$ab_flu, dat$pres_flu) ~ 
                 v + race + lunch_rec + year + grade,
            data = dat)
myivr

myivr2 <- ivreg(cbind(dat$ab_flu, dat$pres_flu) ~ v + race + lunch_rec + year + grade |
             v + race + lunch_rec + year + grade, data = dat)
myivr2

# interpretation: Relative to the reference group (Asians)
# being black increases your z-score by x, whereas
# not being on free lunch reduces your z-score by x
# http://www.ats.ucla.edu/stat/r/dae/probit.htm

# confidence intervals
confint(myprobit)

###
# NUMBER OF ENTRIES BY SCHOOL AND YEAR
###
sy <- dat %>%
  group_by(school, year) %>%
  summarise(n = n())

###
# T TEST BETWEEN ABSENTEEISM AND ALL OTHER VARIABLES
####
#catbi
#racebi

# ###
# # Clean up school grades
# ###
# dat$school_grade_2011 <- as.character(dat$X2011grade)
# dat$school_grade_2012 <- as.character(dat$X2012grade)
# dat$school_grade_2013 <- as.character(dat$X2013grade)
# 
# dat$school_grade <- NA
# dat$school_grade[which(dat$year == 2011)] <- dat$school_grade_2011[which(dat$year == 2011)]
# dat$school_grade[which(dat$year == 2012)] <- dat$school_grade_2012[which(dat$year == 2012)]
# dat$school_grade[which(dat$year == 2013)] <- dat$school_grade_2013[which(dat$year == 2013)]
# 
# #eliminate spaces
# dat$school_grade <- gsub(" ", "", dat$school_grade)
# 
# # eliminate extra variables
# dat$school.1 <- NULL
# dat$X2011grade <- NULL
# dat$X2012grade <- NULL
# dat$X2013grade <- NULL
# dat$school_grade_2011 <- NULL
# dat$school_grade_2012 <- NULL
# dat$school_grade_2013 <- NULL
# 
# # write csv 
# write.csv(dat, "obesity_flu_absences_merged2.csv")

###
# SUMMARIZE ABSENTEEISM BY GRADE
###

df_grade <- dat %>%
  group_by(grade) %>%
  summarise(absences = sum(ab_flu, na.rm = T),
            presences = sum(pres_flu, na.rm = T),
            flu_days = sum(flu_days, na.rm = T))

df_grade$fsar <-
  df_grade$absences / 
  df_grade$flu_days * 100

###
# SUMMARIZE ABSENTEEISM BY SCHOOL YEAR
###

df_year <- dat %>%
  group_by(year) %>%
  summarise(absences = sum(ab_flu, na.rm = T),
            presences = sum(pres_flu, na.rm = T),
            flu_days = sum(flu_days, na.rm = T))

df_year$fsar <-
  df_year$absences / 
  df_year$flu_days * 100


###
# SUMMARIZE IMMUNIZATION STATUS
###
table(dat$v)
prop.table(table(dat$v))

df_v <- dat %>% 
  group_by(year) %>% 
  summarise(v_rate= length(v[which(v == "Yes")]) / 
              length(v[which(!is.na(v))]))

# t-test absenteeism rate of imm and not immunized
x <- dat$ab_flu_per[which(dat$v == "Yes")]
y <- dat$ab_flu_per[which(dat$v == "No")]
t.test(x, y)

# Likelihood to immunize by other factors
fit <- glm(v == "Yes" ~
             lunch_rec, 
           data = dat,
           family = binomial("logit"))

summary(fit)
exp(coef(fit))

###
# SUMMARIZE BMI PERCENTILE FOR AGE
###
table(dat$cat)
prop.table(table(dat$cat))

hist(dat$z, 
     col = "lightblue",
     main = "Histogram of BMI Z score for age",
     xlab = "Z-score")



# Correlation with absenteeism

# first reorder factor
dat$cat <- factor(dat$cat,
                  levels = c("normal", 
                             "overweight", 
                             "obese"))

df_fat <- dat %>%
  group_by(cat) %>%
  summarise(absences = sum(ab_flu, na.rm = T),
            presences = sum(pres_flu, na.rm = T),
            flu_days = sum(flu_days, na.rm = T))

df_fat$fsar <-
  df_fat$absences / 
  df_fat$flu_days * 100

# number of people
barplot(prop.table(table(dat$cat)),
        col = "lightblue")

# fsar
bp <- barplot(df_fat$fsar, 
        names.arg = df_fat$cat,
        col = "lightblue")

###
# RACE
###
df_race <- dat %>%
  group_by(race) %>%
  summarise(absences = sum(ab_flu, na.rm = T),
            presences = sum(pres_flu, na.rm = T),
            people = n(),
            flu_days = sum(flu_days, na.rm = T))

df_race$fsar <-
  df_race$absences / 
  df_race$flu_days * 100

# add ci
df_race$lb <- 
  simpasym(n = df_race$flu_days,
         p = df_race$fsar / 100)$lb * 100
df_race$ub <- 
  simpasym(n = df_race$flu_days,
           p = df_race$fsar / 100)$ub * 100

# order it
df_race <- df_race[order(df_race$fsar),]

# plot it
df_race <- df_race[which(!is.na(df_race$fsar)),]
bar_fun(var = df_race$fsar,
        names.arg = c("Asian",
                      "Indian", 
                      "White",
                      "Hispanic", 
                      "Multiracial", 
                      "Black"), #df_race$race,
        xlab = "Race",
        ylab = "Flu season absenteeism rate",
        add_ci = TRUE,
        yplus = df_race$ub,
        yminus = df_race$lb)

# Test the statistical significant differences
dat$race <- factor(dat$race, 
                   levels=c("W", "A",
                "I","H", "M", "B"))
fit <- glm(cbind(dat$ab_flu, dat$pres_flu) ~ 
             race, 
           data=dat,
           family=binomial("logit"))
summary(fit)


###
# FREE AND REDUCED LUNCH
###
df_lunch_rec <- dat %>%
  group_by(lunch_rec) %>%
  summarise(absences = sum(ab_flu, na.rm = T),
            presences = sum(pres_flu, na.rm = T),
            flu_days = sum(flu_days, na.rm = T))

df_lunch_rec$fsar <-
  df_lunch_rec$absences / 
  df_lunch_rec$flu_days * 100

# add ci
df_lunch_rec$lb <- 
  simpasym(n = df_lunch_rec$flu_days,
           p = df_lunch_rec$fsar / 100)$lb * 100
df_lunch_rec$ub <- 
  simpasym(n = df_lunch_rec$flu_days,
           p = df_lunch_rec$fsar / 100)$ub * 100

# plot it
df_lunch_rec <- df_lunch_rec[which(!is.na(df_lunch_rec$lunch_rec)),]
bar_fun(var = df_lunch_rec$fsar,
        names.arg = df_lunch_rec$lunch_rec,
        xlab = "Free/reduced lunch status",
        ylab = "Flu season absenteeism rate",
        add_ci = TRUE,
        yplus = df_lunch_rec$ub,
        yminus = df_lunch_rec$lb)

###
# GrADE OF STUDENT
###
dat$grade <- factor(dat$grade)
df_grade <- dat %>%
  group_by(grade) %>%
  summarise(absences = sum(ab_flu, na.rm = T),
            presences = sum(pres_flu, na.rm = T),
            flu_days = sum(flu_days, na.rm = T))

df_grade$fsar <-
  df_grade$absences / 
  df_grade$flu_days * 100

# add ci
df_grade$lb <- 
  simpasym(n = df_grade$flu_days,
           p = df_grade$fsar / 100)$lb * 100
df_grade$ub <- 
  simpasym(n = df_grade$flu_days,
           p = df_grade$fsar / 100)$ub * 100

# plot it
df_grade <- df_grade[which(!is.na(df_grade$grade)),]
bar_fun(var = df_grade$fsar,
        names.arg = df_grade$grade,
        xlab = "Grade",
        ylab = "Flu season absenteeism rate",
        add_ci = TRUE,
        yplus = df_grade$ub,
        yminus = df_grade$lb)


###
# School Grade
###
df_school_grade <- dat %>%
  group_by(school_grade) %>%
  summarise(absences = sum(ab_flu, na.rm = T),
            presences = sum(pres_flu, na.rm = T),
            flu_days = sum(flu_days, na.rm = T))

df_school_grade$fsar <-
  df_school_grade$absences / 
  df_school_grade$flu_days * 100

# add ci
df_school_grade$lb <- 
  simpasym(n = df_school_grade$flu_days,
           p = df_school_grade$fsar / 100)$lb * 100
df_school_grade$ub <- 
  simpasym(n = df_school_grade$flu_days,
           p = df_school_grade$fsar / 100)$ub * 100

# plot it
df_school_grade <- df_school_grade[which(!is.na(df_school_grade$school_grade)),]
bar_fun(var = df_school_grade$fsar,
        names.arg = df_school_grade$school_grade,
        xlab = "School grade",
        ylab = "Flu season absenteeism rate",
        add_ci = TRUE,
        yplus = df_school_grade$ub,
        yminus = df_school_grade$lb)

###
# nfsar
###
plot(dat$ab_flu_per, dat$ab_non_flu_per)
# jitter to make more visible
plot(jitter(dat$ab_flu_per, factor = 200), 
     jitter(dat$ab_non_flu_per, factor = 200),
     col = adjustcolor("lightblue",
                       alpha.f = 0.2),
     pch = 16,
     xlab = "Flu season absenteeism rate",
     ylab = "Non flu season absenteeism rate")
fit <- lm(ab_non_flu_per ~
            ab_flu_per,
          data = dat)
abline(fit,
       lwd = 2,
       col = adjustcolor("darkred", alpha.f = 0.6))

fit <- glm(cbind(dat$ab_non_flu, dat$pres_non_flu) ~ 
             race, 
           data=dat,
           family=binomial("logit"))
summary(fit)



###
# Omit all observations with any NA (we're not doing this)
###
dat_sans_na <- na.omit(dat)

# What percentage of the dataset did we just lose?
nrow(dat_sans_na) / nrow(dat) * 100

# Given that this is so much of the data, we're gonna move 
# forward omitting NA's on a as-needed basis

###
# Summarize entire dataset
###
summary(dat)

###
# Break up data by year and describe
###
datbis2011 <- subset(dat, dat$year==2011)
describe(datbis2011)
#1916 observations

datbis2012 <- subset(dat, dat$year==2012)
describe(datbis2012)
#3030 observations

datbis2013 <- subset(dat, dat$year==2013)
describe(datbis2013)
#3693 observations

describe(dat)

## 1. Ethnic characteristics

# Weight type per type of race
attach(dat)
mytable <- table(dat$race,dat$cat) # A will be rows, B will be columns 
mytable # print table 

margin.table(mytable, 1) # A frequencies (summed over B) 
margin.table(mytable, 2) # B frequencies (summed over A)

x <- round (prop.table(mytable, 1), digits = 4) * 100 # row percentages 
y <- round (prop.table(mytable, 2), digits = 4) * 100 # row percentages 

barplot(x, beside = T, legend = T)
abline(h=seq(0,100,10), col = adjustcolor("black", alpha.f = 0.2),
       lty = 6)

barplot(y, beside = T, legend = T)
abline(h=seq(0,100,10), col = adjustcolor("black", alpha.f = 0.2),
       lty = 6)

print(mytable)

# Proportions of weight categories per race
x <- round(prop.table(table(dat$cat, dat$race), margin = 2) * 100, digits = 1)
x     
barplot(x, beside = T, legend = T)
abline(h=seq(0,100,10), col = adjustcolor("black", alpha.f = 0.2),
       lty = 6)

# Proportions of weight categories among White and nonWhite
y <- round(prop.table(table(dat$cat, dat$race_rec), margin = 2) * 100, digits = 1)
y    
barplot(y, beside = T, legend = T)
abline(h=seq(0,100,10), col = adjustcolor("black", alpha.f = 0.2),
       lty = 6)

# Free lunches and ethnicity
attach(dat)
ethnfree <- table(dat$race,dat$lunch_rec) # A will be rows, B will be columns 
ethnfree # print table 

margin.table(ethnfree, 1) # A frequencies (summed over B) 
margin.table(ethnfree, 2) # B frequencies (summed over A)

prop.table(ethnfree) # cell percentages
z <- round (prop.table(ethnfree, 1), digits = 4) * 100 # row percentages 
t <- round (prop.table(ethnfree, 2), digits = 4) * 100 # row percentages 

print(ethnfree)

barplot(z, beside = T, legend = T)
abline(h=seq(0,100,10), col = adjustcolor("black", alpha.f = 0.2),
       lty = 6)


# Other freelunch graphs per ethnicity
y <- round(prop.table(table(dat$lunch, dat$race), margin = 2) * 100, digits = 1)
y     
barplot(y, beside = T, legend = T)
abline(h=seq(0,100,10), col = adjustcolor("black", alpha.f = 0.2),
       lty = 6)



# Consent form and race

#interpret the results with caution, almost 1 missing value out of 2
describe(cf)

d <- round(prop.table(table(dat$cf, dat$race), margin = 2) * 100, digits = 1)
d
barplot(d, beside = T, legend = T, xlab="Ethnicity", ylab="% of answers to consent form")
abline(h=seq(0,100,10), col = adjustcolor("black", alpha.f = 0.2),
       lty = 6)

# free lunch variable repartition

hist(lunch,
     xlab = "Free Lunch Variable Distribution",
     breaks = 60,
     xlim = c(0,10),
     col = adjustcolor("blue", alpha.f = 0.5),
     main = NA)


# 2. Health characteristics of children

# School level repartition per year
round(prop.table(table(dat$year, dat$type), 1) * 100, digits=2)
round(prop.table(table(dat$year, dat$type), 2) * 100, digits=2)

# BMI repartition per White/nonWhite
y <- round(prop.table(table(dat$cat, dat$race_rec), margin = 2) * 100, digits = 1)
y
barplot(y, beside = T, legend = T)
abline(h=seq(0,100,10), col = adjustcolor("black", alpha.f = 0.2),
       lty = 6)

# Quantiles population of children 
y <- round(prop.table(table(dat$type, dat$year), margin = 2) * 100, digits = 1)
y
barplot(y, beside = T, legend = T)
abline(h=seq(0,100,10), col = adjustcolor("black", alpha.f = 0.2),
       lty = 6)



# Age repartition of children

hist(dat$age /12,
     xlab = "Age",
     breaks = 100,
     xlim = c(0,20),
     col = adjustcolor("blue", alpha.f = 0.5),
     main = NA)


# Relationship b/w height and weight for children
plot(dat$height, dat$weight,
     col = adjustcolor("blue", alpha.f = 0.2),
     pch = 16,
     xlab = "Height (inches)",
     ylab = "Weight (pounds)")
points(dat$height, dat$weight,
       col = adjustcolor("black", alpha.f = 0.2))


# Quantiles population of children 
y <- round(prop.table(table(dat$cat, dat$year), margin = 2) * 100, digits = 1)
y
barplot(y, beside = T, legend = T)
abline(h=seq(0,100,10), col = adjustcolor("black", alpha.f = 0.2),
       lty = 6)

# 3. Immunization

(mean(imm_rate)) * 100
immratepctage <- imm_rate*100

# necessary to get rid of NA values before observing quantiles (since NA for mean value)
immratepctage <- na.omit(immratepctage)

boxplot(immratepctage,
        main = "Repartition of immunization",
        ylab = "Percentage of kids immunized")
abline(h=seq(0,100,10), col = adjustcolor("black", alpha.f = 0.2),
       lty=6)

hist(immratepctage,
     main = "Repartition of immunization",
     xlab = "Percentage of kids immunized",
     ylab = "Number of kids concerned")

quantile(immratepctage)

# Immunization rates per ethnicity

z <- round(prop.table(table(dat$imm, dat$race), margin = 2) * 100, digits = 1)
z     
barplot(z, beside = T, legend = T, xlab='Ethnicity', ylab='% kids immunized or not')
abline(h=seq(0,100,10), col = adjustcolor("black", alpha.f = 0.2),
       lty = 6)


# Consent form and age of children

d <- round(prop.table(table(dat$imm, dat$cf), margin = 2) * 100, digits = 1)
d
barplot(d, beside = T, legend = T, xlab="Answer to consent form", ylab="% of kids immunized")
abline(h=seq(0,100,10), col = adjustcolor("black", alpha.f = 0.2),
       lty = 6)


# Freelunch and immunization
b <- round(prop.table(table(dat$imm, dat$lunch_rec), margin = 2) * 100, digits = 1)
b     
barplot(b, beside = T, legend = T)
abline(h=seq(0,100,10), col = adjustcolor("black", alpha.f = 0.2),
       lty = 6)


# Absences flu and non-flu children
#### Starting with a regression to test a simple causality
probit1 <- glm(dat$imm ~ dat$cf + dat$imm_rate + dat$cf * dat$imm_rate, family = binomial(link=probit))

## model summary
summary(probit1)


# Absences flu and non-flu children

#idem, we have to get rid of the NA values before
imm_rate_complete <- na.omit(dat$imm_rate)
imm_rate_complete <- imm_rate_complete * 100
mean(imm_rate_complete)
quantile(imm_rate_complete)


# Corrélation flu absences et immunization (of the whole class / of the child)

ab_flu_complete <- na.omit(dat$ab_flu)
imm_complete <- na.omit(dat$imm)
ab_non_flu_complete <- na.omit(dat$ab_non_flu)

#cf_dummy <- as.numeric(dat$cf)
#cf_dummy_complete <- na.omit(cf_dummy)
#cor(ab_non_flu_complete, ab_flu_complete)


rcorr(imm_complete, ab_flu_complete)
cor(imm_complete, ab_flu_complete)
cor(imm_complete, imm_rate_complete)
cor(ab_non_flu_complete, ab_flu_complete)


## GENERAL OBSERVATIONS

library(pastecs)

age_years <- age_months/12

obsvar1<-cbind(ab_flu_complete, ab_non_flu_complete)
options(scipen=100)
options(digits=2)
## for complete descr stats (i didn't want that much info though)
## stat.desc(varobserved)
stat.desc(obsvar1, basic=F)

obsvar2<-cbind(bmi, grade, age_years, height, weight)
options(scipen=100)
options(digits=2)
stat.desc(obsvar2, basic=F)


###
# read in nurse data (stored in the public directory)
###
setwd(public)
nurses <- read.csv("nurses.csv")

###
# check to see if any nurses worked in more than one place
###

# the following will group nurses by each nurse and 
# get the number of schools each has worked out
library(dplyr)
nurses %>% 
  group_by(nurse) %>%
  summarise(x = length(unique(school))) %>%
  arrange(desc(x))

###
# clean up a bit (remove outliers, etc.)
###

###
# explore a bit (and consider cleaning for outliers)
###

# relationship b/w height and weight
plot(dat$height, dat$weight,
     col = adjustcolor("blue", alpha.f = 0.2),
     pch = 16,
     xlab = "Height (inches)",
     ylab = "Weight (pounds)")
points(dat$height, dat$weight,
       col = adjustcolor("black", alpha.f = 0.2))

# age in months (divide by 12 to get years)
hist(dat$age / 12,
     xlab = "Age",
     breaks = 100,
     xlim = c(0,20),
     col = adjustcolor("black", alpha.f = 0.6),
     main = NA)

# table of obesity category by race
x <- round(prop.table(table(dat$cat, dat$race), margin = 2) * 100, digits = 1)
x     
barplot(x, beside = T, legend = T)
abline(h=seq(0,100,10), col = adjustcolor("black", alpha.f = 0.2),
       lty = 6)

# 3d scatterplot of height, weight, and age
library(rgl)
library(car)
scatter3d(dat$weight ~ dat$height + dat$age_months, 
          fit="smooth", #linear, smooth, additive
          ylab="Weight", xlab="Height", zlab="Age",
          axis.col=c("darkmagenta", "black", "darkcyan"),
          surface.col=c("blue", "green", "orange", "magenta", "cyan", "red", 
                        "yellow", "gray"), surface.alpha=0.3,
          neg.res.col="red", pos.res.col="darkgreen",
          point.col = "darkblue")

# 3d scatterplot of: 
# 1. flu season absenteeism rate
# 2. non flu season absenteeism rate
# 3. bmi percentile for age

scatter3d(dat$ab_flu_per ~ dat$ab_non_flu_per + dat$bmi_percentile_for_age, 
          fit="smooth", #linear, smooth, additive
          ylab="Flu season ab", xlab="Non-flu season ab", zlab="BMI p for age",
          axis.col=c("darkmagenta", "black", "darkcyan"),
          surface.col=c("blue", "green", "orange", "magenta", "cyan", "red", 
                        "yellow", "gray"), surface.alpha=0.3,
          neg.res.col="red", pos.res.col="darkgreen",
          point.col = "darkblue")


############################################################################


# t-test of flu season absences b/w non immunized and immunized

t.test(x=dat$ab_flu_per[which(dat$imm_fac==TRUE)], 
       y=dat$ab_flu_per[which(dat$imm_fac==FALSE)])

# t-test of flu season absences with overweight or normal

t.test(x=dat$ab_flu_per[which(dat$cat_bi=="overweight")], 
       y=dat$ab_flu_per[which(dat$cat_bi=="normal")])

# t-test of flu season absences by race

t.test(x=dat$ab_flu_per[which(dat$race_rec=="white")], 
       y=dat$ab_flu_per[which(dat$race_rec=="nonWhite")])

# t-test of flu season absences by lunch status

t.test(x=dat$ab_flu_per[which(dat$lunch_rec=="free")], 
       y=dat$ab_flu_per[which(dat$lunch_rec=="notFree")])

# t-test of flu season absences and consent form return
t.test(x=dat$ab_flu_per[which(dat$cf=="Yes")], 
       y=dat$ab_flu_per[which(dat$cf=="No")])

#########################################################################

# relationship b/w race and absenteeism
plot(dat$race_rec, dat$ab_flu_per,
     col = adjustcolor("blue", alpha.f = 0.2),
     pch = 16,
     xlab = "Race",
     ylab = "Percent Absence During Flu Season",
     main="Race and Absenteeism")
points(dat$race_rec, dat$ab_flu_per,
       col = adjustcolor("black", alpha.f = 0.2))

mod1 <- lm(dat$ab ~ dat$race_rec)
summary(mod1)

###########################################################################

# obesity and absenteeism###

plot(dat$cat_bi, dat$ab_flu_per,
     col = adjustcolor("blue", alpha.f = 0.2),
     pch = 16,
     xlab = "Obesity",
     ylab = "Absences",
     main="Obesity and Absenteeism")
points(dat$cat_bi, dat$ab,
       col = adjustcolor("black", alpha.f = 0.2))

mod2 <- lm(dat$ab ~ dat$cat_bi)
summary(mod2)

plot(dat$weight, dat$ab_flu,
     col = adjustcolor("blue", alpha.f = 0.2),
     pch = 16,
     xlab = "Weight",
     ylab = "Absences",
     main="Weight and Absenteeism")
points(dat$weight, dat$ab,
       col = adjustcolor("blue", alpha.f = 0.2))
abline(lm(dat$ab ~ dat$weight))

mod3 <- lm(dat$ab ~ dat$weight)
summary(mod3)


## immunization status and Absenteeism

plot(dat$imm_fac, dat$ab_flu_per,
     col=adjustcolor("blue", alpha.f=0.2),
     pch=16,
     xlab="Immunization Status",
     ylab="Absences",
     main="Immunization Status and Absenteeism")
points(dat$imm_fac, dat$ab_flu,
       col=adjustcolor("black", alpha.f=0.2))

abline(lm(dat$ab_flu ~ dat$imm_fac))
mod4 <- lm(dat$ab_flu ~ dat$imm_fac)
summary(mod4)

#lunch status and absenteeism

plot(dat$lunch_rec, dat$ab_flu,
     col=adjustcolor("blue", alpha.f=0.2),
     pch=16,
     xlab="Lunch Status",
     ylab="Absences",
     main="Lunch Status and Absenteeism")
points(dat$lunch_rec, dat$ab_flu,
       col=adjustcolor("black", alpha.f=0.2))

abline(lm(dat$ab_flu ~ dat$lunch_rec))
mod5 <- lm(dat$ab_flu ~ dat$lunch_rec)
summary(mod5)

#Herd Immunity in 2013
library(dplyr)

dat13 <- dat[which(dat$year=="2013"),]

dat13 %>% group_by(school) %>% summarise(fsar = mean(ab_flu_per))

school_ab_rate13 <- dat %>% group_by(school) %>% summarise(fsar = mean(ab_flu_per))

dat13 %>% group_by(school) %>% summarise(herd = mean(imm_rate))

imm_rate13 <- dat13 %>% group_by(school) %>% summarise(herd = mean(imm_rate))

#merge the two# i got lost and just created an excel sheet! fuck it! we'll do it live!
herd <- read.csv("herd.csv")

summary(dat13$school)

plot(herd$herd, herd$fsar,
     pch=16,
     xlab="School Immunization Rate",
     ylab="Flu Season Absentee Rate",
     main="Herd Immunity",
     col="blue")

abline(lm(herd$fsar ~ herd$herd))



############################################################################

#Transform flu season/non-flu season absences into categorical.

#regression with only immunization status
ben_model <- glm(cbind(dat$ab_flu, dat$pres_flu) ~ dat$imm_fac, 
                 weights = dat$flu_days, family = binomial("logit")) 
summary(ben_model) 
ben_model_odds_ratios <- exp(cbind(OR = coef(ben_model), 
                                   confint(ben_model)))
ben_model_odds_ratios

#logit with immunization status, free lunch, and weight.

ben_model1 <- glm(cbind(dat$ab_flu, dat$pres_flu) ~ 
                    dat$imm_fac + dat$cat_bi + dat$lunch_rec, 
                  weights = dat$flu_days, family = binomial("logit")) 
summary(ben_model1) 
ben_model_odds_ratios1 <- exp(cbind(OR = coef(ben_model1), 
                                    confint(ben_model1)))
ben_model_odds_ratios1


############################################################################
# Plot to see absenteeism seasonality
##############################

# create week day number variable


