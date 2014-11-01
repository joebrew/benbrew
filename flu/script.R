###
# set working directory (this is a conditional depending on whose computer is being used)
###
if ( Sys.info()["sysname"] == "Linux" ){
  public <- "/home/joebrew/Documents/benbrew/flu"
  private <- "/media/joebrew/JB/fdoh/private/ben"
} else if(Sys.info()["user"] == "BrewJR" ){
  public <- "C:/Users/BrewJR/Documents/benbrew/flu"
  private <- "E:/fdoh/private/ben"
} else {
  public <- "C:/Users/Ben/Documents/benbrew/flu"
  private <- "C:/Users/Ben/Documents/private/"
}
setwd(public)

###
# read in private data (private, stored in private directory)
###
setwd(private)
dat <- read.csv("obesity_flu_absences_merged.csv")

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


