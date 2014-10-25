###
# set working directory (this is a conditional depending on whose computer is being used)
###
if ( Sys.info()["sysname"] == "Linux" ){
  public <- "/home/joebrew/Documents/fdoh/public/flu"
  private <- "/media/joebrew/JB/fdoh/private/ben"
} else if(Sys.info()["user"] == "BrewJR" ){
  public <- "C:/Users/BrewJR/Documents/fdoh/public/flu"
  private <- "E:/fdoh/private/ben"
} else {
  public <- "C:/Users/benbrew/Documents/fdoh/public/flu"
  private <- "C:/Users/benbrew/Documents/private/"
}
setwd(public)

###
# read in data (private, stored in private directory)
###
setwd(private)
dat <- read.csv("obesity_flu_absences_merged.csv")

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

###
#
###


###
#
###


###
#
###