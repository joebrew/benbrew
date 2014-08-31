#######
# READ IN YOUR FOOTBALL SPREADSHEET FROM THE INTERNET, NAMING IT FB
#######
library(RCurl)
## Loading required package: bitops
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))

myLink <- "https://docs.google.com/spreadsheets/d/1zWrcgoJXjs-bFT08DRGFilZmzwDHT41b51enFQDI_ac/export?&format=csv"
myCsv <- getURL(myLink)
fb <- read.csv(textConnection(myCsv))
rm(myLink, myCsv)

#fix data
library(car)
summary(fb$home_away)
##    Away    Home       N Neutral 
##      57      95       1      26
fb$home_away <- factor(Recode(fb$home_away, "'N' = 'Neutral'"))

#recode time of day into 4 groups#################################################

fb$time <- factor(Recode(fb$time, "'12'='noon'; '1230' = 'noon'; '230'= 'afternoon';
                         '1' = 'noon'; '11'=  'noon'; '130' = 'noon';
                         '2' = 'afternoon'; '330'= 'afternoon';
                         '4'= 'afternoon'; '5' = 'evening';
                         '6' = 'evening'; '7' = 'night'; '715' = 'night'; '730'= 'night';
                         '830'= 'night';'8' = 'night'; '830' = 'night'"))

#change name###########################################################################

fb$game <- (fb$game_.)

#recode ranking into 4 categories, in case we would rather use that###############################


fb$rank_cat <- factor(ifelse(fb$rank >= 1 & fb$rank <= 10, "rank1-10", 
                             ifelse(fb$rank > 10 & fb$rank <= 20, "rank11-20",
                                    ifelse(fb$rank > 20 & fb$rank <= 25, "rank21-25",
                                           ifelse(fb$rank > 25, "unranked", "unranked")))))

fb$opprank_cat <- factor(ifelse(fb$opp_rank >= 1 & fb$opp_rank <= 10, "rank1-10", 
                                ifelse(fb$opp_rank > 10 & fb$opp_rank <= 20, "rank11-20",
                                       ifelse(fb$opp_rank > 20 & fb$opp_rank <= 25, "rank21-25",
                                              ifelse(fb$opp_rank > 25, "unranked", "unranked")))))



######
# MAKE A VARIABLE CALLED "spread"
# THIS WILL BE UF POINTS MINUS OPPONENT POINTS (so, negative number means a loss)
######
fb$spread <- fb$points - fb$opp_points

######
# MAKE SOME INTERESTING SCATTERPLOTS
######
names(fb)

#I'm trying to plot year and that seasons winning pct. not working at all. What is 
#a more efficient way to do this

# FIRST LET'S GET A VECTOR OF ALL THE YEARS
# WE'LL CALL IT TS (FOR TIMESERIES)

ts <- data.frame(unique(sort(fb$year)))
names(ts) <- "year"

# NOW, LET'S FILL IN OUR "WINNING PERCENTAGE" COLUMN WITH NA'S
fb$wp <- NA

# NOW WE'LL WRITE A LOOP TO GET THE WINNING PERCENTAGE FOR EACH YEAR
for (i in ts$year){ # for each and every year in the ts dataframe
  ts$wp[which(ts$year == i)] <- # assign to the corresponding wp
    100 * (nrow(fb[which(fb$year == i & fb$result == "W"),]) /  # number of wins divided by
           nrow(fb[which(fb$year == i),])) #number of games
}

# NOW THAT YOU HAVE THAT, YOU COULD LOOP IT BACK INTO FB IF YOU WANTED
fb$wp <- NA
for (i in ts$year){
  fb$wp[which(fb$year == i)] <-
    ts$wp[which(ts$year == i)]
}

# WE CAN USE TS TO MAKE OUR PLOT

plot(ts$year, ts$wp,
     type = "l", 
     xlab = "Year", 
     ylab = "Winning percentage",
     ylim=c(0,100),
     lwd =2, 
     col = adjustcolor("black", alpha.f=0.4),
     main = "Winning percentage by year")

# # I'vE COMMENDED OUT ALL YOUR STUFF BELOW
# fb_win2000 <- nrow(fb[which(fb$result=="W" & fb$year==2000),])/
#   nrow(fb[which(fb$game_. >=1 & fb$year==2000),])*100
# 
# fb_win2001 <- nrow(fb[which(fb$result=="W" & fb$year==2001),])/
#   nrow(fb[which(fb$game >=1 & fb$year==2001),])*100
# 
# fb_win2002 <- nrow(fb[which(fb$result=="W" & fb$year==2002),])/
#   nrow(fb[which(fb$game >=1 & fb$year==2002),])*100
# 
# fb_win2003 <- nrow(fb[which(fb$result=="W" & fb$year==2003),])/
#   nrow(fb[which(fb$game >=1 & fb$year==2003),])*100
# 
# fb_win2004 <- nrow(fb[which(fb$result=="W" & fb$year==2004),])/
#   nrow(fb[which(fb$game >=1 & fb$year==2004),])*100
# 
# fb_win2005 <- nrow(fb[which(fb$result=="W" & fb$year==2005),])/
#   nrow(fb[which(fb$game >=1 & fb$year==2005),])*100
# 
# fb_win2006 <- nrow(fb[which(fb$result=="W" & fb$year==2006),])/
#   nrow(fb[which(fb$game >=1 & fb$year==2006),])*100
# 
# fb_win2007 <- nrow(fb[which(fb$result=="W" & fb$year==2007),])/
#   nrow(fb[which(fb$game >=1 & fb$year==2007),])*100
# 
# fb_win2008 <- nrow(fb[which(fb$result=="W" & fb$year==2008),])/
#   nrow(fb[which(fb$game >=1 & fb$year==2008),])*100
# 
# fb_win2009 <- nrow(fb[which(fb$result=="W" & fb$year==2009),])/
#   nrow(fb[which(fb$game >=1 & fb$year==2009),])*100
# 
# fb_win2010 <- nrow(fb[which(fb$result=="W" & fb$year==2010),])/
#   nrow(fb[which(fb$game >=1 & fb$year==2010),])*100
# 
# fb_win2011 <- nrow(fb[which(fb$result=="W" & fb$year==2011),])/
#   nrow(fb[which(fb$game >=1 & fb$year==2011),])*100
# 
# fb_win2012 <- nrow(fb[which(fb$result=="W" & fb$year==2012),])/
#   nrow(fb[which(fb$game >=1 & fb$year==2012),])*100
# 
# fb_win2013 <- nrow(fb[which(fb$result=="W" & fb$year==2013),])/
#   nrow(fb[which(fb$game >=1 & fb$year==2013),])*100
# 
# plot(x=c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 
#          2008, 2009, 2010, 2011, 2012, 2013), y=c(fb_win2000, fb_win2001, fb_win2002, fb_win2003,
#                                                   fb_win2004, fb_win2005, fb_win2006, fb_win2007, fb_win2008,
#                                                   fb_win2009, fb_win2010, fb_win2011, fb_win2012, fb_win2013))
# plot of chunk unnamed-chunk-1

#plot rushing attempts and rush yds.

plot(fb$rush_att, fb$rush_yds, 
     xlab="Rushing Attpempts",
     ylab="Rushing Yards",
     pch=16,
     col= "blue")

#plot passing att and pass yds

plot(fb$pass_att, fb$pass_yds,
     xlab="Passing Attempts", 
     ylab="Passing Yards",
     pch=16,
     col="darkorange")

#plot time of day and point spread

plot(fb$time, fb$spread,
     xlab="Time of Day",
     ylab="Point Spread",
     pch=16,
     col="grey")
# VERY INTERESTING - IT LOOKS LIKE WE DO MUCH BETTER IN EVENING GAMES.  
# IS THAT SOME SORT OF CONFOUNDING ISSUE, THOUGH?  IE, DO WE ONLY PLAY EVENING
# GAMES BECAUSE IT'S HOT / THE BEGINNING OF THE SEASON, AT WHICH TIME 
# WE USUALLY PLAY SHITTY TEAMS, TOO?

#plot spread and rank

plot(fb$opprank_cat, fb$spread,
     xlab="AP Ranking",
     ylab="Point Spread",
     pch=16,
     col="blue")

#plot wins and yds

plot(fb$result, fb$rush_yds,
     xlab="Result",
     ylab="Rushing Yards",
     pch=16,
     col="darkorange")

#plot home and away

plot(fb$home_away, fb$spread,
     xlab="Location",
     ylab="Point Spread",
     pch=16,
     col="blue")
# ANOTHER GREAT PLOT - COOL

######
# MAKE SOME INTERESTING HISTOGRAMS
######

hist(fb$opp_points,
     xlab= "Opponents Points",
     col="red",)

######
# WE'RE GONNA BE BUILDING TWO MODELS
# THE FIRST DESCRIBES WHAT CAUSES WINS / LOSSES (IE, DOES PASSING OR RUNNING OR DEFENSE MATTER MORE?)
# THE SECOND USES ONLY PRE-GAME DATA SO AS TO PREDICT THE OUTCOME OF GAMES BEFORE THEYRE PLAYED
######



######################################
# MODEL 1: WHAT CAUSES WINS / LOSSES
######################################

# CREATE A MODEL USING ALL THE PREDICTORS
fit <- lm(spread ~ ., data=fb)

# LOOK AT THE OUTPUT TO SEE WHAT'S SIGNIFICANT AND NOT
summary(fit) # STARTS INDICATE P < 0.05

# OBVIOUSLY, THIS IS A BIT PROBLEMATIC BECAUSE WE'RE OVERFITTING
# LET'S REMOVE POINTS AND OPPONENT POINTS, AS WELL AS
# TEAM NAME AND RESULT
myvars <- names(fb)%in% c("points", "opp_points", "opp", "result") 
fb2 <- fb[!myvars]

# NOW RECREATE OUR MODEL, BUT JUST USING FB2
fit <- lm(spread ~ ., data=fb2)
summary(fit)

# LOOK AT THE ABOVE SUMMARY - THIS SHOWS YOU THE MOST IMPORTANT
# COMPONENTS OF A WIN / LOSS, FROM A STATISTICAL PERSPECTIVE
# AND CHECK IT OUT = R-squared of 0.9 MEANS WE CAN EXPLAIN 90%
# OF THE VARIANCE IN POINT SPREAD THROUGH THESE VARIABLES


# WE'VE STILL GOT A LOT OF VARIABLES IN OUR MODEL, MANY OF THEM INSIGNIFICANT
# SO, LET'S USE BACKWARD STEPWISE REGRESSION TO ELIMINATE SOME

# IF YOU WANNA EXPLORE THOSE LOOK UP TEH FUNCTIONS ADD1, DROP1, STEP, ETC.


######################################
# MODEL 2: PREDICTION MODEL
######################################


#######
# CONSTRUCT A FEW SIMPLE BIVARIATE LINEAR MODELS - WHAT DO THESE TELL YOU?
#######

# HOME VS. AWAY GAMES
pred <- lm(spread ~ home_away, data = fb)
summary(pred)

# WHAT WERE YOU DOING HERE?
nrow(fb[which(fb$result=="W" & fb$home_away=="Away"),])
## [1] 38
nrow(fb[which(fb$result=="W" & fb$home_away=="Neutral"),])
## [1] 16



# OPPONENT
pred <- lm(spread ~ opp, data = fb)
summary(pred)

# TIME
pred <- lm(spread ~ time, data =fb)
summary(pred)

# MONTH
pred <- lm(spread ~ month, data=fb)
summary(pred)

# GAME
pred <- lm(spread ~ game, data=fb)
summary(pred)

########
# NOW START ADDING MORE VARIABLES TO THE MODEL
########

# RANK AND OPPONENT RANK
pred <- lm(spread ~ rank + opp_rank, data = fb)
summary(pred)

# RANK, OPPONENT RANK, HOME VS. AWAY
pred <- lm(spread ~ rank + opp_rank + home_away, data = fb)
summary(pred)

#Rank, Opponent, Rank, Home Vs Away, Time, Month, number of game##############

pred <- lm(spread ~ rank + opp_rank + home_away + time, data = fb)

summary(pred)

#########
# ONCE WE'RE HAPPY WITH OUR MODEL, LET'S READ IN THE DATASET OF THIS YEAR
##########
myLink <- "https://docs.google.com/spreadsheets/d/1vnF4wpnq6wmRc4_dj2uKGYyDPrRtWb-usvw13Xt8lVI/export?&format=csv"
myCsv <- getURL(myLink)
fb14 <- read.csv(textConnection(myCsv))
rm(myLink, myCsv)

##### 
# AS LONG AS OUR PREDICTIONS DATASET HAS ALL OF THE SAME VARIABLES
# AS THOSE IN OUR MODEL, WE CAN MAKE STATISTICAL PREDICTIONS ON EACH
# GAME'S OUTCOME
#####

# YOU WERE GETTING AN ERROR HERE BECAUSE IN YOUR 
# FB 14 DATA, YOU HAD LEFT AN EXTRA SPACE IN ONE OF THE WORDS "HOME" LIKE THIS:
# "Away"    "Home"    "Home "   "Neutral"

# SO, WHEN R SAW "HOME " (with a space), IT DIDN'T KNOW HOW TO PREDICT ON THAT
# SINCE IT HAD NEVER SEEN IT BEFORE
# I'VE GONE INTO THE GOOGLE DOC AND DELETED THE EXTRA SPACE - NOW IT WORKS

fb14$spread <- predict(pred, newdata = fb14)

plot(fb14$game, fb14$spread)

# PER OUR MODEL, WE WIN EVERY GAME!
# BUT MAYBE THAT HAS SOMETHING TO DO WITH THE FACT THAT YOU HAVE THE OPPONENTS
# ALWAYS RANKED AS 119.  WHAT'S UP WITH THAT? FIX IT, AND THEN RE RUN THIS SHIT