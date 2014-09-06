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


######################################
#  PREDICTION MODEL
######################################
# RANK AND OPPONENT RANK
pred <- lm(spread ~ rank + opp_rank, data = fb)
summary(pred)

# RANK, OPPONENT RANK, HOME VS. AWAY
pred <- lm(spread ~ rank + opp_rank + home_away, data = fb)
summary(pred)

#Rank, Opponent, Rank, Home Vs Away, Time, Month, number of game##############
pred <- lm(spread ~ rank + opp_rank + home_away + time, data = fb)


#########
# ONCE WE'RE HAPPY WITH OUR MODEL, LET'S READ IN THE DATASET OF THIS YEAR
##########
myLink <- "https://docs.google.com/spreadsheets/d/1vnF4wpnq6wmRc4_dj2uKGYyDPrRtWb-usvw13Xt8lVI/export?&format=csv"
myCsv <- getURL(myLink)
fb14 <- read.csv(textConnection(myCsv))
rm(myLink, myCsv)

#########
# READ IN BENS PREDICTIONS
########## 
bp <- read.csv("bensPredictions.csv")

# ADD BEN'S PREDICTIONS TO fb14
fb14$spread_ben <- bp$spread


###########
# ADD JOE'S PREDICTIONS
###########

# LOAD CDPH PACKAGE

#GAM
library(mgcv)
my_gam <- gam(spread ~ s(year) + s(game) + home_away + s(rank, opp_rank, bs = "ts"),
              data = fb)

vis.gam(my_gam, view=c("rank", "opp_rank"),
          ticktype="detailed", 
          #color="jet",
          n.grid=100,
          plot.type="contour")

# RANDOM FOREST
library(randomForest)
my_rf <- randomForest(spread ~ year + game + home_away + rank + opp_rank,
                      data = fb)

######
# MAKE PREDICTIONS
######
fb14$spread_joe_rf <- predict(my_rf, newdata = fb14)
fb14$spread_joe_gam <- predict(my_gam, newdata = fb14)



######
#READ IN INTUITION STUFF
######
setwd("C:/Users/BrewJR/Documents/benbrew/learn/football")
int <- read.csv("intuition.csv")
par(mar=c(7,4,1,1))

bp <- barplot(fb14$spread_ben, ylim = c(-13, 32),
              names.arg = gsub(" ", "\n",fb14$opp), las = 3, cex.names = 0.65,
              col = adjustcolor("black", alpha.f=0.3),
              border = adjustcolor("black", alpha.f=0.6),
              space = c(0.4, rep(1.4, 11)),
              width = 0.5,
              offset = 0)
# barplot(fb14$spread_joe_gam, col = adjustcolor("blue", alpha.f=0.3), add = T,
#         names.arg= NA)
bp2 <- barplot(fb14$spread_joe_rf, col = adjustcolor("red", alpha.f=0.3), add = T,
        names.arg = NA, border = adjustcolor("red", alpha.f=0.6),
        space = 1.4,
        width = 0.5)

text(x = bp[,1] - 0.1,
     y =  fb14$spread_ben, #c(-11),
     col = adjustcolor("black", alpha.f=0.6),
     labels = paste(round(fb14$spread_ben, digits = 1)),
     cex = 0.5,
     pos = ifelse(fb14$spread_ben > 0, 3, 1)
     )

text(x = bp2[,1] +0.1,
     y = fb14$spread_joe_rf, #  c(-13), # 
     col = adjustcolor("red", alpha.f=0.6),
     labels = paste(round(fb14$spread_joe_rf, digits = 1)),
     cex = 0.5,
     pos = ifelse(fb14$spread_ben > 0, 3, 1)
     )

legend(x = "topright",
       fill = adjustcolor(c("black", "red"), alpha.f=0.3),
       border = adjustcolor(c("black", "red"), alpha.f=0.6),
       legend = c("Ben", "Joe"),
       bty = "n")
title(main = "2014 spread predictions\n(Spread = UF score minus opponent score)",
      line = -1, cex.main =0.87)

# 
# text(x = bp[,1], 
#      y =  - 25, #fb14$spread_ben,
#      #pos = 3,
#      labels = paste("J:", round(fb14$spread_joe_rf, digits = 1), "\n",
#                     "B:", round(fb14$spread_ben, digits = 1)))
