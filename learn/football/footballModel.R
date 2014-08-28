#######
# READ IN YOUR FOOTBALL SPREADSHEET FROM THE INTERNET, NAMING IT FB
#######
library(RCurl)
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))

myLink <- "https://docs.google.com/spreadsheets/d/1zWrcgoJXjs-bFT08DRGFilZmzwDHT41b51enFQDI_ac/export?&format=csv"
myCsv <- getURL(myLink)
fb <- read.csv(textConnection(myCsv))
rm(myLink, myCsv)

######
# MAKE A VARIABLE CALLED "spread"
# THIS WILL BE UF POINTS MINUS OPPONENT POINTS (so, negative number means a loss)
######
fb$spread <- fb$points - fb$opp_points

######
# MAKE SOME INTERESTING SCATTERPLOTS
######



######
# MAKE SOME INTERESTING HISTOGRAMS
######



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
myvars <- names(fb) %in% c("points", "opp_points", "opp", "result") 
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

# OPPONENT
pred <- lm(spread ~ opp, data = fb)
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

fb14$spread <- predict(pred, newdata = fb14)
