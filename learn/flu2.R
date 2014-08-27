###################
# 1. USE THE FOLLOWING SCRIPT TO READ IN THE FLU DATA
###################

library(RCurl)
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))

myLink <- paste0("https://docs.google.com/spreadsheets/d/1KpUK8IX0E5L9MhGvdS5jbtqDh2GB0PGx4ii",
                 "wux89Nqk/export?&format=csv")

myCsv <- getURL(myLink)
ir <- read.csv(textConnection(myCsv))

rm(myCsv, myLink)



library(googleVis)
M <- gvisMotionChart(data = ir, 
                     idvar = "school", 
                     timevar = "year",
                     xvar = "year",
                     yvar = "immRate",
                     sizevar = "totMem")
plot(M)


#####################
# 1. READ IN THE MOSQUITO DATA
#####################
mosq <- read.csv("https://raw.githubusercontent.com/joebrew/fdoh/master/public/mosquito/cesar/time_series.csv")
mosq <- mosq[,c("date", "nTraps", "tot", "year", "day")]

rt <- read.csv("https://raw.githubusercontent.com/joebrew/fdoh/master/public/mosquito/rainAndTemp/rainAndTempUpdated.csv")
id <- read.csv("https://github.com/joebrew/fdoh/blob/master/public/mosquito/id.csv")

#####################
#
#####################


#####################
#
#####################

#####################
#
#####################

#####################
#
#####################

