#######
# READ IN YOUR FOOTBALL SPREADSHEET FROM THE INTERNET, NAMING IT FB
#######
library(RCurl)
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
myLink <- "https://docs.google.com/spreadsheets/d/1wxx9wT_kDBNreARi6AY59QfTn8ePI9qoTs37lKsWTNY/export?&format=csv"
myCsv <- getURL(myLink)
fb <- read.csv(textConnection(myCsv))
rm(myLink, myCsv)

#######
# CONSTRUCT YOUR MODEL
#######