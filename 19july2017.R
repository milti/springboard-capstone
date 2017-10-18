knitr::opts_chunk$set(echo = TRUE)
library(RNeo4j)
library(twitteR)
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumer_key <- '8D843UpqjLV3kOhckizpHXKd4'
consumer_secret <- 'EXRkgexU1KLCZFeVEfrbgfHYvRFdqCVCxmYaJw06ze6nEQZInj'
access_token <- '1240280636-VNnxhIGWUP5UDWftvU8Ts7tb2ypdjAhFKBIhjBY'
access_secret <- 'I2lRPYXhGgdsUJhqbxzoC2CSuov7ClAlDkjuabXVLzBVF'
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

library(ROAuth)
my_oauth <- OAuthFactory$new(consumerKey=consumer_key,consumerSecret=consumer_secret, requestURL=requestURL, accessURL=accessURL, authURL=authURL)
my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
save(my_oauth, file = "my_oauth.Rdata")


sessionInfo()
library(reticulate)
library(XML)
path_to_python <- "/usr/local/bin/python3.6"
use_python(path_to_python)
dbot=import("debot")
db = dbot$DeBot('YocR3mKAc7U6hjKZrNnOCk2jjnWrqgSfwWYo8yOb')#This is my API key
bots=htmlParse(db$get_related_bots('election2016'))#Change 'election2016' to any other topic of your interest
#Now parse the XML data into a dataframe
botlist <- xmlToList(bots)
library(twitteR)
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumer_key <- '8D843UpqjLV3kOhckizpHXKd4'
consumer_secret <- 'EXRkgexU1KLCZFeVEfrbgfHYvRFdqCVCxmYaJw06ze6nEQZInj'
access_token <- '1240280636-VNnxhIGWUP5UDWftvU8Ts7tb2ypdjAhFKBIhjBY'
access_secret <- 'I2lRPYXhGgdsUJhqbxzoC2CSuov7ClAlDkjuabXVLzBVF'
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
map_data(botlist)
library(maps)
library(tidyverse)
install.packages("listviewer")
library(listviewer)
lapply(botList, function(x) x[[1]])
sapply(botlist, `[[`, 1)
lapply(botlist, `[[`, 1)
flatbot <- flatten(botlist)
flatterbot <- flatten(flatbot)
remove(flatbot)
flatbot <- flatten(flatterbot)
flatbot %>% map_chr("screen_name")
botbod <- flatbot %>% map_chr("screen_name")
lapply(flatbot, `[[`, 2)
lapply(flatbot, `[[`, 1)
flatbot
lapply(flatbot, `[`, 2)
unname(sapply(flatbot, `[`, 2))
newBotlist <- unname(sapply(flatbot, `[`, 2))
as.data.frame(newBotlist)
View(newBotlist)
botAcctlist <- lapply(newBotlist,getUser)
?twitteR
library(twitteR)
botAcctlist <- lapply(newBotlist,getUser)
botAcctlist <- lapply(newBotlist,lookupUsers)
botAcctlist <- sapply(newBotlist,lookupUsers)
botAcctlist <- vapply(newBotlist,lookupUsers)
botAcctlist <- apply(newBotlist,lookupUsers)
botAcctlist <- lapply(newBotlist,getUser())
botAcctlist <- lapply(newBotlist[],getUser())
savehistory("~/R/springboard\ twitteR/19july2017.R")
savehistory("~/R/github/springboard\ capstone/19july2017.R")
savehistory("~/R/github/springboard\ capstone/19july2017.R")
