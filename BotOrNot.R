library(caret)
library(igraph)
library(twitteR)
library(streamR)
library(RNeo4j)
library(tidyverse)
library(reticulate)
library(XML)
library(httr)


requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumer_key <- 'QpSBwPC1PwqUQ4eSMCVeXenDQ'
consumer_secret <- '39YRwPaXF9u3TCvjqFaElSxmEEe7xcUJTzg0M2oa3egutPa6B4'
access_token <- '1240280636-GFbqEWiGKiP17KmN4oyrFwdZwQBJEJA3BRy2TmW'
access_secret <- 'vsnTNryz83p0XS4rSz6dwOmGRf0SZC2pPtxGnAkvUikSR'
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
path_to_python <- "/anaconda/bin/python"
use_python(path_to_python)


dbot=import("debot")
db = dbot$DeBot('YocR3mKAc7U6hjKZrNnOCk2jjnWrqgSfwWYo8yOb')#This is my API key
cbots=htmlParse(db$get_related_bots('Charlottesville'))
#Now parse the XML data into a dataframe
botlist <- xmlToList(cbots)

flatbot <- flatten(flatten(flatten(botlist)))
newBotlist <- unlist(unname(sapply(flatbot, `[`, 2)))

botdetails <- map(newBotlist[1:100], ~twListToDF(lookupUsers(.x)))

################################
#Collect some tweets

tweets <- searchTwitteR("Charlottesville",n=1000,lang='en')
tweetsdf <- twListToDF(tweets)

#Lexical Diversity with quanteda package
library(quanteda)

#First we have to convert the text into a document-feature matrix
x <- dfm(tweetsdf$text)

#Now we can get various lexical diversity measures
ldiv <- textstat_lexdiv(x)

#This calculates various measures that you can read up on in the documentation
#The koRpus package contains even more measures like the MLTD. Try that too.


library(qdapRegex)

AppURL <- rm_between_multiple(tweetsdf$statusSource,"<",">")
tweetsdf$App <- unlist(AppURL)

appcount <- tweetsdf%>%group_by(App)%>%summarize(Count=n())%>%arrange(desc(Count))
tweetsdf <- left_join(tweetsdf,appcount)

tweetsdf$BoN <- ifelse(tweetsdf$Count>4,0,1)