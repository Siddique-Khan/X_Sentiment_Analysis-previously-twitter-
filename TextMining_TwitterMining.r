# Social Media Mining
#How to download and play with your own Twitter Data
#Cleaning
#Document Term Matrix
# Wordcloud
# Network


library(twitteR)
library(RCurl) #Provides functions to allow one to
#compose general HTTP requests
library(RJSONIO) #allows conversion to and from data
#in Javascript object notation (JSON) format.
library(stringr) #Simplifies string operations 
library(tm) # Text mining
library(parallel)
library(wordcloud)
library(igraph) # creates graphs
#download.file(url="http://curl.haxx.se/ca/cacert.pem",
#                       destfile="cacert.pem")

setup_twitter_oauth(consumer_key="zzzzz",
                    consumer_secret="zzzz",
                    access_token="zzzzz",
                    access_secret="zzzz")

# If/When asked:
# Use a local file to cache OAuth access credentials between R sessions?
#1: Yes
#2: No

# Answer: 2


tweetList <- searchTwitter("#zika", n=500, lang=c("en", "eng"), since="2017-01-01")
# For Windows users
#tweetList <- searchTwitter("#nepal OR earthquake", n=500, lang=c("eng", "en"), since="2016-01-01",cainfo="cacert.pem") 
print(tweetList[1:3])
length(tweetList)

tweetDF <- do.call("rbind", lapply(tweetList,as.data.frame))
View(tweetDF)
head(tweetDF,1)

hist(tweetDF$created, breaks=10, freq=TRUE,col="tan")

tweetDF$textlen1 <-str_length(tweetDF$text)

print(head(tweetDF$textlen1))

print(tail(tweetDF$textlen1))

summary(tweetDF$textlen1)
boxplot(tweetDF$textlen1)

hist(str_length(tweetDF$text), 
     col="lightgray", 
     ylim=c(0,200),
     main="Distribution of Lengths of Tweets")

tweetDF$modtext <- str_replace_all(tweetDF$text,"  "," ")

tweetDF$textlen2 <- str_length(tweetDF$modtext)

tweetDF$wordCount<-(str_count(tweetDF$modtext," ") + 1)

summary(tweetDF$wordCount)

#Parsing
str_match(tweetDF$modtext,"RT @[a-z,A-Z]*:")[1:2]

tweetDF$rt <- str_match(tweetDF$modtext,"RT @[a-z,A-Z]*: ")

head(tweetDF$rt,3)

tweetDF$rt<-str_replace(tweetDF$rt, "RT @","")

tweetDF$rt<-str_replace(tweetDF$rt,": ","")


table(as.factor(tweetDF$rt))

barplot(table(as.factor(tweetDF$rt)),
        main="Most Influential Screennames", col="orange",
        horiz=T, las=1, cex.names  =0.55)

#Similar for URLs
tweetDF$urlist<-str_match_all(tweetDF$text,"https://t.co/[a-z,A-Z,0-9]{8}")

head(tweetDF$urlist,2)

tweetDF$numurls<-rapply(tweetDF$urlist,length)

summary(tweetDF$numurls,20)

tweetCorpus<-Corpus(VectorSource(tweetDF$modtext))

tweetCorpus <- tm_map(tweetCorpus,
                      content_transformer(function(x) iconv(x, 
                                                            to='UTF-8-MAC', 
                                                            sub='byte')),
                      mc.cores=1)

getTransformations()

toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
tweetCorpus <- tm_map(tweetCorpus, toSpace, "/")
tweetCorpus <- tm_map(tweetCorpus, toSpace, "@")
tweetCorpus <- tm_map(tweetCorpus, toSpace, "\\|")
tweetCorpus <- tm_map(tweetCorpus, toSpace, "http:")
tweetCorpus <- tm_map(tweetCorpus, toSpace, "https:")

tweetCorpus[[1]]$content
tweetCorpus<-tm_map(tweetCorpus, content_transformer(tolower), lazy=T)
tweetCorpus[[1]]$content


tweetCorpus<-tm_map(tweetCorpus, removePunctuation, 
                    preserve_intra_word_dashes = TRUE)

tweetCorpus[[1]]$content

tweetCorpus[[1]]$content
tweetCorpus<-tm_map(tweetCorpus,
                    removeWords,
                    stopwords('english'), lazy=T)
tweetCorpus[[1]]$content

tweetTDM<-TermDocumentMatrix(tweetCorpus)

inspect(tweetTDM)

findFreqTerms(tweetTDM, lowfreq=15)

toString <- content_transformer(function(x, from, to) gsub(from, to, x))

# Word merge

tweetCorpus <- tm_map(tweetCorpus, toString, "zika", 
                      "zikavirus", lazy=T)

tweetCorpus <- tm_map(tweetCorpus, toString, "zikavirusvirus", 
                      "zikavirus", lazy=T)

tweetTDM<-TermDocumentMatrix(tweetCorpus)
findFreqTerms(tweetTDM, lowfreq=15)

toString <- content_transformer(function(x, from, to) gsub(from, to, x))
# Word Elimination
tweetCorpus <- tm_map(tweetCorpus, removeWords, 
                      c("amp", "will", "tco","qrc6n9mics", "1p7rqnihbj"))

tweetTDM<-TermDocumentMatrix(tweetCorpus)
findFreqTerms(tweetTDM, lowfreq=15)

findAssocs(tweetTDM, c( "vaccine"), 
           corlimit=0.6)

tdMatrix <- as.matrix(tweetTDM) # Convert to large matrix format
sortedMatrix<-sort(rowSums(tdMatrix),decreasing=TRUE) 
# Sort by frequency of each term
cloudFrame<-data.frame(word=names(sortedMatrix),
                       freq=sortedMatrix) # Creates new dataframe

set.seed(123)
pal2=brewer.pal(8,'Dark2') # Creates nice looking color palettes
wordcloud(cloudFrame$word,
          cloudFrame$freq, min.freq=3, max.words=15,
          colors=brewer.pal(8, "Dark2"))

pal2=brewer.pal(8,'Dark2') # Creates nice looking color palettes
wordcloud(cloudFrame$word,
          cloudFrame$freq, min.freq=3, max.words=15,
          colors=brewer.pal(8, "Dark2"))

pal2=brewer.pal(8,'Dark2')
wordcloud(cloudFrame$word,
          cloudFrame$freq, min.freq=3, 
          max.words=25,colors=brewer.pal(8, "Dark2"),
          scale=c(5, 0.1))

tweetTDM.RS<-removeSparseTerms(tweetTDM, 0.95)
tdMatrix <- as.matrix(tweetTDM.RS)
tdMatrix[tdMatrix>=1]<-1

termMatrix<-tdMatrix%*%t(tdMatrix)
# Inspect
termMatrix


g<- graph.adjacency(termMatrix, weighted=T, mode="undirected")

plot(g)

simplify(graph, remove.multiple = TRUE, remove.loops = TRUE)

g<-simplify(g)

plot(g)

