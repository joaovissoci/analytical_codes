
# source https://www.r-bloggers.com/playing-with-twitter-data/

lapply(c('twitteR','stringr' ,'dplyr', 'ggplot2', 'lubridate', 
	'network', 'qgraph', 'qdap', 'tm','cowplot'),
       library, character.only = TRUE)
# theme_set(new = theme_bw())
# source('../../R/twitterAuth.R')
set.seed(95616)
library(SnowballC)
library(qgraph)
library(polycor)

# library("stringr")

# set authentication: Get ID and TOKEN information after registering your 
# application.
# https://apps.twitter.com/
# setup_twitter_oauth("", 
# 					"", 
# 					"", 
# 					"")


# #extract tweets bases on:
# #1. A string with searches. you can search more then one 
# # hashtag or user with  + sign. Ex. '@jrvissoci+@R3nza'
# # or '@jrvissoci+#datascience'
# #
# #2. n expresses the amount of tweets to extract. default is 25
# #
# #3. lang restrictis to a tweet languageEl
# #
# #4. since and until gives a flooring and ceiling data
# #
# #5. geocode gives a radius area given by lat and long
# # given a radius size in miles or km Ex. geocode='37.781157,-122.39720,1mi'
# #
# tw = searchTwitter("#UFC202",n=1e5, since="2016-08-22",
# 	until="2016-08-23")
# # length(tw)
# # saveRDS(tw, '../../R/MSST_Tweets.RDS')
# # tw = readRDS('../../R/MSST_Tweets.RDS')
# d = twListToDF(tw)

# tw_text = sapply(tw, function(x) x$getText())

# d$get_text<-tw_text

# write.csv(d,"/Users/joaovissoci/Desktop/twitter.csv")
d<-read.csv("/Users/joaovissoci/Box Sync/Home Folder jnv4/Data/personal/twitter.csv")

length(d$get_text)

#############################################################
#cleaning tweets
#############################################################

# http://stackoverflow.com/questions/31348453/how-do-i-clean-twitter-data-in-r

## something to remove stopwords

# paste(mytweets, collapse=" ")

 clean_tweet = gsub("&amp", "", d$get_text)
  clean_tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", clean_tweet)
  clean_tweet = gsub("@\\w+", "", clean_tweet)
  clean_tweet = gsub("[[:punct:]]", "", clean_tweet)
  clean_tweet = gsub("[[:digit:]]", "", clean_tweet)
  clean_tweet = gsub("http\\w+", "", clean_tweet)
  clean_tweet = gsub("[ \t]{2,}", "", clean_tweet)
  clean_tweet = gsub("^\\s+|\\s+$", "", clean_tweet) 
  clean_tweet = gsub('[^[:graph:]]', ' ',clean_tweet)

#get rid of unnecessary spaces
clean_tweet <- str_replace_all(clean_tweet," "," ")
# Get rid of URLs
# clean_tweet <- str_replace_all(clean_tweet, "http://t.co/[a-z,A-Z,0-9]*{8}","")
# Take out retweet header, there is only one
clean_tweet <- str_replace(clean_tweet,"RT @[a-z,A-Z]*: ","")
# Get rid of hashtags
clean_tweet <- str_replace_all(clean_tweet,"#[a-z,A-Z]*","")
# Get rid of references to other screennames
clean_tweet <- str_replace_all(clean_tweet,"@[a-z,A-Z]*","") 

d$clean_tweet<-clean_tweet


# Split into retweets and original tweets
sp = split(d, d$isRetweet)
orig = sp[['FALSE']]

# Extract the retweets and pull the original author's screenname
rt = mutate(sp[['TRUE']], sender = substr(text, 5, 
    regexpr(':', text) - 1))

length(rt$clean_tweet)
# text1<-gsub(txt,"[^[:graph:]]", " ") %>%
#         gsub('(\\.|!|\\?)\\s+|(\\++)', ' ', .) %>%




# pol = 
#     lapply(orig$text, function(txt) {
#         # strip sentence enders so each tweet is analyzed as a sentence,
#         # and +'s which muck up regex
#         gsub('[^[:graph:]]', ' ',txt) %>%
#         gsub('(\\.|!|\\?)\\s+|(\\++)', ' ', .) %>%
#             # strip URLs
#         gsub(' http[^[:blank:]]+', '', .) %>%
#             # calculate polarity
#     }
# )


#fazendo a mineração de texto:
corpus = VCorpus(VectorSource(orig$clean_tweet))
# corpus<- tm_map(corpus, function(x) iconv(enc2utf8(x), sub = "byte"))
# corpus <- tm_map(corpus, tolower)
# corpus <- tm_map(corpus, removePunctuation)
# corpus <- tm_map(corpus, removeWords("UFC201", stopwords("english")))
corpus <- tm_map(corpus, stemDocument)

# olhando o primeiro conteudo das entrevistas
corpus[[1]]

#transformando em matriz
dtm <- DocumentTermMatrix(corpus)
dtm

# removendo "sparse terms": vai fazer cair de 3 mil e cacetada termos para menos termos
dtm2 <- removeSparseTerms(dtm, 0.3)
dtm2

# findFreqTerms(dtm, 5)
# findAssocs(dtm, "vencer", 0.5)

#############################################################
#Identify descriptives of tweets
#############################################################
# Put in local time
d$created = lubridate::with_tz(d$created, 'America/Los_Angeles')

timeDist = ggplot(d, aes(created)) + 
    geom_density(aes(fill = isRetweet), alpha = .5) +
    scale_fill_discrete(guide = 'none') +
    xlab('All tweets')

# Zoom in on conference day
dayOf = filter(d, mday(d$created) == 22)
timeDistDayOf = ggplot(dayOf, aes(created)) + 
    geom_density(aes(fill = isRetweet), adjust = .25, alpha = .5) +
    theme(legend.justification = c(1, 1), legend.position = c(1, 1)) +
    xlab('Day-of tweets')
cowplot::plot_grid(timeDist, timeDistDayOf)

#############################################################
#Platform used
#############################################################

par(mar = c(3, 3, 3, 2))
d$statusSource = substr(d$statusSource, 
                        regexpr('>', d$statusSource) + 1, 
                        regexpr('</a>', d$statusSource) - 1)
dotchart(sort(table(d$statusSource)))
mtext('Number of tweets posted by platform')

#############################################################
#Emotional valence of tweets
#############################################################

pol = polarity(orig$clean_tweet)

orig$emotionalValence <- pol$all$polarity

# As reality check, what are the most and least positive tweets
orig$text[which.max(orig$emotionalValence)]
## [1] "Hey, this Open Science Framework sounds like a great way to  collaborate openly! Where do I sign up? Here: https://t.co/9oAClb0hCP #MSST2016"
orig$text[which(orig$emotionalValence==0)]
## [1] "1 Replications are boring 2 replications are attack 3 reputations will suffer 4 only easy ones will be done 5 bad studies are bad #MSST2016"
# How does emotionalValence change over the day?
# filter(orig, mday(created) == 22) %>%

pol_data<-subset(orig,orig$emotionalValence!=0)

pol_data$time<-as.POSIXlt(pol_data$created,format='%H:%M:%S')
str(pol_data$time)

    ggplot(pol_data, aes(time, emotionalValence)) +
    geom_point() + 
    geom_smooth(span = .5)

# Happiest tweets are mostly retweeted?
ggplot(orig, aes(x = emotionalValence, y = retweetCount)) +
    geom_point(position = 'jitter') +
    geom_smooth()

#############################################################
#Emotional content of tweets
#############################################################

pol = 
    lapply(orig$text, function(txt) {
        # strip sentence enders so each tweet is analyzed as a sentence,
        # and +'s which muck up regex
        gsub('[^[:graph:]]', ' ',txt) %>%
        gsub('(\\.|!|\\?)\\s+|(\\++)', ' ', .) %>%
            # strip URLs
        gsub(' http[^[:blank:]]+', '', .) %>%
            # calculate polarity
        polarity()
    }
)

polWordTables = 
    sapply(pol, function(p) {
        words = c(positiveWords = paste(p[[1]]$pos.words[[1]], collapse = ' '), 
                  negativeWords = paste(p[[1]]$neg.words[[1]], collapse = ' '))
        gsub('-', '', words)  # Get rid of nothing found's "-"
    }) %>%
    apply(1, paste, collapse = ' ') %>% 
    stripWhitespace() %>% 
    strsplit(' ') %>%
    sapply(table)

par(mfrow = c(1, 2))
invisible(
    lapply(1:2, function(i) {
    dotchart(sort(polWordTables[[i]],decreasing=TRUE)[20:1], cex = .8)
    mtext(names(polWordTables[i]))
    }))

#############################################################
#Emotionally associated non-emotional words
#############################################################

polSplit = split(orig, sign(orig$emotionalValence))
polText = sapply(polSplit, function(df) {
	    gsub('[^[:graph:]]', ' ',df$text) %>%
    paste(tolower(.), collapse = ' ') %>%
        gsub(' (http|@)[^[:blank:]]+', '', .) %>%
        gsub('[[:punct:]]', '', .)
    }) %>%
structure(names = c('negative', 'neutral', 'positive'))

# remove emotive words
polText['negative'] = removeWords(polText['negative'], 
	names(polWordTables$negativeWords))
polText['positive'] = removeWords(polText['positive'], 
	names(polWordTables$positiveWords))

# Make a corpus by valence and a wordcloud from it
corp =  VCorpus(VectorSource(polText))
# col3 = RColorBrewer::brewer.pal(3, 'Paired') # Define some pretty colors, mostly for later
wordcloud::comparison.cloud(as.matrix(TermDocumentMatrix(corp)), 
           max.words = 100, min.freq = 2, random.order=FALSE, 
       rot.per = 0, colors = c("red","blue","green"), 
       vfont = c("sans serif", "plain"))

#############################################################
#Social network of tweets and re-tweets and mentions
#############################################################

# Adjust retweets to create an edgelist for network
el = as.data.frame(cbind(sender = tolower(rt$sender), 
                         receiver = tolower(rt$screenName)))
el = count(el, sender, receiver) 
rtnet = network(el, matrix.type = 'edgelist', directed = TRUE, 
                ignore.eval = FALSE, names.eval = 'num')

# Get names of only those who were retweeted to keep labeling reasonable
vlabs = rtnet %v% 'vertex.names'
vlabs[degree(rtnet, cmode = 'outdegree') == 0] = NA

par(mar = c(0, 0, 3, 0))
plot(rtnet, label = vlabs, label.pos = 5, label.cex = .8, 
     vertex.cex = log(degree(rtnet)) + .5, vertex.col = col3[1],
     edge.lwd = 'num', edge.col = 'gray70', main = '#MSST2016 Retweet Network')

### SNA Mention

# Extract who is mentioned in each tweet. 
# Someone has probably written a function to do this, but it's a fun regex problem.
mentioned = 
    lapply(orig$text, function(tx) {
        matches = gregexpr('@[^([:blank:]|[:punct:])]+', tx)[[1]]
        sapply(seq_along(matches), function(i) 
            substr(tx, matches[i] + 1, matches[i] + attr(matches, 'match.length')[i] - 1))
    })
# Make an edge from the tweeter to the mentioned, for each mention
mentionEL = 
    lapply(seq_along(orig$text), function(i) {
        # If the tweet didn't have a mention, don't make edges
        if(mentioned[[i]] == '')  
            return(NULL)
        # Otherwise, loop over each person mentioned, make an edge, and rbind them
        lapply(mentioned[[i]], function(m)
            c(sender = orig$screenName[i], receiver = m)) %>%
            do.call(rbind, .) %>% as.data.frame()
    }) %>% 
    do.call(rbind, .) %>%
    count(tolower(sender), tolower(receiver))

# Make the network
mentionNet = network(mentionEL, matrix.type = 'edgelist', directed = TRUE, 
                ignore.eval = FALSE, names.eval = 'num')

# Color speakers and the host
vCol = rep(col3[3], network.size(mentionNet))
speakers = c('duncantl', 'rlucas11', 'cristobalyoung5', 'katiecorker', 
             'mcxfrank', 'tracykteal', 'siminevazire', 'jwpatty', 
             'kramtrak', 'phylogenomics', 'donandrewmoore')
vCol[(mentionNet %v% 'vertex.names') %in% speakers] = col3[1]
vCol[mentionNet %v% 'vertex.names' == 'ucdavisiss'] = col3[2]

plot(mentionNet, displaylabels = TRUE, label.pos = 5, label.cex = .8, 
     vertex.cex = degree(mentionNet, cmode = 'indegree'), vertex.col = vCol,
     edge.lwd = 'num', edge.col = 'gray70', main = '#MSST2016 Mention Network')
legend(x = 'bottomleft', legend = c('Speaker', 'Host', 'Other'), 
       pt.bg = col3, pch = 21, pt.cex = 1.5, bty = 'n')

#############################################################
#Network of words
#############################################################
network_data<-subset(orig,orig$emotionalValence!=0)

network_data$clean_tweet <- removeWords(network_data$clean_tweet,c("ufc"))

network_data$clean_tweet <- removeWords(network_data$clean_tweet,
    findFreqTerms(dtm, 1))

network_data$clean_tweet <- removeWords(network_data$clean_tweet,
    findFreqTerms(dtm, 2))

#fazendo a mineração de texto:
corpus = VCorpus(VectorSource(network_data$clean_tweet))
# corpus<- tm_map(corpus, function(x) iconv(enc2utf8(x), sub = "byte"))
# corpus <- tm_map(corpus, tolower)
# corpus <- tm_map(corpus, removePunctuation)
# corpus <- tm_map(corpus, removeWords("UFC201", stopwords("english")))


corpus <- tm_map(corpus, stemDocument)

#transformando em matriz
dtm <- DocumentTermMatrix(corpus,control=list(stopwords=TRUE))
dtm

findFreqTerms(dtm, c(5))

# removendo "sparse terms": vai fazer cair de 3 mil e cacetada termos para menos termos
dtm2 <- removeSparseTerms(dtm, 0.9)
dtm2

# Put in local time
data<-as.matrix(dtm)
#data[data>=1] <- 1
dataMatrix <- t(data) %*% data

cor <- cor(data,method="spearman")
names<-rownames(cor)


Q1_atleta1 <- qgraph(dataMatrix, borders = FALSE, cut=10, 
  minimum = 5, labels=names,label.cex = 0.60, label.color="black",
  layout = "spring",directed=FALSE,label.scale=FALSE,
  # posCol=c("#BF0000","red"),
  gray=FALSE)

Q2_atleta2 <- qgraph(cor, borders = FALSE, cut=1, 
  minimum = 0.5, labels=names,label.cex = 0.60, label.color="black",
  layout = "spring",directed=FALSE,label.scale=FALSE,
  posCol=c("steelblue","steelblue"),
  gray=FALSE)

Q3_atleta3 <- qgraph(cor$correlations, borders = FALSE, cut=1, 
  minimum = 0.5, labels=names,label.cex = 0.60, label.color="black",
  layout = "spring",directed=FALSE,label.scale=FALSE,gray=FALSE)


col=brewer.pal(6,"Dark2")
wordcloud(corpus, min.freq=25, scale=c(5,2),rot.per = 0.25,
          random.color=T, max.word=45, random.order=F,colors=col)

