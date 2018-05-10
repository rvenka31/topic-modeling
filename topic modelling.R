library("tm")
library("wordcloud")
library("slam")
library("topicmodels")
library("tidytext")
library("magrittr")
library("dplyr")
library("ggplot2")
library("RColorBrewer")
#load the file

speeches <- read.csv("/Users/mac/Downloads/inaug_speeches.csv")
View(speeches)

#get only the data for Trump speech
df <- speeches[speeches$X %in% 61,]
clean_data <- preprocessing(df)


View(df)

#preprocessing data

preprocessing <- function(document){
  df <- sapply(document,function(row) iconv(row, "latin1", "ASCII", sub="")) 
  df <- gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", df) 
  corpus = Corpus(VectorSource(df))
  corpus = tm_map(corpus,removePunctuation)
  corpus = tm_map(corpus,stripWhitespace)
  corpus = tm_map(corpus,tolower)
  #remove other words and stop words
  other_words <- c('ark','ate','ax','bay','ci','ct','dam','de','dew','dis','dot','dry','ebb','ed','em','ess','exp','ha','hat','hot','ich','ii','ing','inn','ins','ion','ism','iwo','khe','le','ll','mad','mai','mrs','nd','o','oar','odd','ome','onl','ose','pat','pra','pre','rel','rid','rio','rob','rod','sad','ses','som','son','sow','t','tap','wh','wi','wit','apt','boy','cup','din','dr','fed','icy','lit','m','non','par','red','row','sap','sit','sun','tie','vow','w','y','beg','bow','box','eve','fix','re','sky','ve','woe','won','arm','art','bar','buy','eye','fit','joy','low','n','owe','raw','six','thy','air','foe','key','lie','fly','st','ten','win','cut','job','aim','big','lay','sum','run','saw','ill','lot','bad','sea','she','try','yes','met','led','why','0','off','add','era','age','th','pay','put','set','due','mr','ago','aid','say','her','how','end','him','two','use','act','see','1','s','up','old','yet','way','nor','man','too','day','god','out','he','law','men','let','me','you','his','now','own','new','one','was','if','may','who','so','no','us','my','u','on','or','its','has','i','not','it','is','we','our','in','to','of','the')
  specific_words <- c('nationu0092s')
  corpus = tm_map(corpus,removeWords,c(stopwords("SMART"),other_words,specific_words))
  return(corpus)
}

#specific_words <- c('people','government','constitution','country','states','united','citizens','fellowcitizens','power','powers','great','made','nation')
#specific_words <- c('union')

#LDA function

top_terms_by_topic_LDA <- function(input_text, # should be a columm from a dataframe
                                   plot = T, # return a plot? TRUE by defult
                                   number_of_topics = 4) # number of topics (4 by default)
{    
  # create a corpus (type of object expected by tm) and document term matrix
  Corpus <- Corpus(VectorSource(input_text)) # make a corpus object
  DTM <- DocumentTermMatrix(Corpus) # get the count of words/document
  
  # remove any empty rows in our document term matrix (if there are any 
  # we'll get an error when we try to run our LDA)
  unique_indexes <- unique(DTM$i) # get the index of each unique value
  DTM <- DTM[unique_indexes,] # get a subset of only those indexes
  
  # preform LDA & get the words/topic in a tidy text format
  lda <- LDA(DTM, k = number_of_topics, control = list(seed = 1234))
  topics <- tidy(lda, matrix = "beta")
  
  # get the top ten terms for each topic
  top_terms <- topics %>% group_by(topic) %>% top_n(10, beta) %>% ungroup() %>% arrange(topic, -beta) # arrange words in descending informativeness
  
  # if the user asks for a plot (TRUE by default)
  if(plot == T){
    # plot the top ten terms for each topic in order
    top_terms %>% # take the top terms
      mutate(term = reorder(term, beta)) %>% ggplot(aes(term, beta, fill = factor(topic))) + geom_col(show.legend = FALSE) + facet_wrap(~ topic, scales = "free") + labs(x = NULL, y = "Beta") + coord_flip() 
  }else{ 
    # if the user does not request a plot
    # return a list of sorted terms instead
    return(top_terms)
  }
}

#word cloud to visualize the most frequent words
dtm <- TermDocumentMatrix(clean_data)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=30, random.order=FALSE,rot.per = 0.1,
          colors=brewer.pal(8, "Dark2"))

head(d,10)

wordcloud(clean_data,max.words = 20)

#barplot for the top 10 words
barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")


#Visualize the topics for the given data
top_terms_by_topic_LDA(clean_data,number_of_topics = 3)



