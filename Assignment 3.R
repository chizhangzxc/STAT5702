library(tidytext)
library(tidyverse)
library(tm)
library(janitor)
library(topicmodels)
library(tidyr)
library(janeaustenr)
library(dplyr)
library(stringr)
library(genius)
library(jsonlite)
library(tibble)

Artist <- "Taylor Swift"
Album <- "Folklore"
track_list = genius_tracklist(Artist, Album)

Folklore = NULL
for(songNumber in 1:nrow(track_list))
{
  temp = genius_url(track_list$track_url[songNumber])
  while(nrow(temp) == 0)
  {
    Sys.sleep(3)
    temp = genius_url(track_list$track_url[songNumber])
  }
  Folklore = rbind(Folklore, temp)
}
Folklore$track_title =  gsub("\u200b", "", Folklore$track_title)

Album <- "Lover"
Lover = NULL

track_list = genius_tracklist(Artist, Album)

for(songNumber in 1:nrow(track_list))
{
  temp = genius_url(track_list$track_url[songNumber])
  while(nrow(temp) == 0)
  {
    Sys.sleep(3)
    temp = genius_url(track_list$track_url[songNumber])
  }
  Lover = rbind(Lover, temp)
}
Lover$track_title =  gsub("\u200b", "", Lover$track_title)

Lover = Lover %>% mutate(album = 'Lover')
Folklore = Folklore %>% mutate(album = 'Folklore')

lyrics = rbind(Lover, Folklore) %>% unnest_tokens(output = word,input = lyric, token = "words") %>% inner_join(get_sentiments("nrc") )
Sents = lyrics %>%group_by(album)%>%count(sentiment)

Sents %>% filter(!(sentiment =="positive"|sentiment=="negative")) %>% ggplot(aes(fill=sentiment, y=n, x=album)) + geom_bar(position="fill", stat="identity")
MyTable = tabyl(lyrics
                %>%group_by(album),sentiment,album)
MyTable = tabyl(lyrics %>%group_by(album),sentiment,album) %>% filter(!(sentiment =="positive"|sentiment=="negative"))
chisq.test(MyTable)
lyrics = rbind(Lover, Folklore) %>% unnest_tokens(output = word,input = lyric, token = "words") %>% inner_join(get_sentiments("bing") )
Sents = lyrics %>%group_by(album)%>%count(sentiment)
MyTable = tabyl(lyrics
                %>%group_by(album),sentiment,album)
chisq.test(MyTable)
fisher.test(MyTable)


data("AssociatedPress")
APLDA = LDA(AssociatedPress,k=5)
AP_topics = tidy(APLDA, matrix = "beta")
AP_TopWords = AP_topics %>% group_by(topic) %>% top_n(10, beta) %>% ungroup() %>% arrange(topic, -beta)

AP_TopWords %>%
   mutate(term = reorder_within(term, beta, topic)) %>% # Used for faceting (glue topic to term)
  #basically make sure that topic 1 is my topic #1
 ggplot(aes(term, beta, fill = factor(topic))) +
   geom_col(show.legend = FALSE) +
   facet_wrap(~ topic, scales = "free") +
   coord_flip() +
   scale_x_reordered()


metadata <- fromJSON("https://data.nasa.gov/data.json")
names(metadata$dataset)
nasa_desc <- tibble(title = metadata$dataset$title, desc = metadata$dataset$description)

nasa_desc[nasa_desc$title=="Gazetteer of Planetary Nomenclature",]



wordcount = nasa_desc %>% unnest_tokens(output = word, input = desc) %>% group_by(title) %>% count(word, sort = TRUE) %>% ungroup()

DTM = cast_dtm(data = wordcount, term = word, document = title,value = n)
DTM95 = removeSparseTerms(DTM,.95)

nrow(as.matrix(DTM95))
nrow(DTM)

Ndocs = nrow(DTM)
wordcount = wordcount %>% mutate(Ndocs=Ndocs)
Ndocswithword = wordcount %>% group_by(word) %>% count() %>% ungroup()
Ndocswithword = Ndocswithword %>% rename(NdocsWithWord =n) #rename col
wordcountidf = left_join(wordcount, Ndocswithword) # extend the tibble
wordcountidf = wordcountidf %>% mutate(idf = log(Ndocs/ NdocsWithWord)) # extend the tibble with the idf column

freq_by_rank = wordcountidf %>%
   group_by(title) %>%
   mutate(rank = row_number()) 
rank1words = freq_by_rank%>% filter(rank ==1)
head(sort(table(rank1words$word),decreasing=TRUE),10)

WordsInDoc = wordcountidf %>% group_by(title)%>% summarize(NwordsinDoc = sum(n))

wordcountidf = left_join(wordcountidf ,WordsInDoc)

wordcountTFIDF = wordcountidf %>% mutate(termfreq = n/NwordsinDoc, tfidf
                                         = n/NwordsinDoc*idf)

# wordcountTFIDF = wordcountTFIDF %>% bind_tf_idf(word,id,n)


wordcountTFIDF %>%
   select(-idf,-termfreq,-Ndocs,-NdocsWithWord,-tfidf) %>%
   arrange(desc(tf_idf))

SimplerCols = wordcountTFIDF %>%
   select(-idf,-termfreq,-Ndocs,-NdocsWithWord,-tfidf) %>%
   arrange(tf_idf)


sort(apply(as.matrix(DTM95)>0,2,sum),decreasing=TRUE)[1:10]


wordcount = wordcount %>% bind_tf_idf(word,title,n)
wordcount = wordcount%>% filter(tf_idf>.001) #remove stop words
wordcount%>% filter(tf_idf<=.001) 
