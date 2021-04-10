library(dplyr)
library(httr)
library(tm)
library(wordVectors)
library(word2vec)
library(genius)
library(rword)

temp <- tempfile()
download.file("http://mattmahoney.net/dc/text8.zip", destfile = temp)
(file_list <- as.character(unzip(temp, list = TRUE)$Name))
unzip(temp)
unlink(temp) # Delete temp file
file.rename(from="text8",to="text8.txt") # append an extension to the filename

T1 = Sys.time()
wiki_skip <- train_word2vec('text8.txt', output_file = "wiki_skip.bin", vectors=
                          75, window = 12, cbow= 0, threads = 8 ,iter = 5, force = TRUE)
wiki_cbow <- train_word2vec('text8.txt', output_file = "wiki_cbow.bin", vectors=
                              75, window = 12, cbow= 1, iter = 5, threads = 8 , force = TRUE)
(Elapsed4 = T1-Sys.time())

cookbook_skip = train_word2vec("cookbooks.txt","cookbook_vectors_skips.bin", vectors = 75, threads = 8, cbow = 0, window = 12, iter = 5, negative_samples = 10, force = TRUE)
cookbook_cbow = train_word2vec("cookbooks.txt","cookbook_vectors_cbow.bin", vectors = 75, threads = 8, cbow = 1, window = 12, iter = 5, negative_samples = 10, force = TRUE)


wiki_skip %>% closest_to(~ "megadeth" - "mustaine" + "nirvana")
wiki_cbow %>% closest_to(~ "metallica" - "slash" + "nirvana")
wiki_skip %>% closest_to(~ "japan" - "japanese" + "english")
wiki_skip %>% closest_to(~ "nirvana" - "cobain" + "mustaine")
wiki_skip %>% closest_to('sushi')
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
lyrics = rbind(Lover, Folklore)
lyrics %>% group_by(track_title, album)

model = word2vec(lyrics$lyric,'cbow')

library(tidytext)
paste(lyrics$lyric[lyrics$track_title=="cardigan"], sep = '')
lyrics %>% unnest_tokens('word','lyric') %>% group_by(track_title) %>% select(word)
word2vec(lyrics %>% filter(track_title == "cardigan"), 'cbow')
c = lyrics %>% filter(track_title == "cardigan")
w1 = word2vec(c$lyric, 'cbow')
c = lyrics %>% filter(track_title == "the 1")
w2 = word2vec(c$lyric, 'cbow')
cosineSimilarity(w1,w2)

library(text2vec)
text2vec::create_vocabulary(lyrics %>% unnest_tokens(output = word, input = lyric, token = 'words'))
wordcount = lyrics %>% unnest_tokens(output = word, input = lyric, token = 'words')
wordcount %>% group_by(track_title)
lyrics %>% select(-line)
test = paste(lyrics$lyric, collapse = ' ')
track_list = lyrics %>% select(track_title) %>% unique()
lyrics_merged = NULL
for(songNumber in 1:nrow(track_list))
{
  temp = lyrics %>% filter()
}


wiki_cbow %>% closest_to(wiki_cbow[[ingredients]],10)
term_set = lapply(ingredients,function(ingredient) {
  nearest_words = wiki_cbow %>%closest_to(wiki_cbow[[ingredient]],50)
  nearest_words$word}) %>% unlist
subset = wiki_cbow[[term_set,average=F]]
subset %>% cosineDist(subset) %>% as.dist %>% hclust %>% plot

tastes = wiki_cbow[[ingredients,average=F]]
wiki_cbow%>% cosineSimilarity(tastes)