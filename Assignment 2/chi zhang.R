load("Beatles Song Album Year.Rdata")
library(tidytext)
library(dplyr)
library(stringr)

data <- Lyric.album.year.keep[order(Lyric.album.year.keep$year),] %>% select(album, year) %>% unique()
Lyric.album.year.keep %>% subset(year==1963)
  unnest_tokens(input = songlyric, output = words, token = "words") %>% unique()
  
data$album
num_songs <- count(Lyric.album.year.keep, album = album) %>% arrange(sapply(album, function(y) which(y == data$album)))
Lyric.album.year.keep %>% mutate(num_songs)




data <- data %>% mutate(num_songs)

nrow(filter(Lyric.album.year.keep, album == data$album[1]) %>% unnest_tokens(output = words, input = songlyric, token = 'words') %>% unique())
wordcount = Lyric.album.year.keep %>%
   unnest_tokens(output = word, input = songlyric) %>%
   #anti_join(stop_words) %>%
 group_by(track_title) %>%
   count(word,sort=TRUE)%>%
   ungroup()

filter(Lyric.album.year.keep, album == data$album[1]) %>% unnest_tokens(output = words, input = songlyric, token = 'words') %>% count(words)
unique_words <- sapply(data$album, function(x) nrow(filter(Lyric.album.year.keep, album == x) %>% unnest_tokens(output = words, input = songlyric, token = 'words') %>% count(words)
))

data <- data %>% mutate(unique_words)
data <- data %>% mutate(words_per_song = data$unique_words/data$n)

gtools::permutations(12,12)
data %>% select(album, words_per_song)
filter(Lyric.album.year.keep, track_title == 'And I Love Her') %>% unnest_tokens(output = words, input = songlyric, token = 'words') %>% count(words)
temp_song <- (filter(Lyric.album.year.keep, track_title == 'And I Love Her') %>% unnest_tokens(output = words, input = songlyric, token = 'words'))
filter(Lyric.album.year.keep, track_title == 'A Taste of Honey')$songlyric[str_detect(filter(Lyric.album.year.keep, track_title == 'Twist and Shout')$songlyric, pattern = '\\bit\\b')]


library(genius)
genius_lyrics("The Beatles", "A Taste of Honey'")


test_data <- Lyric.album.year.keep %>% mutate(words = 
  sapply(Lyric.album.year.keep$track_title, function(x) nrow(filter(Lyric.album.year.keep, track_title == x) %>% unnest_tokens(output = words, input = songlyric, token = 'words') %>% count(words)
)))
library(rcompanion)
factored_album <- factor(data$album)
pairwisePermutationTest(words ~ album, data = filter(test_data, album %in% c("Please Please Me", "Let It Be")))

filter(test_data, album == 'Please Please Me') %>% select(words) %>% sum() / nrow(filter(test_data, album == 'Please Please Me') %>% select(words))

library(broom)

test_statistic <- function(data)
{
  return (tidy(anova(lm(words ~ album, data = data)))$statistic[1])
}

tidy(anova(lm(words ~ album, data = test_data)))$statistic[1]

filtered_data <- filter(test_data, album %in% c("Please Please Me", "Let It Be"))

TestStatFromData <- TestStat(filtered_data)
NPermute = 10000
Samples <- numeric(NPermute)
for(perms in 1:NPermute){
  Index = sample(1:nrow(filtered_data),size = nrow(filtered_data), replace = FALSE)
  filtered_data[Index,]
  Samples[perms] = filtered_data %>% mutate(album = album[Index]) %>%
    TestStat()
  if(perms %% 1000 == 0)
    print(perms)
}
(Pvalue = sum(Samples>TestStatFromData)/NPermute)
hist(Samples,100);abline(v=TestStatFromData ,lwd=3,col="red")



ttt <- t.test(MASS::Boston$medv)
ci <- ttt$conf.int

qnorm(.975)
qt(.975, length(MASS::Boston$medv))
