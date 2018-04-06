library(readtext)
library(tidytext)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(wordcloud)
library(reshape2)

# read in text
jfk <- readtext("JFK first state of the union.txt") %>% 
  unnest_tokens(word, text)

obama <- readtext("obama first state of the union.txt") %>% 
  unnest_tokens(word, text)

# joy, anger, anticipation
nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

nrc_anger <- get_sentiments("nrc") %>% 
  filter(sentiment == "anger")

nrc_anticipation <- get_sentiments("nrc") %>% 
  filter(sentiment == "anticipation")

### OBAMA
# obama joy: 52 words
obama_joy <- obama %>% 
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)

obama_joy_wordcloud <- obama_joy %>% 
  with(wordcloud(word, n))

# obama anger: 51 words
obama_anger <- obama %>%
  inner_join(nrc_anger) %>%
  count(word, sort = TRUE)

obama_anger_wordcloud <- obama_anger %>%
  with(wordcloud(word, n))

# obama anticipation: 77 words
obama_anticipation <- obama %>%
  inner_join(nrc_anticipation) %>%
  count(word, sort = TRUE)

obama_anticipation_wordcloud <- obama_anticipation %>%
  with(wordcloud(word, n))


# positive vs. negative overall
bing_word_counts <- obama %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

obama %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)



### JFK
# jfk joy: 62 words
jfk_joy <- jfk %>% 
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)

jfk_joy_wordcloud <- jfk_joy %>% 
  with(wordcloud(word, n))

# jfk anger: 51 words
jfk_anger <- jfk %>%
  inner_join(nrc_anger) %>%
  count(word, sort = TRUE)

jfk_anger_wordcloud <- jfk_anger %>%
  with(wordcloud(word, n))

# jfk anticipation: 89 words
jfk_anticipation <- jfk %>%
  inner_join(nrc_anticipation) %>%
  count(word, sort = TRUE)

jfk_anticipation_wordcloud <- jfk_anticipation %>%
  with(wordcloud(word, n))


# positive vs. negative overall
bing_word_counts <- jfk %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

jfk %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)
