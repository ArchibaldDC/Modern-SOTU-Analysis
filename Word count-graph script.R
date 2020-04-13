#THIS SCRIPT IS LARGELY BASED ON JULIA SILGE'S & DAVID ROBINSON'S "TEXT MINING WITH R"

library(readtext)
library(tidytext)
library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggthemes)


#92d refers to the 92nd congress in Nixon's speech
custom.stopwords <- tibble(word = c("january", "february", "92d", ("1":"2020"), lexicon = "sotu"))

my.stop.words <- stop_words %>%
bind_rows(custom.stopwords)

#remove words within brackets
sotu <- readtext("*.txt") %>%
  mutate(text = str_replace_all(text, pattern = "\\[.*?\\]", replacement =  "")) %>%
  mutate(text = str_replace_all(text, pattern = "\\(.*?\\)", replacement =  ""))


sotu.D <- readtext("*Democrat.txt") %>%
  mutate(text = str_replace_all(text, pattern = "\\[.*?\\]", replacement =  "")) %>%
  mutate(text = str_replace_all(text, pattern = "\\(.*?\\)", replacement =  ""))
  
sotu.R <- readtext("*Republican.txt") %>%
  mutate(text = str_replace_all(text, pattern = "\\[.*?\\]", replacement =  "")) %>%
  mutate(text = str_replace_all(text, pattern = "\\(.*?\\)", replacement =  ""))

#apply tidy principles
tidy.sotu <- sotu %>%
  unnest_tokens(word, text) %>%
  count(doc_id, word, sort = T) %>%
  mutate(party = ifelse(grepl("Democrat", doc_id), "Democrat", "Republican")) %>%
  ungroup()

total.words <- tidy.sotu %>%
group_by(doc_id) %>%
summarize(total = sum(n))

tidy.sotu <- left_join(tidy.sotu, total.words)

#tf_idf
tf.idf <- tidy.sotu %>%
bind_tf_idf(word, doc_id, n) %>%
arrange(desc(tf_idf)) %>%
anti_join(my.stop.words)




tf.idf.plot <- tf.idf %>%
mutate(word = factor(word, levels = rev(unique(word)))) %>%
group_by(party) %>%
top_n(20, tf_idf) %>%
ungroup %>%
ggplot(aes(word, tf_idf)) +
geom_col(aes(fill = party),show.legend = T) +
labs(x = NULL, y = "TF-IDF") +
coord_flip() +
theme_hc() +
scale_fill_manual(values = c(Republican = "#FF0000",  Democrat = "#0009FF")) +
facet_wrap(~ party, ncol = 2, scales = "free")


#DEMOCRATS
tidy.sotu.D <- sotu.D %>%
  unnest_tokens(word, text) %>%
  count(doc_id, word, sort = T) %>%
  mutate(party = ifelse(grepl("Democrat", doc_id), "Democrat", "Republican")) %>%
  ungroup()

total.words.D <- tidy.sotu.D %>%
  group_by(doc_id) %>%
  summarize(total = sum(n))

tidy.sotu.D <- left_join(tidy.sotu.D, total.words.D)

tf.idf.D <- tidy.sotu.D %>%
  bind_tf_idf(word, doc_id, n) %>%
  arrange(desc(tf_idf)) %>%
  anti_join(my.stop.words)

tf.idf.D.plot <- tf.idf.D %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(party) %>%
  top_n(20, tf_idf) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf)) +
  geom_col(aes(fill = party),show.legend = F) +
  labs(x = NULL, y = "TF-IDF") +
  coord_flip() +
  theme_hc() +
  scale_fill_manual(values = c(Democrat = "#0009FF"))


#Republicans
tidy.sotu.R <- sotu.R %>%
  unnest_tokens(word, text) %>%
  count(doc_id, word, sort = T) %>%
  mutate(party = ifelse(grepl("Democrat", doc_id), "Democrat", "Republican")) %>%
  ungroup()

total.words.R <- tidy.sotu.R %>%
  group_by(doc_id) %>%
  summarize(total = sum(n))

tidy.sotu.R <- left_join(tidy.sotu.R, total.words.D)

tf.idf.R <- tidy.sotu.R %>%
  bind_tf_idf(word, doc_id, n) %>%
  arrange(desc(tf_idf)) %>%
  anti_join(my.stop.words)

tf.idf.R.plot <- tf.idf.R %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(party) %>%
  top_n(20, tf_idf) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf)) +
  geom_col(aes(fill = party),show.legend = F) +
  labs(x = NULL, y = "TF-IDF") +
  coord_flip() +
  theme_hc() +
  scale_fill_manual(values = c(Republican = "#FF0000"))


