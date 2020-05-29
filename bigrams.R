## Bigrams

library(tidytext)
library(dplyr)

## bigrams
## for the bigram analysis I decided to remove the swear words completely
bad_words <- data_frame(word = c("ass", "asses", "asshole", "assholes","fuck", "fucker", "fuckers", "fucking", "fuckin", "fucked",
                                 "motherfucker", "motherfuckers", "butt", "bitch", "cock", "cunt",
                                 "dick", "nigger", "shit", "tits"))

extra_stop_words <- data_frame(word = c("yeah","â","gonna","wanna",
                                        "hey","ya","gotta","uh","ah",
                                        "muh","shhh","sh","wh","er","ha","em", "du")
)

## names
names <- read_csv("./data/characters_clean.csv") %>% select(word=speaker) %>% mutate(word=tolower(word))
names_hadley <- read_csv("./data/baby-names.csv") %>% select(word=name) %>% mutate(word=tolower(word))

## plus I know of a couple of names on the screen that will not show up in either e.g. "Dufresne"
## so let's fix that
hidden_names <- data_frame(word = c("mcmurphy", "gump", "dom","dufresne", "mills", "somerset", "starling", "lecter", "ratched", "pilbow",
                                    "corleone", "hannibal"))

all_names <- rbind(names, names_hadley, as_tibble(hidden_names))



## bigrams - female --------------------------------------------------------------------------------------

df_bigrams_female <- read_csv("./data/df.csv") %>% 
  filter(gender == "female") %>%
  unnest_tokens(bigram, script, token = "ngrams", n = 2)

bigrams_separated_fm <- df_bigrams_female %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered_fm <- bigrams_separated_fm %>%
  filter(!word1 %in% c(stop_words$word, extra_stop_words$word, bad_words$word, NA)) %>%
  filter(!word2 %in% c(stop_words$word, extra_stop_words$word, bad_words$word, NA))

# new bigram counts:
bigram_counts_fm <- bigrams_filtered_fm %>% 
  count(word1, word2, sort = TRUE)

female_bigrams_plot <- bigram_counts_fm %>% 
  mutate(bigrams = paste0(word1, " ", word2)) %>% 
  mutate(ranking = row_number()) %>% 
  filter(ranking <=10) %>% 
  ggplot(aes(reorder(bigrams, n), n)) +
  geom_col(fill = "#F8766D") +
  coord_flip() +
  labs(title = "Bigrams", subtitle = "Names included", x = "", y = "") +
  theme_bw() +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_text(size = 11),
        plot.title = element_text(color = "#615e59", size = 13, hjust = 0),
        plot.subtitle = element_text(color = "#615e59",size = 8, hjust = 0))

ggsave("female_bigrams_plot.PNG", female_bigrams_plot)

## without names

bigrams_filtered_fm2 <- bigrams_separated_fm %>%
  filter(!word1 %in% c(stop_words$word, all_names$word, extra_stop_words$word, bad_words$word, NA)) %>%
  filter(!word2 %in% c(stop_words$word, all_names$word, extra_stop_words$word, bad_words$word, NA))

# new bigram counts:
bigram_counts_fm2 <- bigrams_filtered_fm2 %>% 
  count(word1, word2, sort = TRUE)

female_bigrams_plot2 <- bigram_counts_fm2 %>% 
  mutate(bigrams = paste0(word1, " ", word2)) %>% 
  mutate(ranking = row_number()) %>% 
  filter(ranking <=10) %>% 
  ggplot(aes(reorder(bigrams, n), n)) +
  geom_col(fill = "#F8766D") +
  coord_flip() +
  labs(title = "Bigrams", subtitle = "Names excluded", x = "", y = "") +
  theme_bw() +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_text(size = 11),
        plot.title = element_text(color = "#615e59", size = 13, hjust = 0),
        plot.subtitle = element_text(color = "#615e59",size = 8, hjust = 0))

ggsave("female_bigrams_plot2.PNG", female_bigrams_plot2)

## bigrams - male ----------------------------------------------------------------------------------------

df_bigrams_male <- read_csv("./data/df.csv") %>% 
  filter(gender == "male") %>%
  unnest_tokens(bigram, script, token = "ngrams", n = 2)

bigrams_separated_m <- df_bigrams_male %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered_m <- bigrams_separated_m %>%
  filter(!word1 %in% c(stop_words$word, extra_stop_words$word, bad_words$word, NA)) %>%
  filter(!word2 %in% c(stop_words$word, extra_stop_words$word, bad_words$word, NA))


# new bigram counts:
bigram_counts_m <- bigrams_filtered_m %>% 
  count(word1, word2, sort = TRUE)

male_bigrams_plot <- bigram_counts_m %>% 
  mutate(bigrams = paste0(word1, " ", word2)) %>% 
  mutate(ranking = row_number()) %>% 
  filter(ranking <=10) %>% 
  ggplot(aes(reorder(bigrams, n), n)) +
  geom_col(fill = "#00BFC4") +
  coord_flip() +
  labs(title = "Bigrams", subtitle = "Names included", x = "", y = "") +
  theme_bw() +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_text(size = 11),
        plot.title = element_text(color = "#615e59", size = 13, hjust = 0),
        plot.subtitle = element_text(color = "#615e59",size = 8, hjust = 0))

ggsave("male_bigrams_plot.PNG", male_bigrams_plot)


## without names

bigrams_filtered_m2 <- bigrams_separated_m %>%
  filter(!word1 %in% c(stop_words$word, all_names$word, extra_stop_words$word, bad_words$word, NA)) %>%
  filter(!word2 %in% c(stop_words$word, all_names$word, extra_stop_words$word, bad_words$word, NA))

# new bigram counts:
bigram_counts_m2 <- bigrams_filtered_m2 %>% 
  count(word1, word2, sort = TRUE)

male_bigrams_plot2 <- bigram_counts_m2 %>% 
  mutate(bigrams = paste0(word1, " ", word2)) %>% 
  mutate(ranking = row_number()) %>% 
  filter(ranking <=10) %>% 
  ggplot(aes(reorder(bigrams, n), n)) +
  geom_col(fill = "#00BFC4") +
  coord_flip() +
  labs(title = "Bigrams", subtitle = "Names excluded", x = "", y = "") +
  theme_bw() +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_text(size = 11),
        plot.title = element_text(color = "#615e59", size = 13, hjust = 0),
        plot.subtitle = element_text(color = "#615e59",size = 8, hjust = 0))

ggsave("male_bigrams_plot2.PNG", male_bigrams_plot2)

