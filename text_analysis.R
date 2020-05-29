library(dplyr)
library(ggplot2)
library(tidytext)
library(stringr)
library(tidyverse)
library(scales)

### EDA

df <- read_csv("./data/df.csv")
## let's get rid of the irrelevant lines and unnest the test into tokens

table(df$gender)
## the number of male characters seems to be overwhelming compared to female ones, let's 
## check if there's any change in terms of words before removing stop words

df_tidy <- df %>% filter(speaker != "NO SPEAKER") %>% 
  unnest_tokens(word, script) %>% 
  anti_join(stop_words)

table(df_tidy$gender)
## whew, that was close, thank god that most of the female speech was just stop words
## so it seems that women only account for about 14 % of the whole conversation here. 

str(df_tidy)

## check most popular words overall
df_tidy %>% count(word, sort = TRUE) %>% 
  filter(n > 60) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) + 
  geom_col() + 
  labs(title = "Most common words in our population", x = "", y = "Count") +
  coord_flip() +
  theme_bw() +
  theme(axis.title.y = element_text(colour = "#615e59", size = 14))

View(df_tidy %>% count(word, sort = TRUE))

## there seem to be a couple of bad words.. let's remove them
## disclaimer: I am 100% sure that I won't be able to remove all as there are more than 8k words

#### -------------------------------------------------------------------------------------------

## I think swearing should be an important aspect of human speech so I'm going to convert the bad words
## into something else - this way I can show them without showing them. 

bad_words <- data_frame(word = c("ass", "asses", "asshole", "assholes","fuck", "fucker", "fuckers", "fucking", "fuckin", "fucked",
                          "motherfucker", "motherfuckers", "butt", "bitch", "cock", "cunt",
                          "dick", "nigger", "shit", "tits"))

coverup_words <- c("blip", "bloop", "woopsie", "shikaka", "blippity", "blooppity", "dagnabbit", "fiddlesticks",
                   "snickerdoodle", "bambooya", "jiminycrickets", "shenanigans", "duckwater", "firegoat",
                   "digikiki", "turkeypunch", "macarena", "waterchicken", "dragonhat", "oopsidoodle", "antshoes")


df_tidy$word <- case_when(
  df_tidy$word %in% c("ass", "asses", "asshole", "assholes") ~ "BLIPPITY",
  df_tidy$word %in% c("fuck", "fucker", "fuckers", "fucking", "fuckin", "fucked") ~ "BLOOPITY",
  df_tidy$word %in% c("motherfucker", "motherfuckers") ~ "SNICKERDOODLE",
  df_tidy$word == "butt" ~ "WATERCHICKEN",
  df_tidy$word == "bitch" ~ "FIDDLESTICKS",
  df_tidy$word == "cock" ~ "WHOOPSIE",
  df_tidy$word == "cunt" ~ "FIREGOAT",
  df_tidy$word == "dick" ~ "BLOOP",
  df_tidy$word == "nigger" ~ "SHENANIGANS",
  df_tidy$word == "shit" ~ "SHIKAKA",
  df_tidy$word == "tits" ~ "OOPSIDOODLE",
  TRUE ~ df_tidy$word
)

## I've also noticed a couple of what I'd consider as stop words, let's add those too

extra_stop_words <- data_frame(word = c("yeah","â","gonna","wanna",
                                           "hey","ya","gotta","uh","ah",
                                           "muh","shhh","sh","wh","er","ha","em", "du")
)

df_tidy <- df_tidy %>% 
  anti_join(extra_stop_words)

#### -------------------------------------------------------------------------------------------

## check most popular words @clean edition
(common_words_plot <- df_tidy %>% count(word, sort = TRUE) %>% 
  filter(n > 60) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) + 
  geom_col() + 
  labs(title = "Most common words in the data set", x = "", y = "") +
  coord_flip() +
  theme_bw() +
  theme(axis.title.y = element_text(colour = "#615e59", size = 14)) +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_text(size = 11),
        plot.title = element_text(colour = "#615e59", size = 16, hjust = 0.0475),
        plot.subtitle = element_text(colour = "#615e59", size = 8, hjust = 0.047)))


ggsave("common_words_plot.PNG", common_words_plot)

#### -------------------------------------------------------------------------------------------

## let's start drilling down into genders - how are they represented on this bar chart?
## I'm filtering for the top 20 words again but not adding a gender variable in 

(common_words_colored <- df_tidy %>% group_by(gender) %>% 
  count(word, sort = TRUE) %>% 
  ungroup() %>% 
  filter(n > 60) %>%
  ggplot(aes(reorder(word, n), n, fill = gender)) +
  geom_col() +
  labs(title = "Most common words in the data set", x = "", y = "") +
  coord_flip() +
  theme_bw() +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_text(size = 11),
        legend.title = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(colour = "#615e59", size = 16, hjust = 0),
        plot.subtitle = element_text(colour = "#615e59", size = 8, hjust = 0)))


ggsave("common_words_colored_plot.PNG", common_words_colored)

#### -------------------------------------------------------------------------------------------

## what's the situation with the characters we couldn't classify?
## are we losing some discourse there?
df_tidy %>% filter(gender == "unclear") %>% 
  count(word, sort = TRUE) %>% 
  mutate(ranking = row_number()) %>%
  filter(ranking <= 20) %>% 
  summarise(sum(n))


## 102 words, doesn't seem that many
df_tidy %>% filter(gender == "unclear") %>% 
  count(word, sort = TRUE) %>% 
  mutate(ranking = row_number()) %>% 
  filter(ranking <= 20) %>% 
  ggplot(aes(reorder(word, n), n)) +
  geom_col(fill = "#7CAE00")+
  labs(title = "Most common words of unclear genders", x = "", y = "Count") +
  coord_flip() +
  theme_bw()

## a lot seems to be about addressing male characters again - sir, morpheus, johnny, forrest, father, 
## with some reference to females as well - girl, kay
## and some fishing and religion
## since the overall number of words spoken by unclear characters is low, let's not consider them
## going forward

#### NAMES ---------------------------------------------------------

## I'll combine Hadley's name file with my own character file
names <- read_csv("./data/characters_clean.csv") %>% select(word=speaker) %>% mutate(word=tolower(word))
names_hadley <- read_csv("./data/baby-names.csv") %>% select(word=name) %>% mutate(word=tolower(word))

## plus I know of a couple of names on the screen that will not show up in either e.g. "Dufresne"
## so let's fix that
hidden_names <- data_frame(word = c("mcmurphy", "gump", "dom","dufresne", "mills", "somerset", "starling", "lecter", "ratched", "pilbow",
                                    "corleone", "hannibal"))

all_names <- rbind(names, names_hadley, as_tibble(hidden_names))

#### -------------------------------------------------------------------------------------------

### FEMALE SPEECH

## let's see the individual rankings
df_tidy %>% filter(gender == "female") %>% 
  count(word, sort = TRUE) %>% 
  mutate(total_words = sum(n),
         frequency = n/total_words) %>% 
  mutate(ranking = row_number()) %>%
  filter(ranking <= 20) %>% 
  summarise(sum(n))

## the 20 most common female words account for 630 appearances within the total of 4893 words
## signalling that a lot of different words are used once or twice, and a very small proportion of them get
## repeated

## the 20 most common female words contribute for 630 appearances in the data altogether

(female_words1 <- df_tidy %>% filter(gender == "female") %>% 
    count(word, sort = TRUE) %>% 
    mutate(total_words = sum(n),
           frequency = n/total_words) %>% 
    mutate(ranking = row_number()) %>%
    filter(ranking <= 20) %>% 
    ggplot(aes(reorder(word, frequency), frequency)) +
    geom_col(fill = "#F8766D" )+
    labs(title = "Female words", subtitle = "Names included", x = "", y = "Frequency") +
    coord_flip() +
    theme_bw() +
    theme(axis.ticks.y = element_blank(),
          axis.text.y = element_text(size = 11),
          axis.text.x = element_text(colour = "#615e59"),
          plot.title = element_text(colour = "#615e59", size = 16, hjust = 0),
          plot.subtitle = element_text(colour = "#615e59", size = 8, hjust = 0)))

ggsave("female_words1.PNG", female_words1)

### Remove names

## let's see the individual rankings
df_tidy %>% filter(gender == "female") %>% 
  anti_join(all_names) %>% 
  count(word, sort = TRUE) %>% 
  mutate(total_words = sum(n),
         frequency = n/total_words) %>% 
  mutate(ranking = row_number()) %>%
  filter(ranking <= 20) %>% 
  summarise(sum(n))


(female_words2 <- df_tidy %>% filter(gender == "female") %>% 
    anti_join(all_names) %>% 
    count(word, sort = TRUE) %>% 
    mutate(total_words = sum(n),
           frequency = n/total_words) %>% 
    mutate(ranking = row_number()) %>%
    filter(ranking <= 20) %>% 
    ggplot(aes(reorder(word, frequency), frequency)) +
    geom_col(fill = "#F8766D" )+
    labs(title = "Female words", subtitle = "Names excluded", x = "", y = "Frequency") +
    coord_flip() +
    theme_bw() +
    theme(axis.ticks.y = element_blank(),
          axis.text.y = element_text(size = 11),
          axis.text.x = element_text(colour = "#615e59"),
          plot.title = element_text(colour = "#615e59", size = 16, hjust = 0),
          plot.subtitle = element_text(colour = "#615e59", size = 8, hjust = 0)))

ggsave("female_words2.PNG", female_words2)

#### -------------------------------------------------------------------------------------------

### MALE SPEECH

df_tidy %>% filter(gender == "male") %>% 
  count(word, sort = TRUE) %>% 
  mutate(total_words = sum(n),
         frequency = n/total_words) %>% 
  mutate(ranking = row_number()) %>%
  filter(ranking <= 20) %>% 
  summarise(sum(n))

## for male characters, the variety seems to be a lot larger
## the total number of words is 29257 and the most frequent one accounts for only about
## .8 per cent of the total wordcount. And it's a curse word. 

## male characters have a head start with almost 3 times as many word appearances as women
## with a score of 2085

(male_words1 <- df_tidy %>% filter(gender == "male") %>% 
  count(word, sort = TRUE) %>% 
  mutate(total_words = sum(n),
         frequency = n/total_words) %>%
  mutate(ranking = row_number()) %>% 
  filter(ranking <= 20) %>% 
  ggplot(aes(reorder(word, frequency), frequency)) +
  geom_col(fill = "#00BFC4")+
  labs(title = "Male words", subtitle = "Names included",x = "", y = "") +
  coord_flip() +
  theme_bw() +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_text(size = 11),
        axis.text.x = element_text(colour = "#615e59"),
        plot.title = element_text(colour = "#615e59", size = 16, hjust = 0),
        plot.subtitle = element_text(colour = "#615e59", size = 8, hjust = 0)))

ggsave("male_words1.PNG", male_words1)

### without names
df_tidy %>% filter(gender == "male") %>% 
  anti_join(all_names) %>% 
  count(word, sort = TRUE) %>% 
  mutate(total_words = sum(n),
         frequency = n/total_words) %>% 
  mutate(ranking = row_number()) %>%
  filter(ranking <= 20) %>% 
  summarise(sum(n))

## not a single word got removed from the male pool - they are still going strong with 1806
(male_words2 <- df_tidy %>% filter(gender == "male") %>% 
    anti_join(all_names) %>% 
    count(word, sort = TRUE) %>% 
    mutate(total_words = sum(n),
           frequency = n/total_words) %>%
    mutate(ranking = row_number()) %>% 
    filter(ranking <= 20) %>% 
    ggplot(aes(reorder(word, frequency), frequency)) +
    geom_col(fill = "#00BFC4")+
    labs(title = "Male words", subtitle = "Names excluded",x = "", y = "") +
    coord_flip() +
    theme_bw() +
    theme(axis.ticks.y = element_blank(),
          axis.text.y = element_text(size = 11),
          axis.text.x = element_text(colour = "#615e59"),
          plot.title = element_text(colour = "#615e59", size = 16, hjust = 0),
          plot.subtitle = element_text(colour = "#615e59", size = 8, hjust = 0)))

ggsave("male_words2.PNG", male_words2)

#### -------------------------------------------------------------------------------------------

df_mf <- df_tidy %>% filter(gender != "unclear")

## let's check the 30 most popular words up against each other
(word_split_plot <- df_mf %>% 
  anti_join(all_names) %>% 
  group_by(gender) %>% 
  count(word, sort = TRUE) %>% 
  top_n(20) %>%
  ungroup() %>% 
# arrange(-n) %>% 
  ggplot(aes(x = reorder(word, n, sum),
             y = ifelse(test = gender == "male", yes = -n, no = n), 
             fill = gender)) +
  geom_col() +
  coord_flip() +
  scale_x_reordered() +
  scale_y_continuous(labels = abs) +
  labs(title = "Top words", subtitle = "Ranked by total word count", x = "", y = "") +
  theme_bw() +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_text(size = 10),
        legend.title = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(colour = "#615e59", size = 16, hjust = 0),
        plot.subtitle = element_text(colour = "#615e59", size = 8, hjust = 0)))

ggsave("word_split_plot.PNG", word_split_plot)

## time is in overwhelming majority again, and women actually do seem to be present for the biggest
## buzzwords in the sample - but the fact that the lowest male word score is only about 40% behind the highest
## female word score is incredibly strange. 

## if something doesnt appear it only means that it's not in the top 20 words not that it's not used by that gender

## What are the popular words in these movies?

df_mf %>% 
  anti_join(all_names) %>% 
  group_by(title) %>% 
  count(word, sort = TRUE) %>% 
  top_n(15) %>% 
  ungroup() %>% 
  mutate(title = as.factor(title),
         word = reorder_within(word, n, title)) %>% 
  ggplot(aes(word, n, fill = title)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~title, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  # scale_y_continuous(expand = c(0,0)) +
  labs(title = "Movies", subtitle = "10 most common words per gender", 
       x = "", y = "") +
  theme_bw()

## comparing the sampled movies on the same scale the first thing that stands out is that the word 'mister'
## occupies a very noble place within the discource in Cuckoo's Nest. Shockingly, fight and club appear to be the most common
## words in Fight Club, so apparently they aren't doing great in terms of keeping the three simple rules over there. 
## Forrest Gump is a movie about mothers and boats. Time is a recurring theme along with people and talking. 

facet_df <- df_mf %>% 
  anti_join(all_names) %>% 
  group_by(title, gender) %>% 
  count(word, sort = TRUE) %>% 
  top_n(5) %>%
  ungroup() %>% 
  arrange(title, -n)

movie_split_colored <- facet_df %>% 
  mutate(title = as.factor(title),
         word = reorder_within(word, n, title)) %>%
  ggplot(aes(x = reorder(word, n, sum), y = ifelse(test = gender == "male", yes = -n, no = n), 
             fill = gender)) +
  geom_col() +
  facet_wrap(~title, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  scale_y_continuous(labels = abs) +
  labs(title = "Top words per movie", subtitle = "Ranked by total word count", x = "", y = "") +
  theme_bw() +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_text(size = 9),
        legend.position = "top",
        legend.title = element_blank(),
        plot.title = element_text(colour = "#615e59", size = 16, hjust = 0),
        plot.subtitle = element_text(colour = "#615e59", size = 8, hjust = 0))


## but due to the enormous difference in number of characters it wouldn't make a lot of sense to derive
## conclusion that males have a less grounded set of topics to talk about - even if it appears they
## have a lot more screen time. 

## let's also check the TF - IDF version of these plots

## TF-IDF

## tf_idf
script_tf_idf <- df_mf %>%
# anti_join(all_names) %>% 
  count(gender, word) %>%
  filter(!str_detect(word, "\\d+")) %>%
  bind_tf_idf(word, gender, n) %>%
  arrange(-tf_idf)

TF_IDF_plot <- script_tf_idf %>%    
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(gender) %>% 
  top_n(10) %>% 
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = gender)) +
  geom_col(show.legend = FALSE) +
  labs(title = "TF-IDF Ranking", subtitle = "Stop words excluded", x= "", y = "") +
  facet_wrap(~gender, ncol = 2, scales = "free") +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  theme_bw() +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_text(size = 9),
        plot.title = element_text(colour = "#615e59", size = 16, hjust = 0),
        plot.subtitle = element_text(colour = "#615e59", size = 8, hjust = 0))


ggsave("tf_idf_plot.PNG", TF_IDF_plot, width = 7)

