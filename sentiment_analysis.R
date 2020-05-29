library(tidytext)
library(tidyverse)

sentiments_nrc <- get_sentiments("nrc")

## For the sake of consisency - and because I'm fairly sure that the NRC lexicon doesn't include swear words
## - I'll keep working on the clean dataset I've created previously.
## I'll ignore the unclear genders and will focus on the filtered dataset, excluding names

df <- read_csv("./data/df.csv") %>% 
  filter(speaker != "NO SPEAKER") %>% 
  filter(gender != "unclear") %>% 
  unnest_tokens(word, script)

## there seem to be a lot of NA values among the sentiments so I'm gonna have to filter those out
## with an inner_join. let's see what we're left with in terms of gender representation
sentiment_df <- df %>% 
  group_by(gender) %>% 
  count(word, sort = TRUE) %>% 
  inner_join(sentiments_nrc) %>% 
  ungroup()

table(sentiment_df$gender)
## compared to the original proportion women managed to get a bit more representation here

## For the first part I will not need the actual words themselves so let's group 
## the data and count sentiments
sentiment_plot <- sentiment_df %>% 
  group_by(gender) %>% 
  count(sentiment, sort = TRUE) %>% 
  mutate(total_words = sum(n),
         frequency = n/total_words) %>%
  ggplot(aes(reorder(sentiment, frequency), frequency, fill = gender))+
  geom_bar(position = "dodge", stat = "identity") +
  coord_flip() +
  labs(title = "Frequency of sentiments",subtitle = "per the NRC Lexicon", x = "", y="Frequency") +
  theme_bw() +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_text(color = "#615e59",size = 10),
        legend.title = element_blank(),
        axis.title.y = element_text(size = "9", color ="#615e59"),
        legend.text = element_text(size = 11),
        legend.position = "bottom",
        plot.title = element_text(color = "#615e59", size = 16, hjust = 0),
        plot.subtitle = element_text(color = "#615e59",size = 8, hjust = 0))

ggsave("sentiment_plot.PNG", sentiment_plot)

## despite the huge difference in numbers, female and male charaters seem to go hand in hadn in terms of sentiment of their words
## except for one area - for female characters anticipation seems to outdo anger and sadness with a very tiny difference
## the overall moral of the story is that most characters tend to use less surprised words in our population

## FEMALE
female_positives <- sentiment_df %>%
  filter(gender == "female") %>% 
  filter(sentiment == "positive") %>% 
  mutate(ranking = row_number()) %>% 
  filter(ranking <= 10)
  

positive_fm <- ggplot(female_positives, aes(reorder(word,n), n)) + 
  geom_col(fill = "#F8766D") +
  coord_flip() +
  labs(title = "Positive words", x = "", y = "") +
  theme_bw() +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_text(size = 11),
        axis.text.x = element_text(colour = "#615e59"),
        plot.title = element_text(colour = "#615e59", size = 16, hjust = 0),
        plot.subtitle = element_text(colour = "#615e59", size = 8, hjust = 0))

ggsave("positive_fm.PNG", positive_fm)

## MALE

male_positives <- sentiment_df %>%
  filter(gender == "male") %>% 
  filter(sentiment == "positive") %>% 
  mutate(ranking = row_number()) %>% 
  filter(ranking <= 10)


positive_m <- ggplot(male_positives, aes(reorder(word,n), n)) + 
  geom_col(fill = "#00BFC4") +
  coord_flip() +
  labs(title = "Positive words", x = "", y = "") +
  theme_bw() +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_text(size = 11),
        axis.text.x = element_text(colour = "#615e59"),
        plot.title = element_text(colour = "#615e59", size = 16, hjust = 0),
        plot.subtitle = element_text(colour = "#615e59", size = 8, hjust = 0))

ggsave("positive_m.PNG", positive_m)