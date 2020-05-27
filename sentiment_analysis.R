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
  inner_join(sentiments_nrc) 

table(sentiment_df$gender)
## compared to the original proportion women managed to get a bit more representation here

## For the first part I will not need the actual words themselves so let's group 
## the data and count sentiments
sentiment_plot <- sentiment_df %>% group_by(gender) %>% 
  count(sentiment, sort = TRUE) %>% 
  ggplot(aes(reorder(sentiment, n, sum), n, fill = gender))+
  geom_bar(position = "dodge", stat = "identity") +
  coord_flip() +
  labs(title = "Word Sentiments",subtitle = "per the NRC Lexicon", x = "", y="") +
  theme_bw() +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_text(size = 11),
        legend.title = element_blank(),
        legend.text = element_text(size = 11),
        legend.position = "bottom",
        plot.title = element_text(color = "#615e59", size = 16, hjust = 0.0475),
        plot.subtitle = element_text(color = "#615e59",size = 8, hjust = 0.047))

ggsave("sentiment_plot.PNG", sentiment_plot)

## despite the huge difference in numbers, female and male charaters seem to go hand in hadn in terms of sentiment of their words
## except for one area - for female characters anticipation seems to outdo anger and sadness with a very tiny difference
## the overall moral of the story is that most characters tend to use less surprised words in our population

## let's dig in and see the most common negative words
