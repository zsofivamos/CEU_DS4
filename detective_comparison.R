### MILLS VS STARLING (in alphabetical order)

## so the overall impression so far is that the data I selected is not exactly suitable
## for comparing female and male conversation. however, I've come up with a plan.
## I am going to compare the speech of two leading characters subsetting my data to
## focus on Seven vs Silence of the Lambs
## I decided to go ahead with this because both movies are centered around a young police officer
## trying to hunt down a serial killer
## both are having sidekicks (all male of course) and both are communicating with
## the aforementioned killer in some way
## what I'm trying to get out of this is an actual eye-opening result showing
## differences and similarities in representations of female and male detectives

library(tidytext)
library(glmnet)

df <- read_csv("./data/df.csv")

## let's see if we have the two characters

df <- df %>% 
  filter(title %in% c("Seven", "Silence of the Lambs")) %>% 
  filter(speaker %in% c("MILLS", "CLARICE")) %>% 
  unnest_tokens(word, script) %>% 
  anti_join(stop_words) %>% 
  filter(!str_detect(word, "\\d+"))

str(df)
## so now we only have 1739 observations, let's see their distribution

df %>% 
  group_by(speaker) %>% 
  summarise(word_count = n())

## Mills and Clarice have about the same amount of words in the data
## names, Clarice taking the lead having 39 words more
## stop words have been removed but may need to revisit to remove swear words too

## so let's do this - what are their top words?
View(df %>% 
  count(word, sort = TRUE))

## it seems that there are a couple of swear words listed here
## as I think they can be an important part of the analysis on the individual level, 
## I will mutate them and create a generic word 'cursing'
## this way a character will be known to curse but the plot can stay clean

bad_words <- c("cock", "cunt", "fuck", "fucking", "shit", "bitch", "fucked", "motherfucker", "fucker", "fuckhead")

df$word <- ifelse(df$word %in% bad_words, "CURSE", df$word)

## Clarice
starling <- df %>% filter(speaker == "CLARICE") %>% 
  count(word, sort = TRUE) %>% 
  top_n(20) %>% 
  ggplot(aes(reorder(word, n), n)) +
  geom_col(fill = "#F8766D" )+
  labs(title = "Clarice Starling", x = "", y = "") +
  coord_flip() +
  theme_bw() +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_text(size = 11),
        plot.title = element_text(colour = "#615e59", size = 16, hjust = 0),
        plot.subtitle = element_text(colour = "#615e59", size = 8, hjust = 0))

ggsave("starling.PNG", starling)

## Clarice seems to be talking about Dr Lecter the most - this is not really surprising as he is the villain sidekick/murder consultant
## interestingly she addresses both her male counterparts and female characters, while also
## seems to 'yow' a lot - which we know is not a nice thing
## Clarice is not that concerned with swearing

## MILLS
mills <- df %>% filter(speaker == "MILLS") %>% 
  count(word, sort = TRUE) %>% 
  top_n(20) %>% 
  ggplot(aes(reorder(word, n), n)) +
  geom_col(fill = "#00BFC4")+
  labs(title = "David Mills", x = "", y = "") +
  coord_flip() +
  theme_bw() +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_text(size = 11),
        plot.title = element_text(colour = "#615e59", size = 16, hjust = 0),
        plot.subtitle = element_text(colour = "#615e59", size = 8, hjust = 0))

ggsave("mills.PNG", mills)


## The single most frequent word in the MILLS pool is CURSE, and while this can mean a number of bad_words which
## may not be used that frequently on an individual level, it does appear that MILLS is a very frustrated young man
## he seems to mention a couple of names but none of the words are an obvious reference to female characters



## as I suspected, curse words did have a role in the conversation 
## it seems that Mr Mills cursed a lot more, although Clarice wasn't innocent herself

## let's remove names
names <- read_csv("./data/characters_clean.csv") %>% 
  filter(title %in% c("Seven", "Silence of the Lambs")) %>% 
  select(word=speaker) %>% mutate(word=tolower(word))

names_hadley <- read_csv("./data/baby-names.csv") %>% 
  select(word=name) %>% mutate(word=tolower(word))

hidden_names <- data_frame(word = c("lecter", "mills", "somerset", "starling", "lecter", "ratched", "pilbow","hannibal", "dr", "doctor"))

all_names <- rbind(names, names_hadley, as_tibble(hidden_names))

df %>% 
  group_by(speaker) %>% 
  anti_join(all_names) %>% 
  count(word, sort = TRUE) %>% 
  top_n(10) %>%
  ungroup() %>% 
  ggplot(aes(x = reorder(word, n, sum),
             y = ifelse(test = speaker == "MILLS", yes = -n, no = n), 
             fill = speaker)) +
  geom_col() +
  coord_flip() +
  scale_x_reordered() +
  scale_y_continuous(labels = abs) +
  labs(title = "Word split of Clarice and Mills", subtitle = "Ranked by total word count", x = "", y = "Count") +
  theme_bw()
  

## TF_IDF

df_tf_idf <- df %>%
  anti_join(all_names) %>% 
  count(speaker, word) %>%
  bind_tf_idf(word, speaker, n) %>%
  arrange(-tf_idf)

df_tf_idf %>%    
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(speaker) %>% 
  top_n(10) %>% 
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = speaker)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Most important words of Clarice and Mills", x= "", y = "TF-IDF Score") +
  facet_wrap(~speaker, ncol = 2, scales = "free") +
  coord_flip() +
  theme_bw()

### GLM

words <- df %>%
  anti_join(all_names) %>% 
  group_by(speaker, line_number, word) %>%
  summarise(contains = 1) %>%
  ungroup() %>%
  spread(key = word, value = contains, fill = 0) %>%
  mutate(word_by_clarice = as.integer(speaker == "CLARICE")) %>%
  select(-speaker, -line_number)

fit <- cv.glmnet(
  x = words %>% select(-word_by_clarice) %>% as.matrix(),
  y = words$word_by_clarice,
  family = "binomial"
)

temp <- coef(fit, s = exp(-3.5)) %>% as.matrix()
coefficients <- data.frame(word = row.names(temp), beta = temp[, 1])
data <- coefficients %>%
  filter(beta != 0) %>%
  filter(word != "(Intercept)") %>%
  arrange(desc(beta)) %>%
  mutate(i = row_number())

coeff_plot <- ggplot(data, aes(x = i, y = beta, fill = ifelse(beta > 0, "CLARICE", "MILLS"))) +
  geom_bar(stat = "identity", alpha = 0.75) +
  scale_x_continuous(breaks = data$i, labels = data$word, minor_breaks = NULL) +
  xlab("") +
  ylab("Coefficient Estimate") +
  coord_flip() +
  scale_fill_manual(
    guide = guide_legend(title = "Word typically used by:"),
    values = c("#F8766D", "#00BFC4")
  ) +
  theme_bw() +
  theme(legend.position = "top")

ggsave("coeff_plot.PNG", coeff_plot)

## based on this plot there really isn't a way to say that Clarice uses different words compared to Mills
## as both of their likelier words are strongly connected to their movies' plots.

## what is definitely strikingly obvious is that the male cop swears a lot more than the female cop

sentiments_nrc <- get_sentiments("nrc")

sentiments_df <- df %>% 
  group_by(speaker) %>% 
  count(word, sort = TRUE) %>% 
  inner_join(sentiments_nrc)

## hiding axis labels as the number of overall words is similar
sentiment2 <- sentiments_df %>% group_by(speaker) %>% 
  count(sentiment, sort = TRUE) %>% 
  ungroup() %>% 
  mutate(speaker = as.factor(speaker), 
         sentiment = reorder_within(sentiment, n, speaker)) %>% 
  ggplot(aes(sentiment, n, fill = speaker))+
  geom_col(show.legend = FALSE) +
  facet_wrap(~speaker, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(title = "Word Sentiments",subtitle = "per the NRC Lexicon", x = "", y="") +
  theme_minimal() +
  theme(axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 11),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.title = element_text(color = "#615e59", size = 16, hjust = 0),
        plot.subtitle = element_text(color = "#615e59",size = 8, hjust = 0))

ggsave("sentiment2.PNG", sentiment2)
