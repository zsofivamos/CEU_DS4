library(dplyr)
library(data.table)
library(tidyverse)

### Gender

## I'm going to utilize the Gender API to query the name's gender

## read in the refined version of the scripts
scripts <- fread("./data/scripts_clean.csv")

## extract characters
script_characters <- scripts %>% filter(speaker != "NO SPEAKER") %>% 
  group_by(title, speaker) %>% summarise(line_count = n())

## check who's most popular
script_characters[order(script_characters$title, -script_characters$line_count),]

## let's check on the existing characters list from the script - do these 
## actors have a lot to say?

script_characters %>% filter(line_count<5)

ggplot(script_characters, aes(line_count)) + 
  geom_dotplot(binwidth = 15) +
  facet_wrap(~title) + 
  labs(title = "Distribution of line counts among top IMDB movies", x = "Total # of lines scripted", y = "Portion of characters") +
  theme_bw()

## so that plot is not very meaningful in terms of figuring out the gender split in speech, but it does show that
## in all of our top movie scripts, a huge amount of unimportant characters speak little 
## and only a handful of important ones speak a normal amount 
## the 80-20 rule strikes again ladies and gentlemen

str(script_characters %>% filter(line_count>15))

## it seems that only about 1/3 of the total character population has more than 15 lines
## but obviously this is just some sort of a vantage point, let's dig deeper


## let's fill in the obvious genders
script_characters <- script_characters %>% 
  mutate(gender = ifelse(str_detect(speaker,"\\bWOMAN\\b|\\bGIRL\\b|\\bFEMALE\\b|\\bMOTHER\\b"), 
                         "female", 
                         ifelse(str_detect(speaker, "\\bMAN\\b|\\bBOY\\b|\\bDAD\\b|\\bMALE\\b|\\bFATHER\\b"), 
                                "male",
                                ifelse(str_detect(speaker, "\\bAND\\b|&"), "multiple", NA))))

View(script_characters %>% filter(is.na(gender)))

### Gender API ---------------------------------------------------------------------------------------------------                                             

## time to try the gender api for determining what gender our characters are
## this will obviously need some manual validation

gender_api_key <- Sys.getenv("GENDER_API_KEY")

unique_characters <- script_characters %>% 
  filter(is.na(gender)) %>% 
  group_by(speaker) %>% 
  select(speaker) 

## the function will work best with a vector so let's convert it
unique_characters <- dplyr::pull(unique_characters)

get_gender <- function(character_name) {
  
  results <- content(GET(url = "https://gender-api.com/get?",
                         query = list(key = gender_api_key,
                                      name = character_name)))
  
}

gendered_characters <- rbindlist(lapply(unique_characters, get_gender))

## I'll just save this out to a file because I maxed out the free api limit for the month
fwrite(gendered_characters, "./data/gendered_characters.csv")

### END of API  ---------------------------------------------------------------------------------------------------


### EDA of genders ------------------------------------------------------------------------------------------------

## get the data
gendered_characters <- fread("./data/gendered_characters.csv")

## what to expect in terms of accuracy
ggplot(gendered_characters, aes(accuracy)) + 
  geom_histogram(binwidth = 5, fill = "#bfbfbf") +
  labs(title = "Distribution of determined gender's accuracy", 
       subtitle = "Bin size = 5%", x = "Accuracy %", y = "Count of records") +
  theme_bw()

## so it seems that our API was either pretty sure or admitted defeat - a lot of values ended up as
## 'unknown' with an accuracy of 0. However, the fact that those values are unknown is 100% accurate if you ask me. 

# let's add the column to match on
gendered_characters$speaker <- toupper(gendered_characters$name)

## oh no, it seems that instead of #1 someone put #l! let's fix it
## (I found out by total accident btw)
gendered_characters$speaker <- ifelse(gendered_characters$speaker == "FRIEND #L", "FRIEND #l", gendered_characters$speaker)


## left join the existing script characters to the newly acquired df
merged_characters <- merge(script_characters, gendered_characters, by = "speaker", all.x = TRUE)

merged_characters %>% filter(is.na(gender.y)&is.na(gender.x))

## if the original gender column is NA let's pull the API data in
merged_characters$gender.x <- ifelse(is.na(merged_characters$gender.x), merged_characters$gender.y, merged_characters$gender.x)
View(merged_characters)

## I don't need all that crap
characters <- merged_characters %>% 
  select(title, speaker, line_count, gender = gender.x, accuracy)

## safety check
characters %>% filter(is.na(gender))
characters %>% filter(speaker=="BOYLE")
## what's up with Boyle? let's assign him as male per the script

characters$gender <- ifelse(characters$speaker == "BOYLE", "male", characters$gender)

# now out of vanity let's just change that #l to #1
characters$speaker <- ifelse(characters$speaker == "FRIEND #l", "FRIEND #1", characters$speaker)

ggplot(characters, aes(reorder(gender, gender, function(x)-length(x)))) +
  geom_bar(fill = "#bfbfbf") +
  labs(title = "Gender distribution according to the API", x = "Gender", y = "Count") +
  theme_bw()

## so even if all the unknown genders turn out to be female male characters would still
## have a slight advantage in terms of roles. Now, I know that the API may be wrong 
## so I will look through the list and see if I can find any mistakes that need to be fixed

characters %>% filter(gender == "unknown") %>% 
  group_by(title) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

## seems like Forrest Gump is for the win with Inception having the most 

fwrite(characters, "./data/characters.csv")
