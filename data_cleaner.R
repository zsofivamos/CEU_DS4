library(tidyverse)
library(dplyr)
library(stringr)
library(data.table)
library(zoo)

## read in the file and give column names back
scripts_raw <- fread("./data/movie_scripts.csv", encoding = 'UTF-8')
colnames(scripts_raw) <- c("title", "script", "space_count")

## add row number
scripts_raw <- scripts_raw %>% 
  mutate(line_number = row_number())

## based on the format criteria I found on the internet about 
## screenplays I will be able to separate speech from character and description
## https://screenwriting.io/what-is-standard-screenplay-format/
## unfortunately I will have to customize it per movie

## (V.O) = Voice over
## (O.S) = Off-screen

#### --------------------------------------------------------------------------

## let's check what formats the movies were scraped in - maybe tabs will help me
## determine what is speech and what is not

## so for a couple of movies there are tabs and for a couple of them there are indents
## unfortunately these are not consisent so I'll have to deal with them individually

## I'll separate them and add a new column labelling speech and character - the rows I care about


## The Shawshank Redemption ## -------------------------
shawshank <- scripts_raw %>% filter(title == "The Shawshank Redemption")
## table(shawshank$space_count)
## \t - scene setting/description
## \t\t - speech + the title is \t\t and space, row44-46 are coming from a song on the radio
## those I'll exclude
## \t\t\t - action
## \t\t\t\t - character
## View(shawshank %>% filter(str_detect(script, "^\\t{4}")) %>% group_by(script) %>% summarise(n()))

shawshank <- shawshank %>% mutate(label = ifelse(str_detect(script, "^\\t{4}"), "character", 
                                    ifelse(str_detect(script, "^\\t{2}(?!\\t)(?!\\s)") & !line_number %in% c(44:46), "speech", "other")))

## Godfather ## -------------------------
godfather <- scripts_raw %>% filter(title == "Godfather")
## table(godfather$space_count)
## \t - scene setting/description
## \t\t - speech
## \t\t\t\t - character
## View(godfather %>% filter(str_detect(script, "^\\t{4}(?!\\t)")) %>% group_by(script) %>% summarise(n()))


godfather <- godfather %>% mutate(label = ifelse(str_detect(script, "^\\t{4}(?!\\t)"), "character", 
                                    ifelse(str_detect(script, "^\\t{2}(?!\\t)"), "speech", "other")))


## Pulp Fiction ## -------------------------
pulp_fiction <- scripts_raw %>% filter(title == "Pulp Fiction")
## table(pulp_fiction$space_count)
## 15-16 spaces - scene setting/description
## 25-28 spaces - speech
## 37 spaces - character
## View(pulp_fiction %>% filter(space_count == 37 & str_detect(script, "^[^\\(][^\\s{3,}]")) %>% group_by(script) %>% summarise(n()))


pulp_fiction <- pulp_fiction %>% mutate(label = ifelse(space_count %in% c(25:28), "speech",
                                       ifelse(space_count == 37 & str_detect(script, "^[^\\(][^\\s{3,}]"), "character", "other")))

## Fight Club ## -------------------------
fight_club <- scripts_raw %>% filter(title == "Fight Club")
## table(fight_club$space_count)
## 17> spaces - scene setting/description
## 17-22, 26-28, 30-31 spaces- speech
## 29, 34 - character
## View(fight_club %>% filter(space_count %in% c(29,34)) %>% group_by(script) %>% summarise(n()))

fight_club <- fight_club %>% mutate(label = ifelse(space_count %in% c(29,34) & str_detect(script, "^[^\\(]"), "character",
                                                   ifelse(space_count %in% c(17:22,26:28,30,31), "speech", "other")))

## Forrest Gump ## -------------------------
forrest_gump <- scripts_raw %>% filter(title == "Forrest Gump")
## table(forrest_gump$space_count)
## 15 spaces - scene setting/description
## 25 spaces - speech
## 37 spaces - character
 
forrest_gump <- forrest_gump %>% mutate(label = ifelse(space_count == 25, "speech", 
                                                       ifelse(space_count == 37, "character", "other")))

## Inception ## -------------------------
inception <- scripts_raw %>% filter(title == "Inception")
## table(inception$space_count)
## 1-4 spaces - scene setting/description
## 10-11 spaces - speech
## 12-13, 18-23 spaces - character
## View(inception %>% filter(space_count %in% c(12,13,18:23)) %>% group_by(script) %>% summarise(n()))

inception <- inception %>% mutate(label = ifelse(space_count %in% c(10,11), "speech",
                                                 ifelse(space_count %in% c(12,13,18:23)& str_detect(script, "^[^\\(][^Christopher]"), "character", "other")))

## The Matrix ## -------------------------
the_matrix <- scripts_raw %>% filter(title == "The Matrix")
## table(the_matrix$space_count)
## \t - scene setting/description
## \t\t\t - speech
## \t\t\t\t\t - character + there's one Morpheus character with 4 tabs at line 35282
## View(the_matrix %>% filter(str_detect(script, "^\\t{5}(?=\\b[A-Z]+\\b)")) %>% group_by(script) %>% summarise(n()))

the_matrix <- the_matrix %>% mutate(label = ifelse((str_detect(script, "^\\t{5}(?=\\b[A-Z]+\\b)")) &! str_detect(script, "THE "), "character",
                                                          ifelse(str_detect(script, "^\\t{3}(?!\\t)"), "speech", "other")))

## for now I'll keep lines from the SCREEN as a character, I'll deal with this later
## View(the_matrix %>% filter(str_detect(script, "SCREEN")))

## One Flew Over the Cuckoo's Nest ## -------------------------
cuckoo <- scripts_raw %>% filter(title == "One Flew Over the Cuckoo's Nest")
## table(cuckoo$space_count)
## no tab - scene setting/description
## \t - speech
## \t\t\t - character
## View(cuckoo %>% filter(str_detect(script, "^\\t{3}(?!\\t)")) %>% group_by(script) %>% summarise(n()))

cuckoo <- cuckoo %>% mutate(label = ifelse(str_detect(script, "^\\t{3}(?!\\t)"), "character", 
                                           ifelse(str_detect(script, "^\\t{1}(?!\\t)"), "speech", "other")))


## Seven ## -------------------------
seven <- scripts_raw %>% filter(title == "Seven")
## table(seven$space_count)
## no tabs
## 14> spaces - scene setting/description
## 14-21 spaces - speech
## 32-34 spaces - character

seven <- seven %>% mutate(label = ifelse(str_detect(script, "SEVEN"), "other",
                                         ifelse(space_count %in% c(32:34) & str_detect(script, "^[A-Z\\s]"), "character",
                                         ifelse(space_count %in% c(14:21), "speech", "other"))))



## Silence of the Lambs ## -------------------------
lambs <- scripts_raw %>% filter(title == "Silence of the Lambs")
## table(lambs$space_count)
## no tabs
## 15 spaces - scene setting/description
## 25 spaces - speech
## 37 spaces - character

lambs <- lambs %>% mutate(label = ifelse(space_count == 25, "speech",
                                         ifelse(space_count == 37, "character", "other")))


#### --------------------------------------------------------------------------

## let's bind everything back together
scripts <- bind_rows(shawshank, godfather, pulp_fiction,
                     fight_club, forrest_gump, inception, 
                     the_matrix, cuckoo, seven, lambs)

## we can now get rid of the tabs
scripts$script <- str_replace_all(scripts$script,"\\t","")

## and trim whitespaces as we no longer need them
scripts$script <- trimws(scripts$script)

## time to match speaker to speech
## I'll give the first row a speaker label because I will use a function to fill down NAs
## with the latest previous non-NA 

scripts <- scripts %>% 
  mutate(speaker = ifelse(scripts$label == "character", scripts$script, 
                          ifelse(line_number == 1, "NO SPEAKER", NA)))

## fill down speaker
scripts$speaker <- na.locf(scripts$speaker)
## table(scripts$speaker)

## View(scripts)

## and change every speaker to "NO SPEAKER" where the label isn't speech
scripts$speaker <- ifelse(scripts$label == "speech", scripts$speaker, "NO SPEAKER" )

## remove trailing expressions such as (O.S), (Contd), "'s Voice" etc. to have the name only
scripts$speaker <- str_replace_all(scripts$speaker, "\\(.*\\)", "")
scripts$speaker <- str_replace_all(scripts$speaker, "'S VOICE", "")

## remove spaces
scripts$speaker <- trimws(scripts$speaker)

## drop space count colum
scripts <- scripts %>% select(-space_count)

## I've noticed a couple of typos in character names so I'm going to export this
## csv to openrefine and have a look to do some clustering - I'm using this
## because it's quicker than trying to explore stuff by myself in r

## save the file
fwrite(scripts, "./data/scripts.csv")

refine_upload(file = "scripts.csv", project.name = "scripts_cleaner", open.browser = T)

## saved the clustering operations to a separate JSON file

scripts_clean <- refine_export(project.name = "scripts_cleaner")

fwrite(scripts_clean, "./data/scripts_clean.csv")












