library(dplyr)
library(data.table)
library(stringr)
library(ggplot2)


### Let's start validating the genders

characters <- fread("./data/characters.csv")

## joining datasets tends to duplicate rows so let's take the unique values only
characters <- unique(characters)
characters %>% filter(gender == "")

## what is wrong with BOYLE again?? 
characters$gender <- ifelse(characters$gender == "" & characters$speaker == "BOYLE", "male", characters$gender)


# let's cover the plurals, we won't need them
# View(characters %>% filter(str_detect(speaker, ".S$") | gender == "multiple"))

characters$gender <- ifelse(characters$speaker %in% c("CHILDRENS", "PATIENTS", "ACUTES","ALL SPACE MONKEYS",
                                           "COPS", "KITCHEN WORKERS", "PATRONS",
                                           "SEVERAL ACUTES", "THREE ATTENDANTS", "VARIOUS VOICES",
                                           "VOICES") | characters$gender == "multiple", "unclear", characters$gender)


# View(characters %>% filter(gender == "female"))
## I found a couple of obvious mistakes among the females, let's fix those as well

characters$gender <- ifelse(characters$speaker %in% c("ANGEL FACE", "BOB HOPE", "BLATCH", "HADLEY",
                                                      "LOU", "NERI", "PRESIDENT KENNEDY", "PUMPKIN",
                                                      "TAYLOR"), "male",
                            ifelse(characters$speaker %in% c("ANNOUNCER", "FLIGHT ATTENDANT", 
                                                             "REDHEAD", "REDHEAD DETECTIVE", "SHANKLE", "PENGUIN"), "unclear", characters$gender))


## there are a couple of recurring themes here so let's break it down to movies. 

## The Shawshank Redemption --------------------------------------------------------------------------------------------------------------

# View(characters %>% filter(title == "The Shawshank Redemption"))

## guards are all male - probably related to the fact that the movie is about a male prison
## any sort of managers mentioned in the script are also male according to IMDB
## the projectionist is male
## I'm not gonna guess the voices, given the tendency they are probably male as well but I have no legit way of telling
## RORY is not credited so unclear
## CHAPLAIN is not credited
## no doctor appears in the movie


# characters$gender <- ifelse(characters$title == "The Shawshank Redemption" & (str_detect(characters$speaker, "\\bGUARD\\b")|
#                                                                                 (str_detect(characters$speaker, "\\bPROJECTIONIST\\b")|
#                                                                                    (str_detect(characters$speaker, "\\bMANAGER\\b")))), "male", 
#                                                                                  ifelse(characters$title == "The Shawshank Redemption" & characters$gender == "unknown", "unclear",
#                                                                                         characters$gender))

characters$gender <- case_when(
  characters$title == "The Shawshank Redemption" & str_detect(characters$speaker, "\\bGUARD\\b|\\bPROJECTIONIST\\b|\\bMANAGER\\b") ~ "male",
  characters$title == "The Shawshank Redemption" & str_detect(characters$speaker, "\\bRORY\\b|\\bDOCTOR\\b|\\bCHAPLAIN\\b|\\bVOICE") ~ "unclear",
  characters$title == "The Shawshank Redemption" & characters$gender %in% c("unknown", "multiple") ~ "unclear",
  TRUE ~ characters$gender
)


## Godfather ----------------------------------------------------------------------------------------------------------------------------

# View(characters %>% filter(title == "Godfather"))

## Bonasera is a male character, Mrs Bonasera is his wife
## Nazorine is male 
## all drivers mentioned are male
## the priest is male
## the only credited nurse is female

# characters$gender <- ifelse(characters$title == "Godfather" & characters$speaker %in% c("BONASERA", "NAZORINE", "DRIVER", "PRIEST"), "male",
#                             ifelse(characters$title == "Godfather" & characters$speaker %in% c("MRS. BONASERA", "NURSE"), "female", 
#                                    ifelse(characters$title == "Godfather" & characters$gender == "unknown", "unclear", characters$gender)))

characters$gender <- case_when(
  characters$title == "Godfather" & characters$speaker %in% c("BONASERA", "NAZORINE", "DRIVER", "PRIEST") ~ "male",
  characters$title == "Godfather" & characters$speaker %in% c("MRS. BONASERA", "NURSE") ~ "female",
  characters$title == "Godfather" & characters$gender %in% c("unknown", "multiple") ~ "unclear",
  TRUE ~ characters$gender
)

## Pulp Fiction -------------------------------------------------------------------------------------------------------------------------

# View(characters %>% filter(title == "Pulp Fiction"))

## there's only one Gawker mentioned on IMDB and it is a woman
## the Waitress is a woman I assume
## Sportcasters appear to be male
## PEDESTRIAN is female

# characters$gender <- ifelse(characters$title == "Pulp Fiction" & (str_detect(characters$speaker, "\\bGAWKER\\b") |
#                                                                     str_detect(characters$speaker, "\\bWAITRESS\\b")), "female",
#                             ifelse(characters$title == "Pulp Fiction" & str_detect(characters$speaker, "\\bSPORTCASTER\\b"), "male", 
#                                    ifelse(characters$title == "Pulp Fiction" & characters$gender == "unknown", "unclear", characters$gender)))

characters$gender <- case_when(
  characters$title == "Pulp Fiction" &str_detect(characters$speaker, "\\bGAWKER\\b|\\bWAITRESS\\b|\\bPEDESTRIAN\\b") ~ "female",
  characters$title == "Pulp Fiction" &str_detect(characters$speaker, "\\bSPORTSCASTER\\b") ~ "male",
  characters$title == "Pulp Fiction" & characters$gender %in% c("unknown", "multiple") ~ "unclear",
  TRUE ~ characters$gender
)

## Fight Club -------------------------------------------------------------------------------------------------------------------------

# View(characters %>% filter(title == "Fight Club"))

## Fight club has only male space monkeys credited on IMDB
## and the desk clerk appears to be a woman
## there's only a female reporter credited on IMDB
## the only attendant credited is also female
## the leader in the script is a woman according to the movie - she is the therapy group's leader
## MECHANIC is male
##

# characters$gender <- ifelse(characters$title == "Fight Club" & str_detect(characters$speaker, ".SPACE MONKEY"), "male",
#                                                                           ifelse(characters$title == "Fight Club" & 
#                                                                                    characters$speaker %in% c("ATTENDANT",
#                                                                                                              "REPORTER", "DESK CLERK", "LEADER"), 
#                                                                                  "female",
#                                                                                  ifelse(characters$title == "Fight Club" & characters$gender == "unknown", "unclear", characters$gender)))

characters$gender <- case_when(
  characters$title == "Fight Club" & str_detect(characters$speaker, ".SPACE MONKEY|\\bMECHANIC\\b") ~ "male",
  characters$title == "Fight Club" & characters$speaker %in% c("ATTENDANT","REPORTER", "DESK CLERK", "LEADER") ~ "female",
  characters$title == "Fight Club" & characters$gender %in% c("unknown", "multiple") ~ "unclear",
  TRUE ~ characters$gender
)

## Forrest Gump -------------------------------------------------------------------------------------------------------------------------

# View(characters %>% filter(title == "Forrest Gump"))

## there are no soldiers played by female actors in Forrest Gump
## per IMDB the 'Doctor' is a male character
## all credited coaches are male
## players are all male
## reporters credited are male
## SCHOOLD BUS DRIVER in Forrest Gump is female
## FORREST & RECRUITS is more than one person
## MASAI is unclear
## MEN is plural - gender wise it's clear but I'll not count it as it's multiple characters
## MRS. BLUE & 	MRS. GUMP are female


# characters$gender <- ifelse(characters$title == "Forrest Gump" & str_detect(characters$speaker, "\\bSOLDIER\\b"), "male",
#                             ifelse(characters$title == "Forrest Gump" & str_detect(characters$speaker, "\\bOFFICER\\b"), "male",
#                                    ifelse(characters$title == "Forrest Gump" & str_detect(characters$speaker, "\\bCOACH\\b"), "male",
#                                           ifelse(characters$title == "Forrest Gump" & (str_detect(characters$speaker, "\\bPLAYER\\b") | 
#                                                                                          str_detect(characters$speaker, "\\bDOCTOR\\b") | 
#                                                                                          str_detect(characters$speaker, "\\bREPORTER\\b") |
#                                                                                          str_detect(characters$speaker, "\\bPOLICEMAN\\b") | 
#                                                                                          str_detect(characters$speaker, "\\bNEWSCASTER\\b") | 
#                                                                                          str_detect(characters$speaker, "\\bPRINCIPAL\\b")), "male", 
#                                                  ifelse(characters$title == "Forrest Gump" & characters$gender == "unknown", "unclear", characters$gender)))))

characters$gender <- case_when(
  characters$title == "Forrest Gump" & 
    str_detect(characters$speaker, "\\bSOLDIER\\b|\\bOFFICER\\b|\\bCOACH\\b|\\bPLAYER\\b|\\bREPORTER\\b|\\bPOLICEMAN\\b|\\bNEWSCASTER\\b|\\bPRINCIPAL\\b") ~ "male",
  characters$title == "Forrest Gump" & str_detect(characters$speaker, "MRS. |\\bSCHOOLD\\b") ~ "female",
  characters$title == "Forrest Gump" & (characters$gender %in% c("unknown", "multiple") | str_detect(characters$speaker, "\\bMASAI\\b|\\bMEN\\b|&")) ~ "unclear",
  TRUE ~ characters$gender
)

## Inception -------------------------------------------------------------------------------------------------------------------------

# View(characters %>% filter(title == "Inception"))

## laywers are male
## not sure about attendants 
## businessmen are male
## MAL is female
## flgith attendant is female

# characters$gender <- ifelse(characters$title == "Inception" & characters$speaker %in% c("LAWYER", "BUSINESSMAN"), "male", 
#                             ifelse(characters$title == "Inception" & characters$gender == "unknown", "unclear", characters$gender))

characters$gender <- case_when(
  characters$title == "Inception" & str_detect(characters$speaker,"\\bLAWYER\\b|\\bBUSINESSMAN\\b") ~ "male",
  characters$title == "Inception" & characters$speaker %in% c("MAL", "FLIGHT ATTENDANT") ~ "female",
  characters$title == "Inception" & characters$gender %in% c("unknown", "multiple") ~ "unclear",
  TRUE ~ characters$gender
)

## The Matrix -------------------------------------------------------------------------------------------------------------------------

# View(characters %>% filter(title == "The Matrix"))

## GIZMO is not credited
## MOJO is not credited
## ORACLE is female
## Agent Brown is male
## Dujour is female
## Mouse is male
## Switch is female
## the others are unknown

# characters$gender <- ifelse(characters$title == "The Matrix" & characters$speaker %in% c("AGENT BROWN", "MOUSE"), "male", 
#                             ifelse(characters$title == "The Matrix" & characters$speaker %in% c("DUJOUR", "SWITCH"), "female",
#                                    ifelse(characters$title == "The Matrix" & characters$gender == "unknown", "unclear", characters$gender)))

characters$gender <- case_when(
  characters$title == "The Matrix" & characters$speaker %in% c("AGENT BROWN", "MOUSE") ~ "male",
  characters$title == "The Matrix" & characters$speaker %in% c("DUJOUR", "SWITCH", "ORACLE") ~ "female",
  characters$title == "The Matrix" & (characters$gender %in% c("unknown", "multiple") | characters$speaker %in% c("MOJO", "GIZMO")) ~ "unclear",
  TRUE ~ characters$gender
)


## One Flew Over the Cuckoo's Nest -----------------------------------------------------------------------------------------------------

# View(characters %>% filter(title == "One Flew Over the Cuckoo's Nest"))

## all nurses credited are female
## there's only one doctor credited in IMDB, it's a male doctor
## Cheswick is male, and his name is misspelled in a couple of rows
## Miss Pilbow sounds female: she is referenced as both Miss Pilbow and Nurse Pilbow 
## RUCKLY is actually Ruckley according to IMDB (male)
## SCANLON is male
## SEFELT is male
## SPIVEY is male
## BANCINI is male
## BIG NURSE is referring to Miss Ratched - female

# characters$gender <- ifelse(characters$title == "One Flew Over the Cuckoo's Nest" & characters$speaker %in% c("DOCTOR", "RUCKLY", "CHESWICK", "CHESWTCK",
#                                                                                                               "SCANLON", "SEFELT", "SPIVEY", "BANCINI"), "male",
#                             ifelse(characters$title == "One Flew Over the Cuckoo's Nest" & characters$speaker %in% c("NURSE", "MISS PILBOW", "NURSE PILBOW"), "female",
#                                    ifelse(characters$title == "One Flew Over the Cuckoo's Nest" & characters$gender == "unknown", "unclear", characters$gender)))
# 

characters$gender <- case_when(
  characters$title == "One Flew Over the Cuckoo's Nest" & characters$speaker %in% c("DOCTOR", "RUCKLY", "CHESWICK", "CHESWTCK",
                                                                                    "SCANLON", "SEFELT", "SPIVEY", "BANCINI") ~ "male",
  characters$title == "One Flew Over the Cuckoo's Nest" & str_detect(characters$speaker, "\\bNURSE\\b|\\bMISS\\b") ~ "female",
  characters$title == "One Flew Over the Cuckoo's Nest" & characters$gender == "unknown" ~ "unclear",
  TRUE ~ characters$gender
)

## Seven -------------------------------------------------------------------------------------------------------------------------------

# View(characters %>% filter(title == "Seven"))

## there are two detectives listed on IMDB, one male one female so I'll go with unclear as I can't tell which one speaks in the script
## there's no bleeker or attendant listed on IMDB
## there's only one officer listed as male
## no photographer on IMDB
## policeman is probably a male one
## reporters appear to be female on IMDB
## BARKER is unclear
## there are both male and female 'cops' credited on imdb - in the text I'll label cop as male and female cop as female
## MOVER isn't credited
## the only OFFICER credited is male
## SKETCH ARTIST is female
## VAGRANT is female

# characters$gender <- ifelse(characters$title == "Seven" & characters$speaker %in% c("OFFICER", "POLICEMAN"), "male",
#                             ifelse(characters$title == "Seven" & characters$speaker == "REPORTER", "female", 
#                                    ifelse(characters$title == "Seven" & characters$gender == "unknown", "unclear", characters$gender)))

characters$gender <- case_when(
  characters$title == "Seven" & characters$speaker %in% c("OFFICER", "POLICEMAN") ~ "male",
  characters$title == "Seven" & characters$speaker %in% c("REPORTER", "SKETCH ARTIST", "VAGRANT") ~ "female",
  characters$title == "Seven" & characters$gender == "unknown" ~ "unclear",
  TRUE ~ characters$gender
)

## Silence of the Lambs -----------------------------------------------------------------------------------------------------------------

 View(characters %>% filter(title == "Silence of the Lambs"))

## the only attendant listed on IMDB is male
## both boxing and FBI instructors listed are male
## KRENDLER is male
## MR. BIMMEL is male
## ORDERLY is male
## PEMBRY is male
## PILCHER is male
## TV ANCHORS are unclear
## DR. DANIELSON isn't credited
## FOREMAN isn't credited
## JACOBS is female
## PETERSON isn't credited
## SEN. MARTIN is female

# characters$gender <- ifelse(characters$title == "Silence of the Lambs" & characters$speaker %in% c("KRENDLER", "MR. BIMMEL", "ORDERLY", "PEMBRY",
#                                                                                                    "PILCHER", "INSTRUCTOR", "ATTENDANT"), "male",
#                             ifelse(characters$title == "Silence of the Lambs" & characters$gender == "unknown", "unclear", characters$gender))

characters$gender <- case_when(
  characters$title == "Silence of the Lambs" & characters$speaker %in% c("KRENDLER", "MR. BIMMEL", "ORDERLY", "PEMBRY",
                                                                         "PILCHER", "INSTRUCTOR", "ATTENDANT", "BOYLE") ~ "male",
  characters$title == "Silence of the Lambs" & characters$speaker %in% c("JACOBS", "SEN. MARTIN") ~ "female",
  characters$title == "Silence of the Lambs" & (characters$gender == "unknown" | characters$speaker %in% c("PETERSON", "TV ANCHOR",
                                                                                                           "2ND TV ANCHOR", "DR. DANIELSON")) ~ "unclear",
  TRUE ~ characters$gender
)

## check distribution now
ggplot(characters, aes(reorder(gender, gender, function(x)-length(x)))) +
  geom_bar(fill = "#bfbfbf") +
  labs(title = "Gender distribution after cleanup", x = "gender", y = "Count") +
  theme_bw()

## so at the moment there are slightly more female characters than unknown.. but even if we considered all unknown as female we
## wouldn't reach half the number of male characters


## a lot of characters don't have significant contribution so let's filter for 
## everyone with more than 15 lines

ggplot(characters %>% filter(line_count >15), aes(reorder(gender, gender, function(x)-length(x)))) +
  geom_bar(fill = "#bfbfbf") +
  labs(title = "Gender distribution after cleanup", x = "gender", y = "Count") +
  theme_bw()

## looks better but still pretty weird, I didn't really expect female presence to be this low

## let's join the genders back to the original data
scripts <- fread("scripts_clean.csv")
scripts$speaker <- ifelse(scripts$speaker == "FRIEND #l", "FRIEND #1", scripts$speaker)

df <- merge(scripts, characters, by = c("title", "speaker"), all.x = TRUE) %>% 
  select(line_number, title, script, label, speaker, gender) %>% 
  arrange(line_number)

df %>% filter(gender == "")

fwrite(df, "./data/df.csv")



 


