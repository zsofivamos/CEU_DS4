library(rvest)
library(dplyr)
library(stringr)
library(data.table)

## the films I'll be looking at are the following ten
## selection criteria:
## I used IMDB's top rated English language movies
## as of 9th May, 2020 https://www.imdb.com/chart/top-english-movies
## sequels and superhero movies were excluded
## Lord of the Rings was also excluded as the characters are mostly fictional
## their genders are difficult to assume
## I excluded everything from before 1970 because those scripts were not available

## Movies in order of IMDB ranking
## - The Shawshank Redemption 
## - Godfather
## - Pulp Fiction
## - Fight Club
## - Forrest Gump
## - Inception
## - The Matrix
## - One Flew Over the Cuckoo's Nest
## - Seven
## - Silence of the Lambs

## I'll use https://www.imsdb.com/ for fetching the scripts
## first, let's create the urls

## I'll get the titles into a character vector
titles <- c("The Shawshank Redemption", "Godfather", "Pulp Fiction", "Fight Club", "Forrest Gump", "Inception", "The Matrix",
            "One Flew Over the Cuckoo's Nest", "Seven", "Silence of the Lambs")


# a function to scrape the data
get_scripts <- function(title){
  
  ## check if the title starts with "The " and in that case append it with 
  ## ", The" to match the link's format on the website
  url_title <- ifelse(startsWith(title, "The "), paste0(title, ", The"), title) 
  
  ## Then remove the leading "The "
  url_title <- gsub("^The ", "", url_title)
  
  ## Replace whitespaces with a hyphen
  url_title <- gsub("\\s", "-", url_title)
  
  ## This is very specific to only one of the movies - the title of Seven 
  ## is spelled as Se7en in the link
  url_title <- ifelse(str_detect(url_title, "Seven"), "Se7en", url_title)
  
  # create url
  url <- paste0("https://www.imsdb.com/scripts/",url_title,".html")
  
  # read url
  t <- read_html(url)
  
  # get full script 
  # incorporate ifelse statement as not all scripts are in the same html_node
  full_script <- 
    ifelse(identical(t %>% 
                       html_nodes('pre pre')%>%
                       html_text(), character(0)),
           t %>% 
             html_nodes('pre')%>%
             html_text(),
           t %>% 
             html_nodes('pre pre')%>%
             html_text())
  
  
  # remove leading and trailing whitespaces and read every line into a row
  script <- read_lines(full_script, skip = 0, skip_empty_rows = F, n_max = -1, progress= show_progress())
  
  script <- grep("\\S", script, value = T)
  
  # it's still a character vector, let's convert it to a tibble
  # I am labelling every movie with their title in a separate column
  movie <- tibble(title = title, script)
  
  movie <- movie %>% mutate(space_count = str_count(movie$script, regex("\\s(?=\\s)"))+1)
  
  # write results out to file
  # allow appending to have all data in one file
  write_csv(movie, "./data/movie_scripts.csv", append = TRUE)
  print(paste0(title, " - done"))
}

## let's do this!
lapply(titles, get_scripts)

