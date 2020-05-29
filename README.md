## CEU_DS4

This repo contains all the codes used for my NLP assignment at Central European University. The main goal of the project was to explore whether female characters use a more one dimensional set of words in popular movies compared to their male counterparts. 

### Data

I chose to work with movies from the ["Top Rated English Movies"](https://www.imdb.com/chart/top-english-movies)  on IMDB as of 15th May 2020. The population was selected in order of rankings based on the following criteria:

* Movies **made before 1970** have been **excluded** to avoid detecting patterns from more than 50 years ago
* I only looked at standalone movies, **no sequels or prequels** - this aimed to reduce the chances of getting unnecessary duplications of patterns detected in one movie
* **Superhero and fantasy movies** have also been **excluded** on the grounds that determining gender of important charactrs could be very difficult. I don't know how to classify fairies, space racoons or trees, so I decided it's better to leave those alone and focus on regular human characters.

**The winners were:**
1.	The Shawshank Redemption 
2. Godfather
3. Pulp Fiction
4. Fight Club
5. Forrest Gump
6. Inception
7. The Matrix<sup>\*</sup>
8. One Flew Over the Cuckoo's Nest
9. Seven
10. Silence of the Lambs

<sup>\*:I am aware that The Matrix is borderline fantasy but I chose to keep it as the main focus is the human world. (And the epic limbo moves.)</sup>


### Methodology

I gathered all the scripts from [IMSDB](https://www.imsdb.com/) by using the **rvest** library in [R Studio] (https://rstudio.com/). I wrote a function to extract and append the data into a csv file, then used lapply() to run it on all ten titles. 

The first part of data cleaning consisted of me trying to tell speech apart from everything else. Not going to lie, it was fairly manual as all ten scripts were uploaded with different formatting characteristics. And although every script was unique, they all differentiated between speech, character names and scene setting/descriptions with the help of indentation. So I the data up and looked through the patterns per the individual movies and made notes on the observable features. I used the **stringr** library for this.

Once I had the labels figured out - speech, character, other - I created a new column and copied over the values from the character column where there was one. Everything else was left blank. Then, using the na.locf() function of the **zoo** library I downfilled the character names throughout the blanks until the next one appeared. So this way, I ended up with a pretty good idea of who said what. 

I used [OpenRefine](https://openrefine.org/) for string clustering to eliminate mispronounced versions of the same name. And then came the fun part.

I'm not going to lie, this one gave me a hard time - I had to classify more than 500 characters into the gender categories in scope. I tried a lot of different methods but I ended up sticking to an API based classification. I used the (free) services of the [Gender API](https://gender-api.com/). In most cases it worked okay, but I still had to do a lot of manual validation to make sure everyone is in the right category. There were some cases where the gender was hard to determine, e.g. multiple people were speaking or the character was not actually credited on IMDB, so I flagged these ones as "unclear". 

Finally, I did all the text analysis using **dplyr** and **tidytext** with some help from **ggplot2** in terms of visualizations. 

I posted a summary of the analysis [here]().










