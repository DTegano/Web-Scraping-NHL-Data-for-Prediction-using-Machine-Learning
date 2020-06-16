# Web Scraping NHL Data for Prediction using MachineLearning

<b> Project Background</b>: Using data from the 2017-2018 & 2018-2019 NHL Seasons, I will train a Machine Learning model to predict the games (at least the ones completed before quarantine) for the 2019-2020 season. This Project will be completed in two different parts - Web Scraping (data collection & cleaning) and Predicting using Machine Learning. My model will learn based on the aggregated stats of a given team, at any time in a season, before the scheduled outcome occurs. Using the Rvest package, I will create a web scraper to gather all the data I can from each NHL game for each season. At the time of writing, I'm exclusively using hockey-reference.com for this data. Once my data is pulled, the stats from each game include: Date of the game, Home Team, Away Team, Result (training only - reflects Home Team), Home Goals, Home Shots, Away Goals, Home PIM, Away PIM, Away Shots, Home Corsi (all situations), Away Corsi, Home Offensive Zone Start %, Away Offensive Zone Start %, Home Hits, Away Hits, Home Blocked Shots, Away Blocked Shots, Game Length (Regulation, Overtime, or Shootout), Empty_Netters, Home Save %, Away Save %, Home Shooting %, Away Shooting %, Home SPSV%/PDO, Away SPSV%/PDO, Home Goals Against, Away Goals Against, Home Differential, Away Differential, Home Wins, Away Wins, Home Shots Against, Away Shots Against, Home Points and Away Points. Prediction results will depend on whether it was the home or away team that won the game. 
<p></p>

# Libraries Used:
```{r}
library(rvest) # web scraping
library(readxl) # importing xls & xlsx files
library(dplyr) # used for nesting/chaining
library(tidyr) # data cleaning
library(stringr) # Used to detect OT and Shootouts
library(writexl) # exporting to xlsx file
library(RCurl) # Checking URL
library(chron) # Date conversion to Time
library(stats) # Group Functional
library(plyr) # ddpylr for aggregation
library(ggplot2) # K-Means Plot
library(ggfortify) # K-Means Plot
```

The Rvest package is vital to making the web scraper work - the code is built for Rvest. The Readxl and writexl packages helped me save and backup my data frame into an excel sheet once my data is pulled. Dplyr was key to shortening my script with its' chaining functions  - as I used this in each once of my team's functions. Tidyr was used in cleaning, separating, and uniting my data together. The stringr package was a wildcard when I started this project, but it became a centerpiece to recognizing which games were decided by regulation, overtime or shootout. RCurl was a key package in my web scraping automation as I only pulled data from certain URLS that existed - my web scraper was built based on the pattern of the website and checked many different URL variations. Chron helped me convert the text date (such as October 3rd, 2018 7:00 PM) into an actual Time type/format for easy sorting. The Plyr package was a necessity to aggregating my data properly, and the stats/ggplot packages allowed me to perform some basic k-means clustering analysis in part 1 of this project. 
<p></p>

# Functions 

To pull basic stats for each team:
```{r}
GET_ANA = function (x) {
    table = x %>%
        html_node(xpath = '//*[@id="ANA_skaters"]') %>%
        html_table()
    names(table) = lapply(table[1,], as.character)
    table = table[-1,-1]
    table = table[multi_roster(table), c(2,6,14)]
    }
```
I'll note that the above code was tweaked several times throughout this project's lifecycle. Originally, the code for each of the 31 teams was much larger - this was eventually narrowed down by web scraping using the xpath as opposed to a constant node path. Toward the end of my project, I figured it was better to include some data cleaning steps into each team's function - as I only wanted to grab certain data (as opposed to all of the data by every single player per team) for this project. The above code had to be reproduced 30 additional times to capture the ID for each team. The multi-roster code used will be explained below.

To pull advanced stats for each team:
```{r}
ANA_ADV = function(x) {
  table = x %>%
    html_nodes(xpath = '//comment()') %>%
    html_text() %>%
    paste(collapse = '') %>%
    read_html() %>%
    html_node('table#ANA_adv') %>%
    html_table()
  table = table[inc_roster(table), c(5,9,10,11)]
}
```
This code was a bit tricky to figure out compared to the basic player stats table for each team. All of the advanced stats tables were embedded in html comments as opposed to table tags - meaning I figure had to read the comment texts and collapse. Again, this code was reproduced 30 more times to capture this new table's ID for every team. The inc_roster code/function will also be explained below.

Pulling in the date at each url:
```{r}
date_f = function(x) {
  x %>%
    html_node("div.scorebox_meta") %>%
    html_node("div") %>%
    html_text()
}
```


The date, believe it or not, is a key factor to this machine learning project. All of the games need to be in order from oldest to newest so that the team stats can aggregate as the season progresses. Eventually, I want to be able to predict games day by day - which means I'll need all of the updated stats that reflect every game played by that team in that season.

Pulling in the name of each team and goals per game:

```{r}
away_team_name = function(x) {
  x %>%
    html_node(xpath = '//*[@id="content"]/div[2]/div[1]/div[1]/strong/a') %>%
    html_text()
}

home_team_name = function(x) {
  x %>%
    html_node(xpath = '//*[@id="content"]/div[2]/div[2]/div[1]/strong/a') %>%
    html_text()
}

away_score_f = function(x) {
  x %>%
    html_node(xpath = '//*[@id="content"]/div[2]/div[1]/div[2]/div') %>%
    html_text() %>%
    as.numeric()
}

home_score_f = function(x) {
  x %>%
    html_node(xpath = '//*[@id="content"]/div[2]/div[2]/div[2]/div') %>%
    html_text() %>%
    as.numeric()
}
```

Now
