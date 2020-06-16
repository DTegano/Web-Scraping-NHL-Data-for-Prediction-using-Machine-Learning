# Web Scraping NHL Data for Prediction using MachineLearning

<b> Project Background</b>: Using data from the 2017-2018 & 2018-2019 NHL Seasons, I will train a Machine Learning model to predict the games (at least the ones completed before quarantine) for the 2019-2020 season. This Project will be completed in two different parts - Web Scraping (data collection) and Predicting using Machine Learning. My model will learn based on the aggregated stats of a given team, at any time in a season, before the scheduled outcome occurs. Using the Rvest package, I will create a web scraper to gather all the data I can from each NHL game for each season. At the time of writing, I'm exclusively using hockey-reference.com for this data. Once my data is pulled, the stats from each game include: Date of the game, Home Team, Away Team, Result (training only - reflects Home Team), Home Goals, Home Shots, Away Goals, Home PIM, Away PIM, Away Shots, Home Corsi (all situations), Away Corsi, Home Offensive Zone Start %, Away Offensive Zone Start %, Home Hits, Away Hits, Home Blocked Shots, Away Blocked Shots, Game Length (Regulation, Overtime, or Shootout), Empty_Netters, Home Save %, Away Save %, Home Shooting %, Away Shooting %, Home SPSV%/PDO, Away SPSV%/PDO, Home Goals Against, Away Goals Against, Home Differential, Away Differential, Home Wins, Away Wins, Home Shots Against, Away Shots Against, Home Points and Away Points. Prediction results will depend on whether it was the home or away team that won the game. There will be some other factors that I may eventually include in my model such as: Last 10 games Win % again Opponent, Faceoffs, and Health (mainly a subjective factor). I may or may not enter these factors right away depending on where I'm at with my Project's deadlines. I’ll also need to gather all of the roster information from the 2017-2018, 2018-2019, and 2019-2020 seasons for team improvement comparisons to be used in the first game for each team.
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

The short description above basically sums up why each package is needed. The Rvest package is vital to making the web scraper work - the code is built for Rvest. The Readxl and writexl packages helped me save and backup my data frame into an excel sheet once my data is pulled. Dplyr was key to shortening my script with its' chaining functions  - as I used this in each once of my team's functions. Tidyr was used in cleaning, separating, and uniting my data together. The stringr package was a wildcard when I started this project, but it became a centerpiece to recognizing which games were decided by regulation, overtime or shootout. RCurl was a key package in my web scraping automation as I only pulled data from certain URLS that existed - my web scraper was built based on the pattern of the website and checked many different URL variations. Chron helped me convert the text date (such as October 3rd, 2018 7:00 PM) into an actual Time type/format for easy sorting. The Plyr package was a necessity to aggregating my data properly, and the stats/ggplot packages allowed me to perform some basic k-means clustering analysis in part 1 of this project. 
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
