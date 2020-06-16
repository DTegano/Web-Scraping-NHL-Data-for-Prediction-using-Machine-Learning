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
<p></p>

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
<p></p>

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
<p></p>

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

Luckily, the pattern of www.hockey-reference.com is quite consistent and made these functions easy to set up. The xpath is generally the same for each of the home and away team for each URL - with the difference being one small change in the 'div' container where the information is stored. Both of the above functions serve it's purpose - the names of each team are binded and stored in each data frame per game within the clean function set up below. The team scores determine whether the result we are predicting (for the home team) is a "W" for win or "L" for loss regardless if the game went into Overtime or Shootout. 1 Point is still awarded to the losing team if Overtime or a Shootout was needed.
<p></p>

Cleaning each basic and advanced table, for each team, and combining into one data frame:
```{r}
game_clean = function(w,x,y,z) {
  d = cbind(w,x)
  d = cbind(result,d)
  t= cbind(home_team, away_team)
  d = cbind(t,d)
  d = cbind(dat, d)
  q = cbind(y,z)
  d = cbind(d, q)
  d = cbind(d,reg)
  d = cbind(d,emp)
}
```
I got to learn the hardway not to have a 'date' data frame when using automatic web scraper that includes 'for date in date_month {' as part of your code. Thankfully, I was able to make the small change from "date" to "dat" to get all of the wheels greased and working again. This cleans and binds all the data for one game together. This way, it is much easier to rbind all of my rows for each game once the last part of the web scraper code has been run.
<p></p>

This checks to see if any game goes into Overtime or Shootout:
```{r}
Read_Boxscore = function(x) {
  x %>%
    html_node(xpath = '//*[@id="scoring"]') %>%
    html_text()
}

Regulation = function(x) {
  y = str_detect(Read_Boxscore(x), "OT Period")
  z = str_detect(Read_Boxscore(x), "Shootout")
  ifelse(y == TRUE, "O", ifelse(z == TRUE, "S", "R"))
}
```

By reading the box score summary of each URL, my string detect function looks for any sign of "OT Period" or "Shootout". This is a revised code. The first code hilariously only read "OT" which was sufficient for every single team - except Ottawa. Since the Senators used a Team ID of "OTT" - well, you can imagine how the reading was off. Once this was fixed, I added a column into each data frame to represent "R" for Regulation, "O" for Overtime, and "S" for Shootout. This is currently only useful for tracking points earned for each game, but I'd like this category to be part of my predictions in the distant future.
<p></p>

Using a similar code to the above, I was also able to read and find empty net goals in the box score:
```{r}
emp_net = function(x) {
    str_count(Read_Boxscore(x), "EN")
}
```

This is a key part when calculating save % for teams. Empty net goals do not count towards a goaltender's save %, so to best line up my data with a team's overall save %, I needed to exclude empty net goals from the calcluation. I also thought there can be a correlation to a team's winning success - as a team with more empty net goals may prove to secure their victories easier than others.
<p></p>

For games with multiple-goalies played and/or incorrect rosters:
```{r}
multi_roster = function(x) {
  ifelse(nrow(x)>20,nrow(x),20)
}

inc_roster = function(x) {
    ifelse(nrow(x)!=190,nrow(x)-8,182)
}
```
The explanation you may, or may not, have been looking for in the above codes. As I was combing through the different URLS, I noticed that not all of the basic stat tables had the same # of rows. While it is mandatory for teams to dress 18 skaters before each game, multiple goalies that play increase the # of rows per table. The above code, mult-roster, was an easy fix and will certainly come in handy for those rare games when an emergency goalie has to come in and play.

The same holds true for inc_roster. While teams cannot have holes in their lineups for games, I did find a few games where incorrect lineup submissions caused a team to start the game shorthanded. To fix this issue, inc_roster will still pull the same data regardless of how many players are dressed.
<p></p>

Though not a function, this variable assists in re-naming the columns as part of the data cleaning process:
```{r}
cnames = c("Date", "Home", "Away", "Result", "Home_G", "Home_PIM", "Home_S", "Away_G", "Away_PIM", "Away_S", "Home_Corsi", "Home_OFS", "Home_H", "Home_BS", "Away_Corsi", "Away_OFS", "Away_H", "Away_BS", "Game_Length", "Empty_Netters")
```
# Pulling Game Information

Here is an example code used for each game:
```{r}
chivan_1031 =  read_html("https://www.hockey-reference.com/boxscores/201810310VAN.html")

CHI_table = GET_CHI(chivan_1031)

VAN_table = GET_VAN(chivan_1031)

CHI_ADV_table = CHI_ADV(chivan_1031)

VAN_ADV_table = VAN_ADV(chivan_1031)

date = date_f(chivan_1031)

date = as.data.frame(date)

away_team = away_team_name(chivan_1031)

home_team = home_team_name(chivan_1031)

away_score = away_score_f(chivan_1031)

home_score = home_score_f(chivan_1031)

result = ifelse(away_score>home_score, "L", "W")

result = as.data.frame(result)

reg = Regulation(chivan_1031)

names(CHI_table) = lapply(CHI_table[1,], as.character)

CHI_table = CHI_table[-1,-1]

names(VAN_table) = lapply(VAN_table[1,], as.character)

VAN_table = VAN_table[-1,-1]

CHI_table = CHI_table[multi_roster(CHI_table), c(2,6,14)]

VAN_table = VAN_table[multi_roster(VAN_table), c(2,6,14)]

CHI_table$G = as.numeric(CHI_table$G)

VAN_table$G = as.numeric(VAN_table$G)

CHI_table$G = ifelse(reg == "S" & result == "L", (CHI_table$G + 1), CHI_table$G)

VAN_table$G = ifelse(reg == "S" & result == "W", (VAN_table$G + 1), VAN_table$G)

t = game_clean(VAN_table, CHI_table, VAN_ADV_table, CHI_ADV_table)

names(t) = cnames

dt = rbind(dt,t)
```

<i>*editors note - 'CHI_table = CHI_table[multi_roster(CHI_table), <b>c(2,6,14)]</b>' is no longer needed since this was updated in each team's function (applies to VAN as well).</i>

As we can see, that's quite A LOT of code to be run for every single game. Especially, when there are 1,271 games played (82 games times 31 teams divided by 2 since each game involves two teams) per season. Initially, I ran through the vigorous process of running the above code for EACH game. Believe it or not, it only took 15-20 seconds per game - I pasted my code into sublime and used the 'find and replace all' functional to swap out each team's ID and the date. Still, this took me anywhere from 1-2 hours just to pull a month's worth of data. For a Data Scientist? Come on, I can do better.

# Automating the Web Scraper

Huge sigh of relief here. With the final web scraper code, I can scrape, clean, and organize all the data that I want in 1/5 of the time. To get started on this. I'll need to add some additional variables and functions to help put all of the pieces together:

```{r}
date_year = c(2019)
date_month = sprintf("%02d", c(10:12))
date_day = sprintf("%02d", c(1:31))

team_abrev = c('ANA','ARI','BOS','BUF','CAR','CBJ','CGY','CHI','COL','DAL','DET','EDM','FLA','LAK','MIN','MTL','NJD','NSH','NYI','NYR','OTT','PHI','PIT','SJS','STL','TBL','TOR','VAN','VEG','WPG','WSH')
```

The above code only represents the first half of the season (or close enough to it) for the 2018-2019 campaign. I chose to split of seasons in half for quality control - less data to manage gives me a better chance to find mistakes within in the data. Running less data in my webscraper also saves me more time when I have to debug and re-run the code. As we see, the year will represent "2018" and the months will be represented as "10" "11" and "12." The days will cycle from 1 to 31 - with the digits being 2 for each day. For example, the 1st of the month will be "01" - and this is key when running through all of the URLs. Since the home team is represented in the URL, I need to run through every single team until on every single day (and month, year, etc.) until the URL exists. More on this will be explained below.

I also will need to add 2 additional functions so that not only does the home and away team names get read, but so does their IDs:
```{r}
read_atn = function(x) {
    tx = x %>%
        html_node(xpath = '//*[@id="nav"]/div/div/a') %>%
        html_text() 
    substr(tx, start = 1, stop = 3)
    
}

read_htn = function(x) {
    tx = x %>%
        html_node(xpath = '//*[@id="nav"]/div/div/a') %>%
        html_text() 
    substr(tx, start = 8, stop = 10)
}
```

Finally, the below code will scrape the data from each game:
```{r}
for (year in date_year) {
  for (date in date_month) {
    for (day in date_day) {
        for (team in team_abrev) {
          url = paste("https://www.hockey-reference.com/boxscores/", year, date, day, "0", team, '.html', sep = '')
          if(url.exists(url) == TRUE) {
            game = read_html(url)
            dat = date_f(game)
            away_team = away_team_name(game)
            home_team = home_team_name(game)
            away_score = away_score_f(game)
            home_score = home_score_f(game)
            result = ifelse(away_score>home_score, "L", "W")
            result = as.data.frame(result)
            reg = Regulation(game)
            emp = emp_net(game)
            ateam = read_atn(game)
            hteam = read_htn(game)
            w = get(paste("GET_",hteam,sep = ''))(game)
            x = get(paste("GET_",ateam,sep = ''))(game)
            y = get(paste(hteam,"_ADV",sep = ''))(game)
            z = get(paste(ateam,"_ADV",sep = ''))(game)
            d = assign(paste(year, date, day, hteam, ateam, "_Table", sep = ''), game_clean(w,x,y,z))
          }
        }
      }
    }
  }
  ```
  
The Web Scraper is going to check every year (at least only the ones that I have specified), every month, every day, and every team until a URL gets a match. This is key since every URL on hockey-reference follows the same format for every game - only the home team ID and date changes in each URL. Once the URL exists, all of the information I need is collected and cleaned by the previous functions I set up, then to a data frame. 
<p></p>

Once this has been completed, I'll need to combine all of this data into one data frame.The step below should do the trick:
```{r}
rm(w)
rm(x)
rm(y)
rm(z)
rm(d)
rm(result)


dfs = sapply(.GlobalEnv, is.data.frame) 
dfs

dt = do.call(rbind, mget(names(dfs)[dfs]))
names(dt) = cnames
```
My dfs variable, also referring to distributed file system, checks my R enviroment and finds all of my data frames by assigning Boolean values. I can then use 'do.call' and 'mget' to retrieve and bind all of the data together. It's important to note that the variables I created with the web scraper needed to be removed first - as these were also created as data frames.

# Create and Apply Variables to the rest of the data

Now that I have my base data from the IRLS, I need to create the rest of my variables based on what I have pulled:
```
dt$Home_G = as.numeric(dt$Home_G)
dt$Home_S = as.numeric(dt$Home_S)
dt$Away_G = as.numeric(dt$Away_G)
dt$Away_S = as.numeric(dt$Away_S)
dt$Home_BS = as.numeric(dt$Home_BS)
dt$Away_BS = as.numeric(dt$Away_BS)

dt$Home_SV = (dt$Away_S - dt$Away_G)/dt$Away_S
dt$Away_SV = (dt$Home_S - dt$Home_G)/dt$Home_S
dt$Home_SH = ifelse(dt$Game_Length == "S" & dt$Result == "W",  (dt$Home_G - 1)/ dt$Home_S,dt$Home_G/dt$Home_S)
dt$Away_SH = ifelse(dt$Game_Length == "S" & dt$Result == "L",  (dt$Away_G - 1)/ dt$Away_S, dt$Away_G/dt$Away_S)
dt$Home_PDO = dt$Home_SV + dt$Home_SH
dt$Away_PDO = dt$Away_SV + dt$Away_SH

dt$Home_GA = dt$Away_G
dt$Away_GA = dt$Home_G
dt$Home_DIF = dt$Home_G - dt$Away_G
dt$Away_DIF = dt$Away_G - dt$Home_G
dt$Home_W = ifelse(dt$Result == "W", 1, 0)
dt$Away_W = ifelse(dt$Result == "L", 1, 0)
dt$Home_SA = dt$Away_S
dt$Away_SA = dt$Home_S

dt$Home_P = ifelse(dt$Result == "W", 2, ifelse(dt$Result == "L" &  dt$Game_Length == "R", 0,1))
dt$Away_P = ifelse(dt$Result == "L", 2, ifelse(dt$Result == "W" & dt$Game_Length == "R", 0,1))
```
First, I'll need to make sure that certain variables in my existing data set is the proper structure. Once I convert these variables to the numeric class, I will create a save% variable for each team. As stated previously, empty net goals don't count towards a team's save % so I wil need to make sure that any empty net goals are reduced from the goal count, as well as the shot count. I'll note that these variables are assuming that the winning team scored the empty net goals (since that is usually the case 99% of the time). Shooting % and PDO are then calcuated based on goals, shots, and save %.

I also need to track the goals against for each team. This is different than the opposing team's goal count since it will be an aggregated stat of goals against from other teams played - not the one the team played against that game. It's important to recall that predictions will be based on aggregated stats for each team prior to that game being played. With that said, I'll also track a team's differential, Wins set up as a binary variable (I will not differentiate overtime and shootout losses), shots against, and points (this will include 1 point for overtime and shootout losses).

Finally, I'll convert the date (or dat) variable to a time structure using the chron package. This allows me to order my data correctly from the earliest games, to the latest games. This will be key when I start aggregagting the data.

```
dt = separate(dt, col = 'Date', into = c('Day', 'Year', "Time"), sep = ",")

dt = unite_(dt, col = "Date", from = c("Day", "Year"), sep = "")

dt$Date = as.Date(dt$Date,format='%B %d %Y')

dt =  dt[order(dt$Date),]
```
<p></p>

# Aggregation
Now that I have all of the data I need *per game*, I will begin to aggregate all of this data by either a cumulative sum or mean. I'll note that I rbinded each season half and created a training base or test base - depending on the data.

Below is my code for aggregating together the goals scored for each team. The code will be similar for every other stat - using either "cumsum" for cumulative sum or "cummean" for cumulative mean. Cumulative mean was used for offensive zone start % and corsi - basically any variable that didn't require a cumulative sum.
```
df = read_excel("2018-2019 Training Base.xlsx", col_names = TRUE)
View(df)

df$HCS_Goals = 0
df$ACS_Goals = 0
# Set up Function
CS_Goal = function(x) {
    e = df[(df$Home==x),]
    t = df[(df$Away==x),]
    e$Team_Goal = e$Home_G
    t$Team_Goal = t$Away_G
    et = rbind(e,t)
    et =  et[order(et$Date),]
    et$Team_Goal_Sum = ave(et$Team_Goal, FUN = cumsum)
    et$Team_Goal_Sum = ave(et$Team_Goal_Sum, FUN = lag)
    et$HCS_Goals = ifelse(et$Home==x, et$Team_Goal_Sum, et$HCS_Goals)
    et$ACS_Goals = ifelse(et$Away==x, et$Team_Goal_Sum, et$ACS_Goals)
    et
    }
```
The above function is set up to find any team name in either the Home or Away column. The function then tracks the Home or Away goals associated with that team, per game, and reorders all of the data as needed. The data is then combined together, and a new variable is created that is a cumulative sum for that target variable (such as goals in this code). I then apply a lag, since I want the 2nd game of the season to represent what the team scored in the first goal of the season.

Next, I'll create a vector that represents every team's name found in either the Home or Away columns:
```
Tnames = c("Anaheim Ducks", "Arizona Coyotes", "Boston Bruins", "Buffalo Sabres", "Calgary Flames", "Carolina Hurricanes", "Chicago Blackhawks", "Colorado Avalanche", "Columbus Blue Jackets", "Dallas Stars", "Detroit Red Wings", "Edmonton Oilers", "Florida Panthers", "Los Angeles Kings", "Minnesota Wild", "Montreal Canadiens",
"Nashville Predators", "New Jersey Devils", "New York Islanders", "New York Rangers", "Ottawa Senators", "Philadelphia Flyers", "Pittsburgh Penguins", "San Jose Sharks", "St. Louis Blues", "Tampa Bay Lightning", "Toronto Maple Leafs", "Vancouver Canucks", "Vegas Golden Knights", "Washington Capitals", "Winnipeg Jets")
```

Now I'll create a loop using lapply. Since lapply only creates lists, I'll need to grab each list created, for each team, and bind them together.

```
z = lapply(Tnames, CS_Goal)

l = c(1:31)

out = NULL

for (num in l){
    a = as.data.frame(z[[num]])
    out = rbind(out,a)
    }

dt =  out[order(out$Date),]
```

Now, you may be wondering if I forgot that each game contains 2 teams - meaning that I'll likely have duplicate games when I combine this information. You would be correct. My first experience with this loop left me confused..... for a bit. Luckily, I was able to remove the duplicate rows using the ddply code below. I'll note that there were plenty of good packages to remove duplicate rows, but since each row was considered unique (since there were different aggregated goal totals for the away and home team variable), this one just happened to work best for me:

```
attach(dt)

dt = ddply(dt, .(Date, Home, Away, Result, Home_G, Home_PIM, Home_S, Away_G, Away_PIM, Away_S, Home_Corsi, Home_OFS, Home_H, Home_BS, Away_Corsi, Away_OFS, Away_H, Away_BS, Game_Length, Home_SV, Away_SV, Home_SH, Away_SH, Home_PDO, Away_PDO, Home_GA, Away_GA, Home_DIF, Away_DIF, Home_W, Away_W, Home_SA, Away_SA, Home_P, Away_P), summarize, HCS_Goals = sum(HCS_Goals), ACS_Goals = sum(ACS_Goals))

write_xlsx(dt, "C:/Users/David/Documents/MSDS 692/data\\2017-2018 Training Aggregated.xlsx")
```
What I like about this code is that it gives me freedom to get rid of columns as I don't need them. Once I had all of the stats aggregated, I removed the stats for each game since predicted games won't have access to those stats until the game is already completed. I then saved the data frame to capture the new column, and moved on to the next stat, Once this was repeated for every stat, I am ready to finalize my data.

# Finalizing the Data

Now that all of my data has been aggregated, I need to re-adjust the save %, shooting %, and PDO variables.
```
dt$HCS_SV = ((dt$HCS_SA- dt$HCS_Empty_Netters_A) - (dt$HCS_GA - dt$HCS_Empty_Netters_A))/(dt$HCS_SA- dt$HCS_Empty_Netters_A)
dt$ACS_SV = ((dt$ACS_SA- dt$ACS_Empty_Netters_A) - (dt$ACS_GA - dt$ACS_Empty_Netters_A))/(dt$ACS_SA- dt$ACS_Empty_Netters_A)

dt$HCS_SH = dt$HCS_Goals/dt$HCS_S
dt$ACS_SH = dt$ACS_Goals/dt$ACS_S

dt$HCS_PDO = dt$HCS_SV + dt$HCS_SH
dt$ACS_PDO = dt$ACS_SV + dt$ACS_SH
```

Since I essentially renamed all of my original variables to "HCS/ACS_", I named all of these columns back to the following:

```
col_names = c("Date", "Home", "Away", "Result", "Home_Goals", "Away_Goals", "Home_PIM", "Away_PIM", "Home_Shots", "Away_Shots", "Home_Corsi", "Away_Corsi", "Home_OFS", "Away_OFS", "Home_HT", "Away_HT", "Home_BS", "Away_BS", "Home_EN", "Away_EN", "Home_GA", "Away_GA", "Home_PIM_A", "Away_PIM_A", "Home_SA", "Away_SA", "Home_Corsi_A", "Away_Corsi_A", "Home_OFS_A", "Away_OFS_A", "Home_HT_A", "Away_HT_A", "Home_BS_A", "Away_BS_A", "Home_EN_A", "Away_EN_A", "Home_GP", "Away_GP", "Home_W", "Away_W", "Home_P", "Away_P", "Home_DIF", "Away_DIF", "Home_SV", "Away_SV", "Home_SH", "Away_SH", "Home_PDO", "Away_PDO")
names(dt) = col_names
```

Next, I want all of the data to be numeric aside from the Home and Away teams, and the result (this will be our prediction factor).
```
dt$Home_Corsi = as.numeric(dt$Home_Corsi)
dt$Away_Corsi = as.numeric(dt$Away_Corsi)
dt$Home_OFS = as.numeric(dt$Home_OFS)
dt$Away_OFS = as.numeric(dt$Away_OFS)

dt$Home_Corsi_A = as.numeric(dt$Home_Corsi_A)
dt$Away_Corsi_A = as.numeric(dt$Away_Corsi_A)
dt$Home_OFS_A = as.numeric(dt$Home_OFS_A)
dt$Away_OFS_A = as.numeric(dt$Away_OFS_A)

dt$Home_SV = as.numeric(dt$Home_SV)
dt$Away_SV = as.numeric(dt$Away_SV)
dt$Home_SH = as.numeric(dt$Home_SH)
dt$Away_SH = as.numeric(dt$Away_SH)
dt$Home_PDO = as.numeric(dt$Home_PDO)
dt$Away_PDO = as.numeric(dt$Away_PDO)
```
For the most part, all of the code has included some kind of data cleaning or rearranging. To put the icing on the cake, I decided to round all of my longer stat variables to only 2 or 3 decimal places.
```
dt$Home_Corsi = format(round(dt$Home_Corsi, 2), nsmall = 2)
dt$Away_Corsi = format(round(dt$Away_Corsi, 2), nsmall = 2)
dt$Home_OFS = format(round(dt$Home_OFS, 2), nsmall = 2)
dt$Away_OFS = format(round(dt$Away_OFS, 2), nsmall = 2)

dt$Home_Corsi_A = format(round(dt$Home_Corsi_A, 2), nsmall = 2)
dt$Away_Corsi_A = format(round(dt$Away_Corsi_A, 2), nsmall = 2)
dt$Home_OFS_A = format(round(dt$Home_OFS_A, 2), nsmall = 2)
dt$Away_OFS_A = format(round(dt$Away_OFS_A, 2), nsmall = 2)

dt$Home_SV = format(round(dt$Home_SV, 3), nsmall = 3)
dt$Away_SV = format(round(dt$Away_SV, 3), nsmall = 3)
dt$Home_SH = format(round(dt$Home_SH, 3), nsmall = 3)
dt$Away_SH = format(round(dt$Away_SH, 3), nsmall = 3)
dt$Home_PDO = format(round(dt$Home_PDO, 3), nsmall = 3)
dt$Away_PDO = format(round(dt$Away_PDO, 3), nsmall = 3)
```

# Analysis
The majority of my analysis will be on part 2 on this project. However, what's the point of doing all of this work without a little bit of fun? Below, I'll grab some of the variables from my test base data set (non-aggregated) and perform k-means clustering analysis. Essentially, I want to see if there are any true subgroups among my data points, or if the variables I use just appear random.

<b> Shots & Goals for Home & Away Teams </b><p></p>
Here, I want to see if there are any patterns among shots and goals for teams. Hockey is a complex sport, but there is one myth that fans and sports analysts assume in this sport - shots = more goals. While I don't disagree with this by any means, there is clearly more to an outcome of a hockey game than shots. Yes, goals ultimately decide the outcome of the game. However, more shots doesn't mean that a team is more likely to walk away with a win.

First, I'll want to find the opimtal amount of clusters for my data. Using the "within groups of sum of squares" approach, I can plot my data points and use the 'elbow method' to find the opimal amount of clusters. The below code is the function I'll use to set up the data:
```
wssplot = function(data, nc=10, seed=99) {
    wss = (nrow(data)-1)*sum(apply(data,2,var))
    for (i in 2:nc) {
        set.seed(seed)
        wss[i] = sum(kmeans(data, centers=i)$withinss)}
    plot(1:nc, wss, type = "b", xlab = "Number of Clusters",
         ylab = "Within groups of sum of squares")
    }
```

Now that my function has been set up, I'll create a new data frame with only the shots and goals column for the home and away teams. I'll note that this is a form of unsupervised learning since I removed the results column. I'll preview the first few rows of the data set so that you can get a feel from what I'm looking at:

```
 head(dt1)
# A tibble: 6 x 4
  Home_G Home_S Away_G Away_S
   <dbl>  <dbl>  <dbl>  <dbl>
1      3     22      2     33
2      5     42      3     26
3      4     35      1     22
4      2     22      3     34
5      2     29      1     33
6      1     29      2     20
```

I'll use my function on my dataset and plot the results:
```
wssplot(dt1)
```

<img src = "https://user-images.githubusercontent.com/39016197/84824480-3d578300-afdd-11ea-9b72-ce95eacbc811.png" width = 440 height = 250>

Based on the image, 2 or 3 clusters seem to be optimal - with 3 being more likely. Since I was hoping I'd be able to subgroup this into 2 clusters, I'll start with that first.

```
km = kmeans(dt1,2)
> autoplot(km, dt1,frame=TRUE)
> km$centers
    Home_G   Home_S   Away_G   Away_S
1 3.146154 36.61923 2.836538 26.96346
2 3.069395 28.02491 2.868327 34.07829

> km
K-means clustering with 2 clusters of sizes 520, 562
#omitted clustering vector due to size
Within cluster sum of squares by cluster:
[1] 28838.91 33984.76
 (between_SS / total_SS =  34.9 %)
```

<img src = "https://user-images.githubusercontent.com/39016197/84825044-10f03680-afde-11ea-9153-4831096d25ed.png" width = 480 height = 320>

Right away, I can tell that the subgroups won't be very meaningful. While the 34.9% above doesn't sell me either, the plot visual is simply too close together to really partition this data. However, on the bright side, the cluster center (means) does tell a story. In cluster one, the average data point has a home goal value of 3.14 and 36.6 home shots while the away team averages 2.83 goals and 26.96 shots. This compares nicely to 3 goals and 28 shots for the home team in cluster 2 while the away team averages 2.86 goals and 34 shots.
This tells me that the games in cluster one give the competitive edge to the Home team. Hockey has its' fair share of even games, but also it's fair share of one-sided games. In games where the home team is dominating puck and zone possession, you'll typically see a higher goal and shot count and a lower count for both stats on the away side. In contrast, if the Away team is playing better, you'll see these numbers skew in the opposite direction. This is what we mainly see in cluster 2 for shots, although there isn't a big difference in goals (which makes sense since both goal marks are around the league average).



# Challenges

There were several obstacles that presented itself and so far, most of them have been overcome. The use of functions not only cut down on the # of lines I would need for each game, but it also helped solve some of the issues I ran into early. When teams play multiple goaltenders, the number of columns in the player table is increased and shifts the coding required to grab only the TOTAL row (since I only care about the total team stats for the time being). Using ifelse and nrow(x) solved this problem. Another came with Overtime and Shootouts. Since I'm tracking points for each game, I needed a way to determine if a loser gets 1 or 0 points. By reading the box score text, I set up a function to read "OT" and "Shootout" and while this worked, I later had to modify this since most Ottawa players were represented by OTT in the box score. Shootouts were another issue since I was only pulling goals scored by players - meaning another formula had to be set up to inflate the goal # if a victory was obtained by shootout. 

Using RVest itself was not challenging but presented a learning curve. Grabbing some of the tables didn't take long to figure out, but the tables wrapped in comments were certainly more challenging to figure out. Automating this web scraper was also a challenge, but with some guidance from my professor Kellen Sorauf, I was able to get this working with how I needed it to.

Now that my web scraper is complete, my next goal is to aggregate all of the stats by time. With a few weeks left in my project deadline, I'll also try to add more data I think will be useful for the eventual Machine Learning analysis - which will be part 2 of this project.

I will admit that one random game messing up all of my code did make me laugh out loud. While I was fully confident that teams could not play with an incomplete roster, turns out one actually did. When pulling data, I realized that one of my data sets had some NA values. Upon digging, it turns out that the Anaheim Ducks played with only 5 Defensemen due to a lineup sheet mistake. Silly Anaheim, and silly me for not producing a better code initially to avoid this mistake:

<img src = "https://user-images.githubusercontent.com/39016197/83693501-6fb3ba00-a5b3-11ea-9684-dc039587557f.png" width = 580 height = 250>
<i>*The above image retreived from https://www.nhl.com/news/anaheim-ducks-edmonton-oilers-game-recap/c-306275970. </i>
<p></p>

There were a few other NAs initally too from incomplete data. Since the website simply lacked the data in the fields I pulled, I used the team average instead:
```
table(is.na(dt$Home_OFS[dt$Home=="San Jose Sharks"]))

dt[!complete.cases(dt),]

dt[is.na(dt)] = c(mean(dt$Home_OFS[dt$Home=="San Jose Sharks"], na.rm = TRUE), mean(dt$Home_OFS[dt$Home=="Florida Panthers"], na.rm = TRUE), mean(dt$Away_OFS[dt$Away=="Ottawa Senators"], na.rm = TRUE),mean(dt$Away_OFS[dt$Away=="Montreal Canadiens"], na.rm = TRUE))
```
