library(rvest)
library(googleVis)

#Retrieve the NBA data for the 2007-2008 season.
#Subset the data for favorite team ------- Boston Celtics
Boston_Celtics_url <- read_html("https://www.landofbasketball.com/stats_by_team/2007_2008_celtics_pl.htm")
Boston_Celtics <- Boston_Celtics_url %>% html_nodes("table") %>% `[[`(1) %>% html_table()
#data clean
names(Boston_Celtics) <- Boston_Celtics[1,]
Boston_Celtics <- Boston_Celtics[-1,]

Boston_Celtics_pt <- Boston_Celtics_url %>% html_nodes("table") %>% `[[`(2) %>% html_table()
#data clean
names(Boston_Celtics_pt) <- Boston_Celtics_pt[1,]
Boston_Celtics_pt <- Boston_Celtics_pt[-1,]

#   Which player has the best three point percentage?
best_three_pt <- Boston_Celtics_pt[order(Boston_Celtics_pt$`3P%`),]
tail(best_three_pt, n = 1)
#   Which player has played the largest number of minutes?
largest_min <- Boston_Celtics[order(as.double(Boston_Celtics$Min)),]
tail(largest_min, n = 1)
#   Which player has the most "Steals"?
most_stl <- Boston_Celtics[order(Boston_Celtics$Stl),]
tail(most_stl, n = 1)



#Show 5 teams for the 2007-2008 season that have the most wins in descending order. 
team_url <- read_html("https://www.landofbasketball.com/yearbyyear/2007_2008_standings.htm")
team_west <- team_url %>% html_nodes("table") %>% `[[`(1) %>% html_table()
team_east <- team_url %>% html_nodes("table") %>% `[[`(2) %>% html_table()
#data clean
team_west <- team_west[c(2:6)]
names(team_west) <- team_west[1,]
team_west <- team_west[-1,]
team_east <- team_east[c(2:6)]
names(team_east) <- team_east[1,]
team_east <- team_east[-1,]
teams <- rbind(team_east, team_west)

sorted_teams <- teams[order(teams$L),]
#5 teams with most win 
sorted_teams[1:5,]


#Use at least 5 Google charts (your choice) to show relevant data from this dataset.
teams$W <- as.double(teams$W)
largest_min$Pts <- as.double(largest_min$Pts)
most_stl$Pts <- as.double(most_stl$Pts)
most_stl$Stl <- as.double(most_stl$Stl)
plot(gvisBarChart(teams, xvar = 'Team',yvar='W'))
#The score changes according to Steal and time
plot(gvisBubbleChart(most_stl, idvar="Player", xvar="Pts", yvar="Stl", 
                     colorvar = "PF", 
                     options = list(width = "Automatic",
                                    height = "Automatic")))
#The score changes according to time
plot(gvisLineChart(largest_min, xvar="Player", yvar="Pts"))
#The score changes according to Steal
plot(
  gvisScatterChart(most_stl[c(4,9)], options=list(trendlines="0"))
)


#display the location on the world map
World_Cup_url <- read_html("https://www.landofbasketball.com/world_cup_stats/medals_by_year.htm")
World_Cup <- World_Cup_url %>% html_nodes("table") %>% `[[`(1) %>% html_table()
#data clean
World_Cup <- World_Cup[c(1,3:5)]
names(World_Cup) <- World_Cup[2,]
World_Cup <- World_Cup[World_Cup$`World Cup:`!="",]
World_Cup <- World_Cup[-1,]
#World_Cup

champion <- c(World_Cup$Gold, World_Cup$Silver, World_Cup$Bronze)
champion <- replace(champion,champion=="USA",'United States')
champion <- table(champion)
champion_df <- as.data.frame(champion)
colnames(champion_df) = c('champion','number')

plot(gvisPieChart(champion_df))
plot(gvisGeoChart(champion_df, locationvar = "champion", colorvar = "number"))


