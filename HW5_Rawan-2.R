####################################################
##                                                ##
##      BUS32100 - HW 5                           ##
##                                                ##
####################################################

# Notes:  

# 1) You do NOT need to submit the tidy data! Submit only your code
# If we have any doubts about whether your code works, we will re-run it ourselves

# 2) Choose THREE of the four possible problems below! 
# You can try all four, but you only _need_ to do three for full credit 

# 3) The answer _can_ be that the data is tidy already
# If you'd like to claim a dataset is already tidy, 
# please provide a few sentences explaining your rationale

# $) To-do: For each of the following datasets, provide code to tidy the dataset 
# (or explanation for how it's already tidy)
# If you need a refresher on what tidy data means: https://r4ds.had.co.nz/tidy-data.html
# We also covered several examples in class

library(tidyverse)
library(readxl)

###### 1) Climate change data
# Source: https://data.world/worldbank/climate-change-data
# Don't download from source, use Canvas version as I've made edits to make it easier

climate <- read_excel("climate_change_edit.xls")
View(climate)
tidy_climate <- climate %>% 
  pivot_longer(c(`1990`, `1991`, `1992`, `1993`,`1994`, `1995`,`1996`, `1997`,`1998`, `1999`, `2000`, `2001`,`2002`, `2003`, `2004`, `2005`, `2006`, `2007`, `2008`, `2009`, `2010`, `2011`), names_to = "year", values_to = "values", names_transform = list(year=as.integer))
tidy_climate
view(tidy_climate)

########################
###### 2) City health data
# Source: https://data.world/health/big-cities-health

health <- read_csv("Big_Cities_Health_Data_Inventory.csv")
View(health)
# there are duplicates! how do we check and then get rid of duplicate rows?
which(duplicated(health))
health <- health %>% distinct() 

which(duplicated(health))
first_tidy_health <- health %>%
  separate(Place, into = c("City or County", "State"), sep = ",", convert=TRUE)
view(first_tidy_health)
second_tidy_health <- first_tidy_health %>% 
    separate(`Race/ Ethnicity`, into = c("Race", "Ethnicity"), sep = "/", convert = TRUE)
view(second_tidy_health)

new_health <- second_tidy_health %>% 
  separate(`Indicator`, into = c("indicator", "rate per population"), sep = "\\(")
view(new_health)
new_health$rate_per_population <- gsub('\\)', '',new_health$rate_per_population)
view(new_health)
########################
###### 3) Activities
# Source: https://cfss.uchicago.edu/notes/tidy-exercise/
# Note: the link above has the answer! 
# We're relying on your honesty to not just copy/paste it :)
load("~/activities.rda")

View(activities)
new_activities <- activities %>%
  pivot_longer(c(`work.T1`, `play.T1`,`talk.T1`,`work.T2`,`play.T2`,`talk.T2`), names_to = "actions", values_to = "values", names_transform = list(values=as.integer))
view(new_activities)
final_new_activities <- new_activities %>%
  separate(actions, into= c("different actions", "time"), sep = "\\.")
view(final_new_activities)


