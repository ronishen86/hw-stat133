# title: Data Preparation
# description: 
# input: data file 'nba2018.csv'
# output: 

# packages
library(dplyr)

# loading data
dat <-  read_csv("../data/nba2018.csv")

# experience to int
dat$experience[dat$experience == "R"] <- 0
dat$experience <- as.integer(dat$experience)

# salary units to million 
dat$salary <- dat$salary/1000000

# factor position and more descriptive labels
dat$position <-  factor(dat$position, levels = c("C", "PF", "PG", "SF", "SG"), 
                        labels = c("center", "power_fwd", "point_guard", "small_fwd", "shoot_guard"))

# adding variables to dat
dat <- mutate(dat, missed_fg = field_goals_atts - field_goals, missed_ft = points1_atts - points1, 
       rebounds = off_rebounds + def_rebounds, efficiency = (points + rebounds + assists + steals + blocks
                                                             - missed_fg - missed_ft - turnovers) / games)
# exporting with sink()
sink("../output/efficiency-summary.txt")
summary(dat$efficiency)
sink()

# creating teams data frame
teams <- dat %>%
  group_by(team) %>%
  select(team, experience, salary, points3, points2, points1, points, off_rebounds, def_rebounds, assists, 
         steals, blocks, turnovers, fouls, efficiency) %>%
  summarise(
    experience = sum(experience),
    salary = round(sum(salary), 2),
    points3 = sum(points3),
    points2 = sum(points2),
    points1 = sum(points1),
    points = sum(points),
    off_rebounds = sum(off_rebounds),
    def_rebounds = sum(def_rebounds),
    assists = sum(assists),
    steals = sum(steals),
    blocks = sum(blocks),
    turnovers = sum(turnovers),
    fouls = sum(fouls),
    efficiency = sum(efficiency),
  )

# exporting txt summary of teams 
sink("../output/teams-summary.txt")
summary(teams)
sink()

# exporting teams.csv file
write.csv(teams, file = "../data/teams.csv")



  