library(dplyr)

data <- read.csv("wave1.teams.final.csv")

count_teams <- data %>%
	group_by(team_name) %>%
	summarise(num_domains = n_distinct(domain))

frequency <- count_teams %>%
	count(num_domains)
