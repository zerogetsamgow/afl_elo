library(tidyverse)
library(elo)
library(fitzRoy)
library(lubridate)
library(purrr)


# Get results data for 
results <- fitzRoy::get_match_results()
results <- results %>%filter(Season %in% c(2012,2013,2014,2015,2016,2017,2018))%>%
	mutate(seas_rnd = paste0(Season, ".", Round.Number),
				 First.Game = ifelse(Round.Number == 1, TRUE, FALSE)
	)
# Get fixture data
fixture <- fitzRoy::get_fixture()
fixture <- fixture %>%
	# filter(Date > max(results$Date)) %>%
	mutate(Date = ymd(format(Date, "%Y-%m-%d"))) %>%
	rename(Round.Number = Round)

## Create two home ground advantage measures

# Get team, venue lat and lon data ----
library(googlesheets)
# Run gs_auth() to set this up
gs <- gs_key("17041tChNHzRNYmi1nCCJvacOqbk19MJUzW8UVX91b_A")

teams <-  gs_read(gs, sheet = "AFL_data", ws = "Teams",
									locale = readr::locale(encoding = "UTF-8")) %>% 
	as.tibble() %>%
	select(team, team_lat, team_lon)


venues <- gs_read(gs, sheet = "AFL_data", ws = "Venues",
									locale = readr::locale(encoding = "UTF-8")) %>% 
	as.tibble %>%
  # Gather all venue name variants into one column
	gather(names, venue, 1:3) %>%
	filter(!is.na(venue)) %>%
	select(venue, venue_lat, venue_lon)

library(geosphere)
# Use crossing to obtain matrix product of team and venue tables
distances <- crossing(teams, venues) %>%
	           #calculate travel distance from team home city to each ground using geosphere::distHaversine
	           mutate(team_pair = pmap(list(team_lon,team_lat),c),
	           			  venue_pair = pmap(list(venue_lon,venue_lat),c),
	           	      travel=pmap_dbl(list(team_pair, venue_pair), distHaversine)/1000) %>%
	           select(team,Venue=venue,travel) %>% as.tibble()

## define travel advantage function
adv_ground_travel <- function(home_value, away_value) {((away_value - home_value)*1)^.2}

## Function to calculate experience at the ground for the current and previous season.
at_ground_exp <- function(game_date, game_season, game_venue, team) {
	results %>% filter(game_date > Date,  # only include games before the game being modelled
										 game_season %in% c(Season-1,Season), # only include games in current season
										 Home.Team==team | Away.Team==team, # include all matches that
										 game_venue==Venue) %>%
		           nrow()
}

# Function to calculate experience advantage
adv_ground_exp <- function(home_value, away_value) {log(home_value + 1) - log(away_value + 1)}

adv_alpha <- 6
adv_beta <- 15
adv_home_total <- function(travel_value, exp_value) {adv_alpha * travel_value + adv_beta * exp_value }

## Add ground advantage data to results and fixture table
results <- results %>% 
					 left_join(., distances, by=c("Home.Team"="team","Venue"="Venue")) %>%
           rename("Home.Travel"='travel') %>% 
	         left_join(., distances, by=c("Away.Team"="team","Venue"="Venue")) %>%
	         rename("Away.Travel"='travel') %>%
	         mutate(home_adv_travel = pmap_dbl(list(Home.Travel,Away.Travel),adv_ground_travel),
	         				home_exp = pmap_int(list(Date, Season, Venue, Home.Team), at_ground_exp),
	         			  away_exp = pmap_int(list(Date, Season, Venue, Away.Team), at_ground_exp),
	         			  home_adv_exp = pmap_dbl(list(home_exp,away_exp), adv_ground_exp),
	         			  home_grd_adv = pmap_dbl(list(home_adv_travel, home_adv_exp), adv_home_total))

fixture <- fixture %>% 
	left_join(., distances, by=c("Home.Team"="team","Venue"="Venue")) %>%
	rename("Home.Travel"='travel') %>% 
	left_join(., distances, by=c("Away.Team"="team","Venue"="Venue")) %>%
	rename("Away.Travel"='travel') %>%
	mutate(home_adv_travel = pmap_dbl(list(Home.Travel,Away.Travel),adv_ground_travel),
				 home_exp = pmap_int(list(Date, Season, Venue, Home.Team), at_ground_exp),
				 away_exp = pmap_int(list(Date, Season, Venue, Away.Team), at_ground_exp),
				 home_adv_exp = pmap_dbl(list(home_exp,away_exp), adv_ground_exp),
				 home_grd_adv = pmap_dbl(list(home_adv_travel, home_adv_exp), adv_home_total))


# Simple ELO
	# Set parameters
	HGA <- 0
	carryOver <- 1
	B <- 0.03
	k_val <- 50
	
	# Create margin function to ensure result is between 0 and 1
	map_margin_to_outcome <- function(margin, B) {
		1 / (1 + (exp(-B * margin)))
	}
	

# Run Simple ELO using
elo.data.simple <- elo.run(
	map_margin_to_outcome(Home.Points - Away.Points, B = B) ~
	adjust(Home.Team, HGA) +
	Away.Team +
	group(seas_rnd) ,
  #regress(First.Game, 1500, carryOver),
	k = k_val,
	initial.elos = rep(1500,18),
	data = results %>% filter(!Season==2018)
)

# Rund elo with home advantage data
elo.data.advantage <- elo.run(
		map_margin_to_outcome(Home.Points - Away.Points, B = B) ~
		adjust(Home.Team, home_grd_adv) + 
		Away.Team +
		group(seas_rnd),
	  #regress(First.Game, 1500, carryOver),
	  k = k_val,
		initial.elos = rep(1500,18),
		data = results %>% filter(!Season==2018)
)

# as.data.frame(elo.data)
# as.matrix(elo.data)
# final.elos(elo.data)


# Do predictions
fixture <- fixture %>%
	         filter(Season==2018) %>%
	         mutate(Prob.Simple = predict(elo.data.simple, newdata = fixture),
				          Prob.Advantage = predict(elo.data.advantage, newdata = fixture))


# View(fixture)

results_summary <- results %>%
	                 select(Date, Home.Team, Margin)

predictions <- left_join(fixture, results_summary, by=c("Date"="Date","Home.Team"="Home.Team" ))%>%
	mutate(pick.simple=if_else(Prob.Simple>0.5,1,-1),
				 pick.advantage=if_else(Prob.Advantage>0.5,1,-1)) # %>%
#	mutate(correct.sim=pick*Margin)%>%
#	mutate(right_pick=if_else(correct>0,1,0))%>%
#	mutate(accuracy=sum(right_pick, na.rm = TRUE))%>%View()
