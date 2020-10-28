## contains constants for raking

total_population = 325.7e6

gender_population = list(Female=0.508, Male=0.492, Other=0.006)

# https://www.census.gov/prod/cen2010/briefs/c2010br-03.pdf
age_group_population = list(
  '18-24'=30.67e6,
  '25-34'=42.9e6,
  '35-44'=41.06e6,
  '45-54'=45e6,
  '55-64'=19.664e6 + 16.817e6,
  '65+'=40.26e6
)

state_pop_df = read_csv(
  'data/state_pop.csv'
)
#' Suggested Citation:
#'   Table 1. Annual Estimates of the Resident Population for the United States, Regions, States, and Puerto Rico: April 1, 2010 to July 1, 2019 (NST-EST2019-01)
#' Source: U.S. Census Bureau, Population Division
#' Release Date: December 2019
state_populations = as.list(state_pop_df$Pop_2019)
names(state_populations)=gsub('\\.','', state_pop_df$State)

state_populations[['Other / Not Applicable']] = 100000

demographic_stats = list(
  State=state_populations, #reorder_list(state_populations,candy_df$State),
  AgeGroup=age_group_population,
  Gender=gender_population
)

electoral_votes_df = read_csv('data/electoral_votes.csv')

house_votes_df = electoral_votes_df %>%
  mutate(Votes=Votes-2) %>%
  filter(State != 'District of Columbia')

# candy colors

candy_colors = c(
  "Reese's Peanut Butter Cups"= '#ff4f00',
  "Air Heads"='#e4e7ff',
  "Almond Joy"="#0f36ff",
  "Butterfingers"="#fff103",
  "Candy Corn"="#fff1be",
  "Hershey's Chocolate Bar"="#785131",
  "Milky Way"="#ac8e68",
  "100 Grand"="#FF1111", # might change this one
  "Skittles"="#b625f0",
  "Sour Patch Kids"="#646365",
  "Hot Tamales"="#a30504",
  "Lifesavers"="#72e88e",
  "M&Ms"="#381d1f",
  "Starburst"="#feb7c0",
  "Kit-Kat Bar"="#eac5a2",
  "Gummy Worms"="#bde8ce",
  "Cotton Candy"="#fc01b2",
  "Gummy Bears"="#00b101",
  "Jolly Ranchers"="#00588d",
  "Hershey's Kisses"="#6f6358",
  "Crunch Bar"="#ebe8ce",
  "Blow Pops"="#fc78ad",
  "Cow Tails"="#FFFFFF",
  "Lemonheads"="#fcf36d",
  "Junior Mints"="#004503",
  "Peanut M&Ms"="#1a0d03",
  "Snickers"="#3c0d03",
  "Twix"="#825a74"
) %>%
  toupper() %>%
  sort()

candy_colors = c(candy_colors, c('Tied'='#FFFFFF'))
