nba_cleaned <- read_csv("./NBA_cleaned.csv")
nba_cleaned %>%
  select(Player) %>%
  group_by(Player) %>%
  summarise(Total_player = n())

date_counter <- nba_cleaned %>%
  mutate(new_date = as.Date(Date, 'Apr 14, 1985',format='%b %d, %Y'),
         Week = format(new_date, format = "%W")) %>%
  group_by(Week) %>%
  summarise(Week_count = n())

nba_cleaned %>%
  select(Team) %>%
  group_by(Team) %>%
  summarise(Team_count = n()) %>%
  arrange(desc(Team_count))

nba_cleaned %>%
  select(Player) %>%
  group_by(Player) %>%
  summarise(Player_count = n()) %>%
  arrange(desc(Player_count))

nba_cleaned %>%
  select(Height) %>%
  summarise(height_avg = mean(Height))

nba_cleaned %>%
  select(Weight, Conference) %>%
  group_by(Conference) %>%
  summarise(median_Weight = median(Weight))