# Empty environment 
rm(list=ls())

# Load packages
library(tidyverse)
library(dplyr)
library(readr)

#---------------------------------------------
# Scrape and Import data 
#---------------------------------------------

# Scraping data from FBref 2023/24 Championship season including the following stats: [player_details], [summary], [passing], [defense], [possession], [misc]
library(devtools) 
devtools::install_github("JaseZiv/worldfootballR") # Installing worldfootballR
library(worldfootballR) 

# Load season stats for all players in Championship 2023/24 season 
league_shooting <- fb_league_stats(country = "ENG", gender = "M", season_end_year = 2024, tier = "2nd", stat_type = "shooting", team_or_player = "player")
league_standard <- fb_league_stats(country = "ENG", gender = "M", season_end_year = 2024, tier = "2nd", stat_type = "standard", team_or_player = "player")
league_gca <- fb_league_stats(country = "ENG", gender = "M", season_end_year = 2024, tier = "2nd", stat_type = "gca", team_or_player = "player")
league_defense <- fb_league_stats(country = "ENG", gender = "M", season_end_year = 2024, tier = "2nd", stat_type = "defense", team_or_player = "player")
league_misc <- fb_league_stats(country = "ENG", gender = "M", season_end_year = 2024, tier = "2nd", stat_type = "misc", team_or_player = "player")

# Combine data sets
Combined2024 <- league_standard %>%
  full_join(league_shooting, by = "Player") %>%
  full_join(league_misc, by = "Player") %>%
  full_join(league_gca, by = "Player") %>%
  full_join(league_defense, by = "Player")

# Remove datasets no longer needed
rm(league_defense, league_gca, league_misc, league_shooting, league_standard)


#---------------------------------------------
# Load data 
#---------------------------------------------

# Save data so I don't have to continuously scrape
write.csv(Combined2024, file="Combined2024.csv", row.names =FALSE)
# Load data 
Combined2024 <- read_csv("Combined2024.csv")


#---------------------------------------------
# Tidy data 
#---------------------------------------------

# Remove columns ending in .y , .y.y , .y.y.y or .y.y.y.y as these are duplicate columns
Combined2024 <- Combined2024 %>%
  select(-which(grepl("\\.y(\\.y)*$", colnames(Combined2024))))
# Remove columns ending in .x.x , .x.x.x or .x.x.x.x as these are duplicate columns
Combined2024 <- Combined2024 %>%
  select(-which(grepl("\\.x(\\.x)+$", colnames(Combined2024))))

# Select stats needed for viz
Combined2024 <- Combined2024 %>%
  select(Player, Pos, Squad, Age, `MP_Playing Time`, `Min_Playing Time`, `Starts_Playing Time`, PrgC_Progression, PrgP_Progression, Gls, Ast, xG_Expected.x, xAG_Expected, G_minus_xG_Expected, Int.x, `Won_percent_Aerial Duels`, GCA_GCA, Tkl_Tackles, TklW_Tackles, Blocks_Blocks, Clr, Err)

# Split player position into primary and secondary 
Combined2024 <- separate(Combined2024, Pos,
                         into = c("Primary Position", "Secondary Position"),
                         sep = ",", fill="right", extra="drop")

# Removing duplicates 
Combined2024 <- Combined2024[!duplicated(Combined2024$Player), ]

#---------------------------------------------
# Data Preparation 
#---------------------------------------------

# Check data structure
str(Combined2024)

# Change variable types 
Stats2024 <- Combined2024 %>%
  mutate(
    Player = as.factor(Player),
    `Primary Position` = as.factor(`Primary Position`),
    `Secondary Position` = as.factor(`Secondary Position`),
    Squad = as.factor(Squad)
  )

summary(Stats2024)

# Calculate per 90 stats for fair comparison 
Stats2024 <- Stats2024 %>%
  mutate(
    games_90 = `Min_Playing Time` / 90,
    prgc_per_90 = PrgC_Progression / games_90,
    prgp_per_90 = PrgP_Progression / games_90,
    goals_per_90 = Gls / games_90,
    assists_per_90 = Ast / games_90,
    xg_per_90 = xG_Expected.x / games_90,
    xag_per_90 = xAG_Expected / games_90,
    g_minus_xg_per_90 = G_minus_xG_Expected / games_90,
    int_per_90 = Int.x / games_90,
    gca_per_90 = GCA_GCA / games_90,
    tackles_per_90 = Tkl_Tackles / games_90,
    tackles_won_per_90 = TklW_Tackles / games_90,
    percentage_tackles_won = Stats2024$TklW_Tackles / Stats2024$Tkl_Tackles * 100,
    blocks_per_90 = Blocks_Blocks / games_90, 
    clearances_per_90 = Clr / games_90, 
    errors_per_90 = Err / games_90
  )

# Replace 'NaN' values that occur from percentage tackles won calculation with '0' 
Stats2024 <- Stats2024 %>%
  mutate(percentage_tackles_won = ifelse(is.nan(percentage_tackles_won), 0, percentage_tackles_won)) 

# Removing players that haven't played enough game time (less than 50% the season)
Stats2024 <- Stats2024[Stats2024$`MP_Playing Time` >= 23, ]

# Select stats needed for viz
Final2024 <- Stats2024 %>%
  select(Player, `Primary Position`, Squad, Age, `Won_percent_Aerial Duels`, games_90, prgc_per_90, prgp_per_90, goals_per_90, assists_per_90, xg_per_90, xag_per_90, g_minus_xg_per_90, int_per_90,gca_per_90, tackles_won_per_90, percentage_tackles_won, blocks_per_90, errors_per_90, clearances_per_90)

# Check for missing data 
missing_data <- is.na(Final2024)
total_missing_values <- sum(missing_data) # Total amount of missing data
print(total_missing_values)
missing_values_per_variable <- colSums(missing_data) # Missing values per variable 
print(missing_values_per_variable)

# Missing data in secondary position (which can be ignored) and one in age so manually inputting this player's age
Final2024[329, "Age"] <- 23

# Check where GK present
levels(Final2024$`Primary Position`) 
# Remove goalkeepers as not relevant for my viz
Final2024 <- Final2024 %>%
  filter(`Primary Position` !="GK") 

write.csv(Final2024, file = "Final2024.csv", row.names = FALSE)


#---------------------------------------------
# Normalisation and Weighting
#---------------------------------------------

# Normalise data across full dataset
ColumnsToNormalize <- c("games_90", "Won_percent_Aerial Duels", "prgc_per_90", "prgp_per_90", "goals_per_90", "assists_per_90", "xg_per_90", "xag_per_90", "g_minus_xg_per_90", "int_per_90", "gca_per_90", "tackles_won_per_90", "percentage_tackles_won", "blocks_per_90", "errors_per_90", "clearances_per_90")

# Compute the maximum values for the specified columns. To normalise to 1 we will need to know the max value present for each KPI in the full dataset, this will be listed in a vector called max_values.
MaxValues <- apply(Final2024[,ColumnsToNormalize], 2, max,na.rm=TRUE)

# Normalize the specified columns (column values / MaxValues)
NormalizedColumnsDF <- sweep(Final2024[, ColumnsToNormalize], 2, MaxValues, "/") 
colnames(NormalizedColumnsDF) <- paste('Norm', colnames(NormalizedColumnsDF), sep = '_')

# Add the normalized columns back to the filtered data frame
Final2024 <- cbind(Final2024, NormalizedColumnsDF)


# Weighting scores for attacking impact
weightingA <- data.frame(Norm_gca_per_90 = 0.1,
                        Norm_goals_per_90 = 0.25,
                        Norm_assists_per_90 = 0.15,
                        Norm_xg_per_90 = 0.15,
                        Norm_xag_per_90 = 0.1,
                        Norm_g_minus_xg_per_90 = 0.1,
                        Norm_prgp_per_90 = 0.05,
                        Norm_prgc_per_90 = 0.05, 
                        Norm_games_90 = 0.05)

Final2024 <- Final2024 %>%
  mutate(GCA_weightA = Norm_gca_per_90 * weightingA$Norm_gca_per_90[1],
         Gls_weightA = Norm_goals_per_90 * weightingA$Norm_goals_per_90[1],
         Ast_weightA = Norm_assists_per_90 * weightingA$Norm_assists_per_90[1],
         xG_weightA = Norm_xg_per_90 * weightingA$Norm_xg_per_90[1],
         xAG_weightA = Norm_xag_per_90 * weightingA$Norm_xag_per_90[1],
         GminusxG_weightA = Norm_g_minus_xg_per_90 * weightingA$Norm_g_minus_xg_per_90[1],
         PrgP_weightA = Norm_prgp_per_90 * weightingA$Norm_prgp_per_90[1],
         PrgC_weightA = Norm_prgc_per_90 * weightingA$Norm_prgc_per_90[1], 
         Games_weightA = Norm_games_90 * weightingA$Norm_games_90[1]) %>%
  mutate(total_scoreA = rowSums(select(.,ends_with("_weightA"))))

Final2024 <- Final2024 %>%
  arrange(desc(total_scoreA))

# Weighting scores for defensive impact 
weightingD <- data.frame(`Norm_Won_percent_Aerial Duels` = 0.1,
                          Norm_tackles_won_per_90 = 0.25,
                          Norm_percentage_tackles_won = 0.2,
                          Norm_blocks_per_90 = 0.15,
                         Norm_clearances_per_90 = 0.2,
                          Norm_games_90 = 0.1)

Final2024 <- Final2024 %>%
  mutate(AerialWp_weightD = `Norm_Won_percent_Aerial Duels` * weightingD$Norm_Won_percent_Aerial.Duels[1],
         TklW_weightD = Norm_tackles_won_per_90 * weightingD$Norm_tackles_won_per_90[1],
         TklWp_weightD = Norm_percentage_tackles_won * weightingD$Norm_percentage_tackles_won[1],
         Blocks_weightD = Norm_blocks_per_90 * weightingD$Norm_blocks_per_90[1],
         Clearances_weightD = Norm_clearances_per_90 * weightingD$Norm_clearances_per_90[1],
         Games_weightD = Norm_games_90 * weightingD$Norm_games_90[1]) %>%
  mutate(total_scoreD = rowSums(select(.,ends_with("_weightD"))))

Final2024 <- Final2024 %>%
  arrange(desc(total_scoreD))

# Clean environment 
rm(Combined2024, Stats2024, NormalizedColumnsDF, missing_data, weightingA, weightingD)


# Save data
write.csv(Final2024, file = "TableauData1.csv", row.names = FALSE)

#---------------------------------------------
# Adding new players who are league average for MF/FW and MF/DF
#---------------------------------------------

# Calculate positional averages
avg_fw <- Final2024 %>%
  filter(`Primary Position` == "FW") %>%
  summarize(across(where(is.numeric), mean)) %>%
  mutate(Player = "Average FW", `Primary Position` = "FW")

avg_mf <- Final2024 %>%
  filter(`Primary Position` == "MF") %>%
  summarize(across(where(is.numeric), mean)) %>%
  mutate(Player = "Average MF", `Primary Position` = "MF")

avg_df <- Final2024 %>%
  filter(`Primary Position` == "DF") %>%
  summarize(across(where(is.numeric), mean)) %>%
  mutate(Player = "Average DF", `Primary Position` = "DF")

# Combine with original data
Final2024 <- bind_rows(Final2024, avg_fw, avg_mf, avg_df)


# Save data
write.csv(Final2024, file = "TableauDataFinal.csv", row.names = FALSE)

Final <- read.csv("TableauDataFinal.csv")
