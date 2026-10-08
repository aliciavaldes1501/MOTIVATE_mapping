# Read initial datasets

# With info on EUNIS 2
initial_dataset <- read_csv(here("..", "MOTIVATE_validation", "data", "clean",
                                 "ReSurvey_QRST_data_V2025.2_20261007.csv"))

# Without info on EUNIS 2
initial_dataset_validation <- read_csv(
  here("..", "MOTIVATE_validation", "data", "clean",
       "initial_dataset_validation.csv"))

# Check that numbers of observations and plots are the same
nrow(initial_dataset) == nrow(initial_dataset_validation)
nrow(initial_dataset %>% distinct(plotID_AV)) ==  nrow(initial_dataset_validation %>% distinct(plotID))

# Get EUNIS 2 for first and last year
eunis2_first_last <- initial_dataset %>%
  arrange(plotID_AV, year) %>%
  group_by(plotID_AV) %>%
  summarise(
    first_year  = min(year, na.rm = TRUE),
    last_year   = max(year, na.rm = TRUE),
    EUNIS2_first = EUNIS2[which.min(year)],
    EUNIS2_last  = EUNIS2[which.max(year)],
    .groups = "drop"
  )

# Add info on EUNIS 2 to initial_dataset_validation
initial_dataset_validation <- initial_dataset_validation %>%
  # Keep only the first and last years for each plotID_AV
  filter(year == first_year | year == last_year) %>%
  left_join(eunis2_first_last,
            by = c("plotID" = "plotID_AV", 
                   "first_year" = "first_year", "last_year" = "last_year"))

# Define EUNIS2
initial_dataset_validation <- initial_dataset_validation %>%
  mutate(EUNIS2 = case_when(
    EUNIS2_first == EUNIS2_last ~ EUNIS2_first,
    EUNIS2_first != EUNIS2_last ~ "Change",
    is.na(EUNIS2_first) & !is.na(EUNIS2_last) ~ EUNIS2_last,
    !is.na(EUNIS2_first) & is.na(EUNIS2_last) ~ EUNIS2_first,
    TRUE ~ NA_character_
  ))

# How many validated plots with EUNIS2 = R4?
valid_obs_R4 <- initial_dataset_validation %>%
  filter(valid_plot == TRUE & EUNIS2 == "R4")
valid_obs_R4 %>% distinct(plotID) %>% nrow() # 65 plots

# How many validated plots with EUNIS2 = R4 and with more than one observation?
valid_obs_R4 %>% count(plotID) %>% filter(n > 1) %>% nrow() # 15 plots

