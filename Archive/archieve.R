pacman::p_load(
  knitr,
  ggplot2,
  gt,
  ggpubr
)

source(here("Data/clean.R"))
      
# Group-Data ---------------------------------------------------------------------------------------------------


# inactive 16-24, year 2016

Percentage.1624.2016 <- Physical_activity %>%
  filter(Degree_of_activity == "active", Age == "16-24", Timepoint == as.Date("01.01.2016", format = "%d.%m.%Y"))
ggplot(Percentage.1624.2016, aes(x = Ethnicity, y = Percentage, fill = Ethnicity)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Percentage of Active People (Age 16-24) by Ethnicity",
       x = "Ethnicity", y = "Percentage") +
  theme_minimal()

# inactive, 16-24, year 2022

Percentage.1624.2022 <- Physical_activity %>%
  filter(Degree_of_activity == "active", Age == "16-24", Timepoint == as.Date("01.01.2022", format = "%d.%m.%Y"))
ggplot(Percentage.1624.2022, aes(x = Ethnicity, y = Percentage, fill = Ethnicity)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Percentage of Active People (Age 16-24) by Ethnicity",
       x = "Ethnicity", y = "Percentage") +
  theme_minimal()

# inactive, 65-74, year 2022
Percentage.6574.2022 <- Physical_activity %>%
  filter(Degree_of_activity == "inactive", Age == "65-74", Timepoint == as.Date("01.01.2022", format = "%d.%m.%Y"))
ggplot(Percentage.1624.2022, aes(x = Ethnicity, y = Percentage, fill = Ethnicity)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Percentage of Inactive People (Age 65-74) by Ethnicity",
       x = "Ethnicity", y = "Percentage") +
  theme_minimal()


# tabelle inactive2016

filtered_data_inactive <- Physical_activity %>%
  filter(Timepoint == as.Date("2016-01-01"), Degree_of_activity == "inactive")
tableinactive2016 <- pivot_wider(filtered_data_inactive, names_from = Ethnicity, values_from = Percentage)


# tabelle alles

tableall <- pivot_wider(Physical_activity, names_from = Ethnicity, values_from = Percentage)

ggtexttable(tableall)

ggtexttable(tableinactive2016)



# just to store ------------------------------------------------------------------------------------------


Physical_activity <- Physical_activity %>%
  mutate(active = degree_of_activity == "active")

Physical_activity <- Physical_activity %>%
  mutate(fairly_active = degree_of_activity == "fairly active")

Physical_activity <- Physical_activity %>%
  mutate(inactive = degree_of_activity == "inactive")




ll_by_measure <- Physical_activity
group_by(measure)
tally()

