pacman::p_load(
  rio,
  here,
  dplyr,
  readr,
  writexl,
  janitor,
  tidyverse,
  knitr,
  ggplot2,
  gt,
)
      
#load data -------------------------------------------------------------------------------------------------------


install.packages("vroom")

Physical_activity <- read_delim(here("Data/Physical activity.csv"), 
    name_repair = "universal",
    delim = ";", 
    escape_double = FALSE, 
    col_types = cols(
      measure = col_factor(levels = c("active", "fairly active", "inactive")),
      ethnicity = col_factor(levels = c("All", "Asian", "Chinese", "Black", "Mixed", "White British", "Any Other White Background", "Other")), 
      time = col_factor(levels = c("Nov 2021 to Nov 2022", "Nov 2015 to Nov 2016")),
      age = col_factor(levels = c("16-24", "25-34", "35-44", "45-54", "55-64", "65-74")),
      value = col_double()), 
    trim_ws = TRUE)

Physical_activity$value[is.na(Physical_activity$value)] <- NA



#transform data ---------------------------------------------------------------------------------------------------


assign_timepoint <- function(time_category) {
  if (time_category == "Nov 2015 to Nov 2016") {
    return(as.Date("01.01.2016", format = "%d.%m.%Y"))
  } else if (time_category == "Nov 2021 to Nov 2022") {
    return(as.Date("01.01.2022", format = "%d.%m.%Y"))
  } else {
    return(NA)
  }
}


  Physical_activity <- Physical_activity %>%
    
    mutate(timepoint = sapply(time, assign_timepoint)) %>%
    mutate(timepoint = as.Date(timepoint)) %>%
    
    select(-time) %>%
    
    rename(degree_of_activity = measure, percentage = value) %>%
    
    # mutate(
      # active = degree_of_activity == "active",
      # fairly_active = degree_of_activity == "fairly active",
      # inactive = degree_of_activity == "inactive") %>%
    
    setNames(tools::toTitleCase(names(.)))
  
  
glimpse(Physical_activity)


# Group-Data ---------------------------------------------------------------------------------------------------



# 16-24, year 2016

Percentage.1624.2016 <- Physical_activity %>%
  filter(Degree_of_activity == "active", Age == "16-24", Timepoint == as.Date("01.01.2016", format = "%d.%m.%Y"))
ggplot(Percentage.1624.2016, aes(x = Ethnicity, y = Percentage, fill = Ethnicity)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Percentage of Active People (Age 16-24) by Ethnicity",
       x = "Ethnicity", y = "Percentage") +
  theme_minimal()
  
# 16-24, year 2022

Percentage.1624.2022 <- Physical_activity %>%
  filter(Degree_of_activity == "active", Age == "16-24", Timepoint == as.Date("01.01.2022", format = "%d.%m.%Y"))
ggplot(Percentage.1624.2022, aes(x = Ethnicity, y = Percentage, fill = Ethnicity)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Percentage of Active People (Age 16-24) by Ethnicity",
       x = "Ethnicity", y = "Percentage") +
  theme_minimal()




# trying out

filtered_data <- Physical_activity %>%
  filter(Degree_of_activity == "inactive", Timepoint == as.Date("2022-01-01"))
table_data <- filtered_data %>%
  group_by(Age, Ethnicity) %>%
  summarise(Percentage = mean(Percentage, na.rm = TRUE)) %>%
  ungroup()
print(table_data)









# just to store ------------------------------------------------------------------------------------------

Physical_activity <- Physical_activity %>%
  mutate(timepoint = sapply(time, assign_timepoint))

Physical_activity <- Physical_activity %>% select(-time)

Physical_activity <- Physical_activity %>% rename(degree_of_activity = measure,
                                                  percentage = value)

Physical_activity <- Physical_activity %>%
  mutate(active = degree_of_activity == "active")

Physical_activity <- Physical_activity %>%
  mutate(fairly_active = degree_of_activity == "fairly active")

Physical_activity <- Physical_activity %>%
  mutate(inactive = degree_of_activity == "inactive")

colnames(Physical_activity)[1:8] <- tools::toTitleCase(colnames(Physical_activity)[1:8])



ll_by_measure <- Physical_activity
group_by(measure)
tally()

