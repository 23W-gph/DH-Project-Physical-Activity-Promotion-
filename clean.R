pacman::p_load(
  rio,
  here,
  dplyr,
  readr,
  janitor,
  tidyverse,
  vroom
)
  

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
  




  
  