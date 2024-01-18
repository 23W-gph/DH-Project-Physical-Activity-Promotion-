pacman::p_load(
  rio,
  here,
  dplyr,
  lubridate
)
      
#load data -------------------------------------------------------------------------------------------------------

install.packages("vroom")

Physical_activity <- read_delim(here("Data/Physical activity.csv"), 
    name_repair = "universal",
    delim = ";", escape_double = FALSE, 
    col_types = cols(
      measure = col_factor(levels = c("active", "fairly active", "inactive")),
      ethnicity = col_factor(levels = c("All", "Asian", "Chinese", "Black", "Mixed", "White British", "Any Other White Background", "Other")), 
      time = col_factor(levels = c("Nov 2021 to Nov 2022", "Nov 2015 to Nov 2016")),
      age = col_factor(levels = c("16-24", "25-34", "35-44", "45-54", "55-64", "65-74")),
      value = col_double()), 
    trim_ws = TRUE)

Physical_activity$value[is.na(Physical_activity$value)] <- 0

#transform data ---------------------------------------------------------------------------------------------------

col_fac = c("measure","ethnicity", "time", "age")

Physical_activity <- Physical_activity %>%
  
  #clean_names()
  
  rename(degree_of_activity = measure,
         percentage = value)

  colnames(Physical_activity)[1:5] <- tools::toTitleCase(colnames(Physical_activity)[1:5])
  
  #select(-row_now)

 
  
