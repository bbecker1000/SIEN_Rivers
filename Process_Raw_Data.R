# Remove header info, relabel column names, and extract Year, Month, Day columns if datetime column found
library(dplyr)


removeHeaderFromData <- function(df) {
  cleaned_df <- df
  cleaned_df <- cleaned_df[!grepl('#', cleaned_df[,1]),] #remove '#' from rows in first col
  cleaned_df <- cleaned_df[-c(1,2),] #remove extra 2 rows with no '#'
  return(cleaned_df)
}

groupByWaterYear <- function(df) {
  df %>%
    mutate(waterYear = ifelse(Month >= 10, Year + 1, Year)) %>% # Assign each row to a water year (10/1-9/30)
    group_by(waterYear) # Separate the data by water year
}




