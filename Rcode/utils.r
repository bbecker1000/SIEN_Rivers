readData <- function() {
  MRHq <- read.table('./Rcode/MRHq.txt', sep = '\t')
  colnames(MRHq) = c('mth', 'day', 'yr', 'dmq')
  MRHq
}

groupByWaterYear <- function(df) {
  df %>%
    mutate(waterYear = ifelse(mth >= 10, yr + 1, yr)) %>% # Assign each row to a water year (10/1-9/30)
    group_by(waterYear) # Separate the data by water year
}

includeDays <- function(df, lower = 1, upper = 366) {
  df %>% filter(row_number() >= lower & row_number() <= upper)
}

writeOutput <- function(df, fmt, file) {
  # Transform each row of the dataframe to formatted string output
  output <- do.call(sprintf, append(fmt, as.list(df)))
  
  # Delete the previous output file if exists
  filePath <- paste('./Output/', file, sep = '')
  if (file.exists(filePath)) {
    file.remove(filePath)
  }
  
  # Print each row of output to the output file
  lapply(output, cat, file = filePath, append = TRUE)
}