readData <- function(inputFile) read_tsv(inputFile, col_names = c('mth', 'day', 'yr', 'dmq'), col_types = c('i', 'i', 'i', 'd'))

groupByWaterYear <- function(df) {
  df %>%
    mutate(waterYear = ifelse(mth >= 10, yr + 1, yr)) %>% # Assign each row to a water year (10/1-9/30)
    group_by(waterYear) # Separate the data by water year
}

includeDays <- function(df, lower = 1, upper = 366) {
  df %>% filter(row_number() >= lower & row_number() <= upper)
}

lagX <- function(df, extraLags = c()) {
  df <- df %>% mutate(cumDMQ = cumsum(dmq))
  for (x in c(3, 7, extraLags, 14)) {
    df <- df %>% mutate('lag{x}' := (cumDMQ - lag(cumDMQ, x)) / x)
  }
  df
}

sumLag <- function(df, f, ...) {
  df %>% summarize(across(starts_with('lag'), f, .names = 'm{str_sub(.col, 4)}'), ...)
}

genOutput3X <- function(df, reverse = FALSE) {
  xxpVals = c(
    0.98, 0.95, 0.90, 0.80, 0.70, 0.6, 0.50, 0.3333, 0.20, 0.1429,
    0.10, 0.0667, 0.05, 0.040, 0.0333, 0.02857, 0.025, 0.02222, 0.020
  )
  
  output3X <- data.frame(xxp = xxpVals) %>%
    rowwise() %>%
    mutate(n = sum(if (reverse) xxp < df$exp else xxp > df$exp) + 1) %>%
    mutate(Px = (xxp - df$exp[n]) / (df$exp[n - 1] - df$exp[n]))
  
  for (col in names(df)[-1]) {
    vec <- df[[col]]
    output3X <- output3X %>% mutate(!!col := vec[n] + Px * (vec[n - 1] - vec[n]))
  }
  
  output3X %>% select(-n, -Px) %>% ungroup() %>% slice(-1)
}

writeOutput <- function(df, fmt, file, dir) {
  # Transform each row of the dataframe to formatted string output
  output <- do.call(sprintf, append(fmt, as.list(df)))
  
  # Delete the previous output file if exists
  filePath <- str_glue('{dir}{file}')
  if (file.exists(filePath)) {
    file.remove(filePath)
  }
  
  # Print each row of output to the output file
  lapply(output, cat, file = filePath, append = TRUE)
}