# snmonset
#Calculates the onset of snowmelt in calendar days from Jan. 1 as the cumulative minimum departure from
#the mean flow over the period calendar days 9 to 248.
library('dplyr')

source('./Rcode/utils.r')

output <- readData() %>%
    groupByWaterYear() %>%
    includeDays(101, 340) %>% # Only observe calendar days 9 to 248
    mutate(cmd = cumsum(dmq) - mean(dmq) * row_number()) %>% # Compute necessary intermediate values
    summarize(mdq = mean(dmq), snwpulse = min(cmd), dypulse = which.min(cmd) + 7) # Produce output columns

writeOutput(output, '%5.0f    %6.2f    %6.2f       %5.0f\n', 'MRHqsnoset')
