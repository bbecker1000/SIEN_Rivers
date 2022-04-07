# surfwtr
#Calculates the mean annual flow, time to center of mass, and
#days to cumulative Q equal to 0.98 of annual runoff
library('dplyr')

source('./Rcode/utils.r')

output <- readData() %>%
    groupByWaterYear() %>%
    mutate(qt = row_number() * dmq, smq = ifelse(mth > 3 & mth < 8, dmq, 0)) %>% # Compute necessary intermediate values
    # Following line reproduces bugged MATLAB code by counting Sep. 30 as 0 for leap years
    # mutate(dmq = ifelse(n() == 366 & mth == 9 & day == 30, 0, dmq)) %>% # Comment this line for correctness
    summarize(dur = n(), mdq = mean(dmq), cmt = sum(qt) / sum(dmq), frsmq = sum(smq) / sum(dmq)) # Produce output columns

writeOutput(output, '%5.0f    %6.2f    %6.2f       %6.2f       %6.5f\n', 'MRHq_snmlt')
