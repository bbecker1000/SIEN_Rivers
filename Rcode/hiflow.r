# hiflow
#Calculates the low flow statistics from the time series of daily values,
library('dplyr')

source('./Rcode/utils.r')

output1 <- readData() %>%
  lagX(10) %>%
  filter(row_number() >= 31) %>%
  groupByWaterYear() %>%
  sumLag(max, m3d = which.max(lag3), m14d = which.max(lag14))

output1[1,] <- output1[1,] %>% mutate(m3d = m3d + 30, m14d = m14d + 30)

writeOutput(output1, '%7.f %8.2f %8.2f %8.2f %8.2f %8.2f %8.2f\n', 'mrhhif1')

output2 <- output1 %>%
  mutate(exp = (n() - row_number() + 1) / (n() + 1), across(m3:m14, sort)) %>%
  select(exp, m3:m14)

writeOutput(output2, ' %8.4f  %8.2f  %8.2f  %8.2f  %8.2f\n', 'mrhhif2')

writeOutput(output2 %>% genOutput3X(TRUE), ' %7.3f  %8.2f   %8.2f  %8.2f  %8.2f\n', 'mrhhif3X')
