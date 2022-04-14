# fldur
#Calculates the flow duration from the time series of daily discharges
library('dplyr')

source('./Rcode/utils.r')

qsrt <- sort(readData()$dmq, decreasing = TRUE)
prbq <- 100 * 1:length(qsrt) / (length(qsrt) + 1)

pbsVals <- c(
  0.01, 0.03, 0.05, 0.1, 0.5, 1.0, 2, 3, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80,
  85, 90, 95, 97, 98, 99, 99.5, 99.9, 99.95, 99.97, 99.99
) #percent of flow

output <- data.frame(pbs = pbsVals) %>%
  rowwise() %>%
  mutate(n = sum(pbs > prbq) + 1) %>%
  mutate(qfd = qsrt[n] + (qsrt[n - 1] - qsrt[n]) * ((pbs - prbq[n]) / (prbq[n - 1] - prbq[n]))) %>%
  mutate(dqfd = qfd / mean(qsrt)) %>%
  select(-n)

writeOutput(output, '   %5.2f    %8.2f   %8.3f\n', 'MRHq_fld')
