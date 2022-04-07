# lwflow
#Calculates the low flow statistics from the time series of daily values,
library('dplyr')

source('./Rcode/utils.r')

yearly <- readData() %>% groupByWaterYear()

outputMAQ <- yearly %>% summarize(amQ = mean(dmq))
writeOutput(outputMAQ, '   %7.f     %10.2f\n', 'mrhMAQ')

avgDmq <- function(df, x) df %>% mutate(!!paste('lag', x, sep='') := (cumDMQ - lag(cumDMQ, x)) / x)
lagged <- yearly %>% mutate(cumDMQ = cumsum(dmq)) %>% avgDmq(3) %>% avgDmq(7) %>% avgDmq(14)

sumMin <- function(df, lower) {
    df %>%
        summarize(m3 = min(lag3), m7 = min(lag7), m14 = min(lag14), m7d = which.min(lag7) + lower - 2)
}

winter <- lagged %>%
    filter(mth %in% c(12, 1, 2) | (mth == 3 & day == 1)) %>%
    sumMin(62)

summer <- lagged %>%
    includeDays(273, 365) %>%
    sumMin(273)

output1D <- winter %>%
    inner_join(summer, by = 'waterYear', suffix = c('w', 's')) %>%
    select(waterYear, m3w:m14w, m3s:m14s, m7dw, m7ds)

writeOutput(output1D, '%7.f %7.3f %7.3f %7.3f %7.3f %7.3f %7.3f %7.f %7.f\n', 'mrhlwf1D')

output2D <- output1D %>%
    mutate(exp = row_number() / (n() + 1), across(m3w:m14s, sort)) %>%
    select(exp, m3w:m14s)

writeOutput(output2D, '%7.3f  %7.3f  %7.3f  %7.3f  %7.3f  %7.3f   %7.3f\n', 'mrhlwf2D')

xxpVals = c(
    0.98, 0.95, 0.90, 0.80, 0.70, 0.6, 0.50, 0.3333, 0.20, 0.1429,
    0.10, 0.0667, 0.05, 0.040, 0.0333, 0.02857, 0.025, 0.02222, 0.020
)

output3DX <- data.frame(xxp = xxpVals) %>%
    rowwise() %>%
    mutate(n = sum(xxp > output2D$exp) + 1) %>%
    mutate(Px = (xxp - output2D$exp[n]) / (output2D$exp[n - 1] - output2D$exp[n]))

for (col in c('m3w', 'm7w', 'm14w', 'm3s', 'm7s', 'm14s')) {
    vec <- output2D[[col]]
    output3DX <- output3DX %>% mutate(!!col := vec[n] + Px * (vec[n - 1] - vec[n]))
}

output3DX <- output3DX %>% select(xxp, m3w:m14s)
writeOutput(output3DX[-1,], '%7.3f  %7.3f   %7.3f  %7.3f  %7.3f  %7.3f  %7.3f\n', 'mrhlwf3DX')
