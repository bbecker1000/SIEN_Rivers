# snmonset
#Calculates the onset of snowmelt in calendar days from Jan. 1 as the cumulative minimum departure from
#the mean flow over the period calendar days 9 to 248.
MRHq <- read.table('./Rcode/MRHq.txt', sep = '\t')
dmq <- MRHq[, 4]
mth <- MRHq[, 1]
day <- MRHq[, 2]
yr <- MRHq[, 3]
#recdyr = input('Years of Record');
recdyr <- 94
sumq = numeric(recdyr)
smq = numeric(recdyr)
swpulse = numeric(recdyr)
cmd = numeric(recdyr)
dypulse = numeric(recdyr)
mindys <- 1
maxdys <- 365
tma <- mindys + 100
tmb <- mindys + 339
#Calculate discharge for period from day 100 to day 339 in water year
for (n in 1:recdyr) {
    for (m in tma:tmb) {
        sumq[n] <- sumq[n] + dmq[m]
        if ((mth[m] == 2) && (day[m] == 29)) {
            maxdys <- maxdys + 1
        }
    }
    wyr[n] <- yr[maxdys]
    mdq[n] <- sumq[n] / 240
    for (m in tma:tmb) {
        cmd[n] <- cmd[n] + (dmq[m] - mdq[n])
        if (cmd[n] < swpulse[n]) {
            swpulse[n] <- cmd[n]
            dypulse[n]  <- (m - tma) + 8
        }
    }
    mindys <- maxdys + 1
    maxdys <- maxdys + 365
    tma <- mindys + 100
    tmb <- mindys + 339
}
n <- 1
while (n <= recdyr) {
    cat(
        sprintf('%5.0f    %6.2f    %6.2f       %5.0f\n', wyr[n], mdq[n], swpulse[n], dypulse[n]),
        file = './Output/MRHqsnoset',
        append = TRUE
    )
    n <- n + 1
}
