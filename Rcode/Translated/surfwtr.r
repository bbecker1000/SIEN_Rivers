# surfwtr
#Calculates the mean annual flow, time to center of mass, and
#days to cumulative Q equal to 0.98 of annual runoff
MRHq <- read.table('./Rcode/MRHq.txt', sep = '\t')
dmq <- MRHq[, 4]
mth <- MRHq[, 1]
day <- MRHq[, 2]
yr <- MRHq[, 3]
#recdyr = input('Years of Record');
recdyr <- 94
sumq = numeric(recdyr)
smq = numeric(recdyr)
for (n in 1:recdyr) {
    sumq[n] <- 0.0
    smq[n] <- 0.0
}
mindys <- 1
maxdys <- 365

qt = numeric(recdyr)
wyr = numeric(recdyr)
dur = numeric(recdyr)
mdq = numeric(recdyr)
cmt = numeric(recdyr)
frsmq = numeric(recdyr)

for (n in 1:recdyr) {
    qt[n] <- 0.0
    for (m in mindys:maxdys) {
        sumq[n] <- sumq[n] + dmq[m]
        if ((mth[m] == 2) && (day[m] == 29)) {
            maxdys <- maxdys + 1
        }
    }
    wyr[n] <- yr[maxdys]
    dur[n] <- maxdys - mindys + 1
    mdq[n] <- sumq[n] / dur[n]
    for (m in mindys:maxdys) {
        nct <- (m - mindys) + 1
        qt[n] <- qt[n] + (nct * dmq[m])
        if ((mth[m] > 3) && (mth[m] < 8)) {
            smq[n] <- smq[n] + dmq[m]
        }
    }
    cmt[n] <- qt[n] / sumq[n]
    frsmq[n] <- smq[n] / sumq[n]
    mindys <- maxdys + 1
    maxdys <- maxdys + 365
}
n <- 1
while (n <= recdyr) {
    cat(
        sprintf(
            '%5.0f    %6.2f    %6.2f       %6.2f       %6.5f\n',
            wyr[n],
            dur[n],
            mdq[n],
            cmt[n],
            frsmq[n]
        ),
        file = './Output/MRHq_snmlt',
        append = TRUE
    )
    n <- n + 1
}
