# fldur
#Calculates the flow duration from the time series of daily discharges
#Check file path here (relative)
MRHq <- read.table('./MATLAB/NedsFiles_sien_hydrolgy/sien_hydro_programs/MRHq.txt') #change load() to read.table()
dmq <- MRHq[,4] #define variables
mth <- MRHq[,1]
day <- MRHq[,2]
yr <- MRHq[,3]
QMA <- mean(dmq) #store statistics
sm2lq <- sort(dmq)
numq <- length(dmq)
m <- 1
qsrt <- list() 
for (n in numq:-1:1){
    qsrt[m] <- sm2lq[n] 
    m <- m + 1
}
for (m in 1:100){
    test <- qsrt[m] 
}
pbs <- list(0.01, 0.03, 0.05, 0.1, 0.5, 1.0, 2, 3, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 85, 90, 95, 97, 98, 99, 99.5, 99.9, 99.95, 99.97, 99.99) #percent of flow
prbq <- list() 
for (m in 1:numq){
    prbq[m] <- 100*(m/(numq + 1))
}
qfd <- list() #added this line
dqfd <- list() #added this line
for (l in 1:29){
    for (m in 1:numq){
        #add parenthesis around the following if statement, change parenthesis to brackets
        if ((prbq[[m]] <= pbs[[l]]) && (prbq[[m+1]]>= pbs[[l]]) ){
            qfd[l] <- qsrt[[m+1]]+(qsrt[[m]]-qsrt[[m+1]])*((pbs[[l]]-prbq[[m+1]])/(prbq[[m]]-prbq[[m+1]]))
            dqfd[l] <- qfd[[l]]/QMA
        }
    }
}
for (l in 1:29){
    cat(
        sprintf(
            '%5.2f    %8.2f   %8.3f\n',
            pbs[l], 
            qfd[l],   
            dqfd[l]
        ),
        file = './Output/MRHq_fld',
        append = TRUE
    )
}