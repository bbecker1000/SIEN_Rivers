# Remove header information and relabel column names of MFSanJoaquin_Q_20220223.txt

df <- read.delim("/Users/noor/Desktop/NPS/SIEN_Rivers_RStudio/Data_Raw/MFSanJoaquin_Q_20220223.txt")

df <- df[-c(1:29),]
colnames(df) <- c('Agency CD','Site Number', 'Datetime', 'Discharge, cubic feet per second (Mean)', 'Discharge, cubic feet per second (Mean) CD')

head(df)








#colnames(df2) <- c('Agency CD','Site Number', 'Datetime', 'Temperature, water, degrees Celsius (Mean)', 'Temperature, water, degrees Celsius (Mean) CD', 'Temperature, water, degrees Celsius (Maximum)', 'Temperature, water, degrees Celsius (Maximum) CD', 'Temperature, water, degrees Celsius (Minimum)', 'Temperature, water, degrees Celsius (Minimum) CD', 'Temperature, water, degrees Celsius (Median)', 'Temperature, water, degrees Celsius (Median) CD', 'Discharge, cubic feet per second (Mean)', 'Discharge, cubic feet per second (Mean) CD')

