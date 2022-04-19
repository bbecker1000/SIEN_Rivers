library(ggplot2)
library("xlsx")
library("scales")

df <- read.xlsx("/Users/noor/Desktop/NPS/MFSanJoaquin_Q_20220223 copy.xlsx", sheetIndex=1)

#mean daily discharge - time series - complete period of record
ggplot(df, aes(x=datetime, y=X9329_00060_00003, group=1)) +
  geom_line() + 
  theme_classic() +
  ggtitle("Mean Daily Discharge") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Time") +
  ylab("Discharge, cubic feet per second (Mean)")

#mean daily discharge - time series - most recent water year 
ggplot(df, aes(x=datetime, y=X9329_00060_00003, group=1)) +
  geom_line() +
  scale_x_date(limits = as.Date(c("2020-10-01", "2021-09-30"))) + 
  ylim(NA, 375) + 
  theme_classic() +
  ggtitle("Mean Daily Discharge (WY2021)") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Time") +
  ylab("Discharge, cubic feet per second (Mean)")

df2 <- read.table("/Users/noor/Desktop/NPS/SIEN_Rivers_RStudio/Output/MRHq_snmlt")

# mean annual discharge boxplot
yr_subset <- subset(df2, V1 >= 2005)
ggplot(yr_subset, aes(x=V1, y=V3, group=V1)) + 
  geom_boxplot() + 
  theme_classic() +
  ggtitle("Mean Annual Discharge Boxplot") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Time") +
  ylab("Discharge, cubic feet per year (Mean)")
#appears to be only a flat line because there is only 1 
#discharge value per every year (each row)

# annual total discharge
#need to sum for each year, then plot those values (one per yr)
ft_cubed_per_yr = df$X9329_00060_00003*31536000 #num secs per yr
ggplot(df, aes(x=datetime, y=ft_cubed_per_yr, group=1)) +
  geom_line() + 
  theme_classic() +
  ggtitle("Annual Total Discharge") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Year") +
  ylab("Discharge, cubic feet per year")


#MRHq_snmlt columns:
# year, num data points, mean annual discharge (cfs), time to center of mass (days), percent days to cumulative Q equal to 0.98 of annual runoff
#for Row 6, convert from seconds to year (ft^3/yr)
#keep comparing MATLAB/rcode
#keep cleaning up code
#pull in and cleanup the two real datasets -- write a program
#that cleans up and deletes the text from the top and adds
#the column names
#convert for loops to functions
#do plots for rows 4-6