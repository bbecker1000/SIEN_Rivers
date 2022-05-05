library(ggplot2)
library("scales")
library(lubridate)
library(dplyr)
library(tidyverse)
source("Process_Raw_Data.R")

sanJoaquin_df <- read.table("./Data_Raw/MFSanJoaquin_Q_20220223.txt", sep = '\t')

colnames(sanJoaquin_df) <- c('Agency_CD','Site_Number', 'Datetime', 'Discharge_ft3_per_sec_mean', 'Discharge_ft3_per_sec_mean_CD')

cleaned_sanJoaquin_df <- removeHeaderFromData(sanJoaquin_df)
cleaned_sanJoaquin_df$Datetime <- as.Date(cleaned_sanJoaquin_df$Datetime, "%m/%d/%Y")
cleaned_sanJoaquin_df$Site_Number <- as.numeric(cleaned_sanJoaquin_df$Site_Number)
cleaned_sanJoaquin_df$Discharge_ft3_per_sec_mean <- as.numeric(cleaned_sanJoaquin_df$Discharge_ft3_per_sec_mean)

#extract year, month, day
cleaned_sanJoaquin_df$Year <- as.numeric(format(as.Date(cleaned_sanJoaquin_df$Datetime, format="%m/%d/%Y"),"%Y"))
cleaned_sanJoaquin_df$Month <- as.numeric(format(as.Date(cleaned_sanJoaquin_df$Datetime, format="%m/%d/%Y"),"%m"))
cleaned_sanJoaquin_df$Day <- as.numeric(format(as.Date(cleaned_sanJoaquin_df$Datetime, format="%m/%d/%Y"),"%d"))

grouped_sanJoaquin_df <- groupByWaterYear(cleaned_sanJoaquin_df)


#mean daily discharge - time series - complete period of record

ggplot(grouped_sanJoaquin_df, aes(x=Datetime, y=Discharge_ft3_per_sec_mean, group=1)) +
  geom_line() + 
  theme_classic() +
  ggtitle("Mean Daily Discharge") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Time") +
  ylab("Discharge, cubic feet per second (Mean)")


#mean daily discharge - time series - most recent water year 

recent_wtr_yr_df <- subset(grouped_sanJoaquin_df, grouped_sanJoaquin_df$waterYear == 21)
ggplot(recent_wtr_yr_df, aes(x=Datetime, y=Discharge_ft3_per_sec_mean, group=1)) +
  geom_line() +
  ylim(NA, 375) + 
  theme_classic() +
  ggtitle("Mean Daily Discharge (WY2021)") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Time") +
  ylab("Discharge, cubic feet per second (Mean)")


# mean annual discharge boxplot
ggplot(grouped_sanJoaquin_df, aes(x=as.character(waterYear), y=Discharge_ft3_per_sec_mean)) + 
  geom_boxplot() + 
  theme_classic() +
  ggtitle("Mean Annual Discharge Boxplot") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Water Year") +
  ylab("Discharge, cubic feet per year (Mean)")


# annual total discharge time series
summary2 <- summarize(grouped_sanJoaquin_df, Value = sum(Discharge_ft3_per_sec_mean*31536000)) #num secs per yr
ggplot(summary2, aes(x=waterYear+2000, y=Value)) +
  geom_line(linetype = "dashed") +
  geom_point() +
  theme_classic() +
  ggtitle("Annual Total Discharge") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Water Year") +
  ylab("Discharge, cubic feet per year")


# AMJJ/Annual time series

AMJJ_df <- grouped_sanJoaquin_df[is.element(grouped_sanJoaquin_df$Month,c(4:7)),]
AMJJ_summary <- summarize(AMJJ_df, AMJJ_Value = sum(Discharge_ft3_per_sec_mean))
annual_summary <- summarize(grouped_sanJoaquin_df, Annual_Value = sum(Discharge_ft3_per_sec_mean))
statistic <- AMJJ_summary$AMJJ_Value / annual_summary$Annual_Value * 100
ggplot(annual_summary, aes(x=waterYear+2000, y=statistic)) +
  geom_line(linetype = "dashed") + 
  geom_point() +
  theme_classic() +
  ggtitle("AMJJ Percent of Annual Flow") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Water Year") +
  ylab("AMJJ Percent (%)")


# Center Timing time series (already in water year)
MRHq_snmlt_df <- read.table("./Output/MRHq_snmlt.txt")
colnames(MRHq_snmlt_df) <- c('year', 'num_data_points','mean_annual_discharge_cfs', 'CM_days', 'percent_days_to_Q=0.98')
MRHq_snmlt_filtered_df <- MRHq_snmlt_df[-1,] #remove 1st yr w incomplete record

ggplot(MRHq_snmlt_filtered_df, aes(x=year, y=CM_days)) +
  geom_line(linetype = "dashed") + 
  theme_classic() +
  ggtitle("Center Timing Time Series") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Year") +
  ylab("Time to Runoff Center of Mass (days)")


# Days to Onset of Snow-melt time series
MRHqsnoset_df <- read.table("./Output/MRHqsnoset.txt")
colnames(MRHqsnoset_df) <- c('year', 'V2?','V3?', 'maybe_time_to_onset_melt_days')
ggplot(MRHqsnoset_df, aes(x=year, y=maybe_time_to_onset_melt_days)) +
  geom_line(linetype = "dashed") + 
  geom_smooth(colour = "black", size = 0.5, method = lm, se = FALSE) +
  theme_classic() +
  ggtitle("Days to Snowmelt Onset Time Series") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Year") +
  ylab("Time to Snowmelt Onset (days)")




merced_df <- read.table("./Data_Raw/MercedHI_Q_T_2022023.txt", sep = "\t")

colnames(merced_df) <- c('Agency_CD','Site_Number', 'Datetime', 'Temperature_water_degrees_Celsius_Mean', 'Temperature_water_degrees_Celsius_Mean_CD', 'Temperature_water_degrees_Celsius_Maximum', 'Temperature_water_degrees_Celsius_Maximum_CD', 'Temperature_water_degrees_Celsius_Minimum', 'Temperature_water_degrees_Celsius_Minimum_CD', 'Temperature_water_degrees_Celsius_Median', 'Temperature_water_degrees_Celsius_Median_CD', 'Discharge_cubic_feet_per_second_Mean', 'Discharge_cubic_feet_per_second_Mean_CD')

#change column data types
cleaned_merced_df <- removeHeaderFromData(merced_df)
cleaned_merced_df$Datetime <- as.Date(cleaned_merced_df$Datetime, "%Y-%m-%d")
cleaned_merced_df$Site_Number <- as.numeric(cleaned_merced_df$Site_Number)
cleaned_merced_df$Temperature_water_degrees_Celsius_Mean <- as.numeric(cleaned_merced_df$Temperature_water_degrees_Celsius_Mean)
cleaned_merced_df$Temperature_water_degrees_Celsius_Maximum <- as.numeric(cleaned_merced_df$Temperature_water_degrees_Celsius_Maximum)
cleaned_merced_df$Temperature_water_degrees_Celsius_Minimum <- as.numeric(cleaned_merced_df$Temperature_water_degrees_Celsius_Minimum)
cleaned_merced_df$Temperature_water_degrees_Celsius_Median <- as.numeric(cleaned_merced_df$Temperature_water_degrees_Celsius_Median)
cleaned_merced_df$Discharge_cubic_feet_per_second_Mean <- as.numeric(cleaned_merced_df$Discharge_cubic_feet_per_second_Mean)

#extract year, month, day
cleaned_merced_df$Year <- as.numeric(format(as.Date(cleaned_merced_df$Datetime, format="%m/%d/%Y"),"%Y"))
cleaned_merced_df$Month <- as.numeric(format(as.Date(cleaned_merced_df$Datetime, format="%m/%d/%Y"),"%m"))
cleaned_merced_df$Day <- as.numeric(format(as.Date(cleaned_merced_df$Datetime, format="%m/%d/%Y"),"%d"))

grouped_merced_df = groupByWaterYear(cleaned_merced_df)

#mean daily water temp time series - complete period of record
ggplot(cleaned_merced_df, aes(x=Datetime, y=Temperature_water_degrees_Celsius_Mean, group=1)) +
  geom_line() + 
  xlim(as.Date("2016-06-01"), NA) +
  theme_classic() +
  ggtitle("Mean Daily Water Temperature Time Series") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Time") +
  ylab("Temperature (degrees C)")

#mean daily water temp time series - most recent water year 

recent_wtr_yr_df <- subset(grouped_merced_df, grouped_merced_df$waterYear == 2021)
ggplot(recent_wtr_yr_df, aes(x=Datetime, y=Temperature_water_degrees_Celsius_Mean)) +
  geom_line() +
  theme_classic() +
  ggtitle("Mean Daily Water Temperature (WY2021)") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Time") +
  ylab("Temperature (degrees C)")


#mean annual water temperature boxplots
grouped_merced_boxplot_df <- subset(grouped_merced_df, grouped_merced_df$waterYear == c(2017,2018,2019,2020,2021))

ggplot(grouped_merced_boxplot_df, aes(x=as.character(waterYear), y=Temperature_water_degrees_Celsius_Mean)) + 
  geom_boxplot() + 
  #xlim(2016.7, NA) +
  theme_classic() +
  ggtitle("Mean Annual Water Temperature Boxplot") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Water Year") +
  ylab("Temperature (degrees C")


#Daily minimum and maximum water temperature time series - complt period of record
ggplot(cleaned_merced_df, aes(x=Datetime)) +
  geom_line(aes(y=Temperature_water_degrees_Celsius_Minimum),color='blue')+
  geom_line(aes(y=Temperature_water_degrees_Celsius_Maximum),color='orange')+
  xlim(as.Date("1962-01-01"), NA) +
  theme_classic() +
  ggtitle("Daily Minimum & Maximum Water Temperature Time Series") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Time") +
  ylab("Temperature (degrees C)")
  
#Daily minimum and maximum water temperature time series - most recent water year
recent_wtr_yr_df <- subset(grouped_merced_df, grouped_merced_df$waterYear == 2021)
ggplot(recent_wtr_yr_df, aes(x=Datetime)) +
  geom_line(aes(y=Temperature_water_degrees_Celsius_Minimum),color='blue')+
  geom_line(aes(y=Temperature_water_degrees_Celsius_Maximum),color='orange')+
  theme_classic() +
  ggtitle("Daily Minimum & Maximum Water Temperature (WY2021)") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Time") +
  ylab("Temperature (degrees C)")


# annual min and max water temperature boxplots
grouped_merced_boxplot_df <- subset(grouped_merced_df, grouped_merced_df$waterYear == c(2017,2018,2019,2020,2021))

ggplot(grouped_merced_boxplot_df, aes(x=as.character(waterYear))) +
  geom_boxplot(aes(y=Temperature_water_degrees_Celsius_Minimum),color='blue',alpha=.8)+
  geom_boxplot(aes(y=Temperature_water_degrees_Celsius_Maximum),color='orange',alpha=.8)+
  theme_classic() +
  ggtitle("Annual Minimum and Maximum Water Temperature Boxplot") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Water Year") +
  ylab("Temperature (degrees C")


# Flow duration curve (from fldur.R)
MRHq_fld_df <- read.table("./Output/MRHq_fld.txt")
colnames(MRHq_fld_df) <- c('V1', 'V2','V3')
ggplot(MRHq_fld_df, aes(x=V1, y=V2)) +
  geom_line() + 
  theme_classic() +
  ggtitle("Flow Duration Curve") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("V1") +
  ylab("V2")
