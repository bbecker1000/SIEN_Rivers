library(tidyverse)

source('./Rcode/utils.R')

inputFile <- './Data_Raw/MercedHI_Q_T_2022023.txt'
outputDir <- './Output/'

compose(inputFile, outputDir, usgs = TRUE)
