library(tidyverse)

source('./Rcode/utils.R')

outputFiles <- list.files('./MATLAB_Output/MRHq')

check <- function(dir) {
  for (file in outputFiles) {
    print(file)
    matlabOutput <- read.table(str_glue('./MATLAB_Output/{dir}/{file}'))
    rOutput <- read.table(str_glue('./Test_Output/{dir}/{file}'))
    print(all.equal(matlabOutput, rOutput, ignore_col_order = FALSE, ignore_row_order = FALSE))
  }
}

print('Input: MRHq.txt')
compose('./Rcode/MRHq.txt', './Test_Output/MRHq/', mrhq = TRUE)
check('MRHq')

print('Input: USGS')
compose('./Data_Raw/MercedHI_Q_T_2022023.txt', './Test_Output/usgs/', startDate = ymd('1915-10-01'), endDate = ymd('2009-09-30'))
check('usgs')
