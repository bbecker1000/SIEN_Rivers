library(tidyverse)

source('./Rcode/utils.R')

compose('./Rcode/MRHq.txt', './Test_Output/')

matLabOutputPath <- './MATLAB_Output/'
outputFiles <- list.files(matLabOutputPath)

for (file in outputFiles) {
  print(file)
  matlabOutput <- read.table(str_glue('{matLabOutputPath}{file}'))
  rOutput <- read.table(str_glue('./Test_Output/{file}'))
  print(all.equal(matlabOutput, rOutput, ignore_col_order = FALSE, ignore_row_order = FALSE))
}
