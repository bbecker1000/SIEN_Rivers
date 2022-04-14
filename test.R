library(tidyverse)

source('./Rcode/utils.R')

data <- readData('./Rcode/MRHq.txt')

RFILES <- c('lwflow', 'snwpulse', 'surfwtr', 'fldur', 'hiflow')

for (file in RFILES) {
  source(str_glue('./Rcode/{file}.R'))
  do.call(file, list(data, './Test_Output/'))
}

matLabOutputPath <- './MATLAB_Output/'
outputFiles <- list.files(matLabOutputPath)

for (file in outputFiles) {
  print(file)
  matlabOutput <- read.table(str_glue('{matLabOutputPath}{file}'))
  rOutput <- read.table(str_glue('./Test_Output/{file}'))
  print(all.equal(matlabOutput, rOutput, ignore_col_order = FALSE, ignore_row_order = FALSE))
}
