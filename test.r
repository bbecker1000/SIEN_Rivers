library('dplyr')

RFILES <- c('lwflow', 'snwpulse', 'surfwtr')

for (file in RFILES) {
  source(paste('./Rcode/', file, '.r', sep = ''))
  print('Hi')
}

matLabOutputPath <- './MATLAB_Output/'
outputFiles <- list.files(matLabOutputPath)

for (file in outputFiles) {
  print(file)
  matlabOutput <- read.table(paste(matLabOutputPath, file, sep = ''))
  rOutput <- read.table(paste('./Output/', file, sep = ''))
  print(all.equal(matlabOutput, rOutput, ignore_col_order = False, ignore_row_order = False))
}
