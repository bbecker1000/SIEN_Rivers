# lwflow
#Calculates the low flow statistics from the time series of daily values,
lwflow <- function(data, outputDir = './Output/') {
    yearly <- data %>% groupByWaterYear()
    
    outputMAQ <- yearly %>% summarize(amQ = mean(dmq))
    
    writeOutput(outputMAQ, '   %7.f     %10.2f\n', 'mrhMAQ', outputDir)
    
    lagged <- yearly %>% lagX()
    sumMin <- function(df, lower) df %>% sumLag(min, m7d = which.min(lag7) + lower - 2)
    
    winter <- lagged %>%
        filter(mth %in% c(12, 1, 2) | (mth == 3 & day == 1)) %>%
        sumMin(62)
    
    summer <- lagged %>%
        includeDays(273, 365) %>%
        sumMin(273)
    
    output1D <- winter %>%
        inner_join(summer, by = 'waterYear', suffix = c('w', 's')) %>%
        select(waterYear, m3w:m14w, m3s:m14s, m7dw, m7ds)
    
    writeOutput(output1D, '%7.f %7.3f %7.3f %7.3f %7.3f %7.3f %7.3f %7.f %7.f\n', 'mrhlwf1D', outputDir)
    
    output2D <- output1D %>%
        mutate(exp = row_number() / (n() + 1), across(m3w:m14s, sort)) %>%
        select(exp, m3w:m14s)
    
    writeOutput(output2D, '%7.3f  %7.3f  %7.3f  %7.3f  %7.3f  %7.3f   %7.3f\n', 'mrhlwf2D', outputDir)
    
    output3DX <- output2D %>% genOutput3X()
    writeOutput(output3DX, '%7.3f  %7.3f   %7.3f  %7.3f  %7.3f  %7.3f  %7.3f\n', 'mrhlwf3DX', outputDir)
    
    list(outputMAQ, output1D, output2D, output3DX)
}