scrape_maxvision <- function(date_begin, date_end, file_type, test_step, file_name) {
    # initialize search list
    
    if(date_end - date_begin > 10) {
        startDates <- seq(date_begin, date_end, by = 10)
        endDates <- startDates[-c(1)]
        startDates <- startDates[-c(length(startDates))]
        searchTable <- data.frame(Start = startDates, 
                                  End = endDates,
                                  file_type = file_type,
                                  Step = test_step,
                                  file_name = file_name)
        searchTable[,1:2] <- format(searchTable[,1:2], "%m-%d-%Y")
        
        # search maxvision
        for(i in 1:nrow(searchTable)) {
            print(paste("Searching... | ", i, " out of ", nrow(searchTable), sep=""))
            if(i == 1) {
                datalogFileList <- search_maxvision_database(date_begin = searchTable$Start[i],
                                                   date_end = searchTable$End[i],
                                                   file_name = searchTable$file_name[i],
                                                   file_type = searchTable$file_type[i], 
                                                   step = searchTable$Step[i])
            } else {
                datalogFileList <- bind_rows(
                    datalogFileList,
                    search_maxvision_database(date_begin = searchTable$Start[i],
                                    date_end = searchTable$End[i],
                                    file_name = searchTable$file_name[i],
                                    file_type = searchTable$file_type[i], 
                                    step = searchTable$Step[i])
                )
            }
        }
        
        
    } else {
        datalogFileList <- search_maxvision_database(date_begin = format(date_begin, "%m-%d-%Y"),
                                           date_end = format(date_end, "%m-%d-%Y"),
                                           file_name = file_name,
                                           file_type = file_type,
                                           step = test_step)
    }
    
    return(datalogFileList)
    
}