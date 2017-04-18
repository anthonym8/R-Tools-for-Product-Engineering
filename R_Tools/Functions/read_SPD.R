#############  SPD READER  ##############################
read_SPD <- function(filePath){
    if(file.exists(filePath)){
        
        if(str_detect(filePath, "\\\\")) {
            filename <- str_split(filePath, "\\\\")[[1]][length(str_split(filePath, "\\\\")[[1]])]
        } else {
            filename <- str_split(filePath, "/")[[1]][length(str_split(filePath, "/")[[1]])]
        }
        
        
        # read metadata
        message("read_SPD: reading metadata")
        firstRead <- suppressMessages(
            suppressWarnings(
                read_csv(filePath, n_max = 10, col_types = "cc")
            )
        )
        
        startIndex <- min(which(is.na(firstRead[[1]])))
        dateIndex <- which(str_detect(firstRead[[1]], "Date"))
        lotIndex <- which(str_detect(firstRead[[1]], "Lot"))
        
        lotNumber <- firstRead[lotIndex, 2] %>% as.character()
        lotDate <- firstRead[dateIndex, 2] %>% mdy_hms()
        
        rm(dateIndex, lotIndex, firstRead)
        
        
        # read spec table
        message("read_SPD: reading spec table")
        secondRead <- suppressMessages(
            suppressWarnings(
                read_csv(filePath, n_max = 6, skip = startIndex, col_names = FALSE)
            )
        )
        
        secondRead <- secondRead[,-c(1,2)]
        testNums <- secondRead[1, ] %>% as.character()
        testBlock <- map_chr(testNums, function(x){ str_split(x, pattern = "\\.")[[1]][2] }) %>%
            as.numeric()
        subtestNum <- map_chr(testNums, function(x){ str_split(x, pattern = "\\.")[[1]][3] }) %>%
            as.numeric()
        comments <- secondRead[3, ] %>% as.character()
        testNames <- secondRead[2, ] %>% as.character() %>% toupper()
        spec_high <- secondRead[4, ] %>% as.numeric()
        spec_low <- secondRead[5, ] %>% as.numeric()
        units <- secondRead[6, ] %>% as.character
        columnNames <- paste(testNums, testNames, sep = ": ")
        
        testMatrix <- data_frame(COLUMNNAME = columnNames,
                                 TEST_NUM = testNums,
                                 TEST_NAME = testNames,
                                 TEST_BLOCK = testBlock,
                                 SUBTEST_NUM = subtestNum,
                                 SPEC_LOW = spec_low,
                                 SPEC_HIGH = spec_high,
                                 UNIT = units,
                                 COMMENTS = comments)
        
        rm(startIndex, comments, spec_high, spec_low, subtestNum, testBlock, testNames, testNums, units, secondRead)
        
        # read test data
        message("read_SPD: reading test data")
        thirdRead <- suppressMessages(
            suppressWarnings(
                read_csv(filePath, n_max = 15, col_types = "c")
            )
        )
        
        startIndex <- which(str_detect(thirdRead[[1]], "Serial"))
        testData <- suppressMessages(
            suppressWarnings(
                read_csv(filePath, skip = startIndex+1, col_names = c("Serial", "Bin", columnNames))
            )
        )
        
        rm(thirdRead, startIndex, columnNames)
        
    }
    
    # add lot number and date columns
    message("read_SPD: formatting data")
    testData <- map_df(testData, as.numeric) %>%
        mutate(LOT = lotNumber, MEASURE_TIME = lotDate) %>%
        mutate(FILENAME = filename) %>% 
        select(LOT, MEASURE_TIME, FILENAME, everything()) %>% 
        mutate(LOT = ifelse(str_detect(LOT, "_"), 
                            str_sub(LOT, start = 1, end = -1 + str_locate(LOT, "_")[1]), 
                            LOT))
    
    #return value
    return(list(testData = testData, spec = testMatrix))
    
}

