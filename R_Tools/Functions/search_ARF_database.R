search_ARF_database <- function(die_type = "", lot_number = "", device_type = "") {
    
    #determine number of search results
    URL1 <- paste("http://packaging2.maxim-ic.internal/arfsearch/arfdata_list.php?q=(DIETYPE~contains~", 
                  die_type, 
                  ")(LOT_NUMBER~contains~", 
                  lot_number, 
                  ")(PART_NUMBER~contains~",
                  device_type,
                  ")",
                  sep="")
    con1 = url(URL1)
    htmlCode1 = readLines(con1)
    close(con1)

    resultCount <- htmlCode1[str_detect(htmlCode1, "Details found:")] %>%
        str_replace_all("</span>", "") %>%
        str_sub(start = 3+str_locate(., "rnr-details_found_count")[2],
                end = -1) %>% 
        as.numeric()
    
    
    
    
    
    #check if there are search results
    if(!is.na(resultCount)) {
        
        #create data table
        assemblyData <- matrix(nrow=resultCount, ncol=10)
        colnames(assemblyData) <- c("ARF NUMBER", "DEVICE TYPE", "DIE TYPE", "LOT NUMBER", "DIE QTY", "PACKAGE", "ASSEMBLER", "TEST FACILITY", "SOURCE LOT", "WAFERS")
        
        #parse search results
        content1 = content(GET(URL1), as="text")
        URL2 <- "http://packaging2.maxim-ic.internal/arfsearch/arfdata_list.php?pagesize=-1"
        content2 = content(GET(URL2), as="text")
        parsedHtml2 = htmlParse(content2, asText=TRUE)
        rawData <- xpathSApply(parsedHtml2, "//*/td/span", xmlValue)
        rawData <- matrix(rawData, ncol=11, nrow=length(rawData)/11, byrow=TRUE)
        
        rawMatrix <- matrix(nrow=nrow(rawData), ncol=8)
        rawMatrix[1:nrow(rawMatrix),] <- rawData[1:nrow(rawData),c(2,3,4,5,6,7,8,10)]
        
        assemblyData <- as_data_frame(rawMatrix)
        colnames(assemblyData) <- c("ARF NUMBER", "DEVICE TYPE", "DIE TYPE", "LOT NUMBER", "DIE QTY", "PACKAGE", "ASSEMBLER", "TEST FACILITY")

    } else {
        assemblyData <- matrix(nrow=1, ncol=8)
        colnames(assemblyData) <- c("ARF NUMBER", "DEVICE TYPE", "DIE TYPE", "LOT NUMBER", "DIE QTY", "PACKAGE", "ASSEMBLER", "TEST FACILITY")
    }
    
    #remove rows with missing values
    assemblyData <- na.omit(assemblyData)
    
    
    #return value
    return(assemblyData)
    
}