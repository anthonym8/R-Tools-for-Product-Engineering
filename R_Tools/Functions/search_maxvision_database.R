

search_maxvision_database <- function(date_begin, date_end, file_name, file_type, step) {
    
    
    # load required libraries
    require(lubridate)
    require(httr)
    require(XML)
    require(RCurl)
    require(stringr)
    require(dplyr)
    require(purrr)
    
    
    # constructURL function
    constructURL <- function(begin, end, fname, type, step, page) {
        link <- vector(mode="character")
        link[1] <- "http://maxv2.maxim-ic.com/cgi-bin/reportarea.pl?CMD=FF&DEBUG=0&DBM="
        link[2] <- if(month(begin)<10){ paste("0",month(begin),sep="") } else {month(begin)}
        link[3] <- "&DBD="
        link[4] <- if(day(begin)<10){ paste("0",day(begin),sep="") } else {day(begin)}
        link[5] <- "&DBY="
        link[6] <- year(begin)
        link[7] <- "&DBH=00&DEM="
        link[8] <- if(month(end)<10){ paste("0",month(end),sep="") } else {month(end)}
        link[9] <- "&DED="
        link[10] <- if(day(end)<10){ paste("0",day(end),sep="") } else {day(end)}
        link[11] <- "&DEY="
        link[12] <- year(end)
        link[13] <- "&DEH=00&.submit=Submit&D=1&FT="
        link[14] <- type
        link[15] <- "&EDCFAC=ALL&FAC=ALL&STEP="
        link[16] <- step
        link[17] <- "&RSLT=ALL&WC=1&PRODUCT=&LOTWC=&PARENT_LOT=&PT="
        link[18] <- page
        link[19] <- "&FN="
        link[20] <- fname
        link[21] <- "&.cgifields=RSLT&.cgifields=TAR"
        
        url <- paste(link, collapse="")
    }
    
    # parseHTML function
    parseHTML <- function(htmlCode) {
        tablehead <- htmlCode[grep(">TIME<", htmlCode)]
        tablehead <- unlist(strsplit(tablehead, split="<td>"))
        tablehead <- as.character(sapply(tablehead, function(x){ str_sub(x, start=1, end=(-1+str_locate(x, "<")[1]) ) }))
        tablehead <- tablehead[tablehead!=""]
        tablehead[which(str_detect(tablehead, "Files"))] <- "FILE"
        htmlCode2 <- htmlCode[ (grep(">TIME<", htmlCode)+1) : (length(htmlCode)-4) ]
        htmlCode2 <- htmlCode2[seq(from=1, to=length(htmlCode2), by=2)]
        
        pagetree <- htmlTreeParse(htmlCode2, error=function(...){}, useInternalNodes = TRUE)
        results <- xpathSApply(pagetree, "//*/td", xmlValue)
        results <- as_data_frame(matrix(results, ncol=14, byrow=TRUE))
        names(results) <- tablehead
        results$FILE_LINK <- rep(NA, nrow(results))
        
        if(nrow(results)>0) {
            fileLinks <- as.character(xpathSApply(pagetree, "//a/@href"))
            fileLinks <- fileLinks[seq(from=5, to=length(fileLinks), by=5)]
            fileLinks <- as.character(unlist(sapply(fileLinks, function(x){ paste("http://maxv2.maxim-ic.com/", x, sep="") })))
            results$FILE_LINK <- fileLinks
        }
        
        return(results)
    }
    
    # getFileNums function
    getFileNums <- function(htmlCode) {
        fileNums <- htmlCode[str_detect(htmlCode, "Showing")]
        fileNums <- str_sub(fileNums, start = str_locate(fileNums, "Showing")[1], 
                            end = str_locate(fileNums, "Files")[2])
        fileNums <- gsub("Showing.*-", "", fileNums)
        fileNums <- gsub("Files", "", fileNums)
        fileNums <- unlist(str_split(fileNums, " of ")) %>% str_trim() %>% as.numeric()
    }
    
    
    # initialize my_results data frame
    my_results <- data_frame()
    
    print(paste("Fetching results for date range: ", date_begin, 
                " to ", date_end, sep =""))
    
    
    # Fetch html source code
    theurl <- constructURL(begin = date_begin, end = date_end, fname = file_name, 
                           type = file_type, step = step, page = 1)
    webpage <- readLines(tc <- textConnection(getURL(theurl))); close(tc);

    
    # parse html file table
    my_results <- parseHTML(webpage)
    
    
    # check if there are multiple page results
    if(any(str_detect(webpage, "Showing"))) {
        
        # extract returned files info
        latestFile <- getFileNums(webpage)[1]
        lastFile <- getFileNums(webpage)[2]
        
        # print progress to console
        if(!is.na(latestFile) & !is.na(lastFile)) {
            print(paste("   Parsing Maxvision results | Page ", 1, 
                        " | ", latestFile, " out of ", lastFile, 
                        " Files...", sep = ""))
        }
        
        # iterate over all pages until entire file list is extracted
        pageNum <- 2
        while(latestFile != lastFile) {
            
            # fetch html source code
            theurl <- constructURL(begin = date_begin, end = date_end, fname = file_name, 
                                   type = file_type, step = step, page = pageNum)
            webpage <- readLines(tc <- textConnection(getURL(theurl))); close(tc);

            # extract returned files info
            latestFile <- getFileNums(webpage)[1]
            lastFile <- getFileNums(webpage)[2]
            
            # print progress to console
            print(paste("   Parsing Maxvision results | Page ", pageNum, 
                        " | ", latestFile, " out of ", lastFile, " Files...", sep = ""))
            
            # parse html file table
            results <- parseHTML(webpage)
            
            # append new results to my_results data frame
            my_results <- bind_rows(my_results, results)
            
            # increment page number
            pageNum <- pageNum + 1
        }
    }
    
    
    return(my_results)

}

