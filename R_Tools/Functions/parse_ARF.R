parse_ARF <- function(ARF_number, lot_number) {
    
    url_p1 <- "http://packaging2.maxim-ic.internal/arfsearch/arfdata_view.php?editid1="
    url_p2 <- toString(ARF_number)
    url_p3 <- "&editid2="
    url_p4 <- toString(lot_number)
    URL3 <- paste(url_p1, url_p2, url_p3, url_p4, sep = "")
    
    con3 <- url(URL3)
    arfText <- readLines(con3)
    close(con3)
    
    waferList <- arfText[247]
    sourceLot <- arfText[246]
    
    if(length(unlist(strsplit(waferList, split="(", fixed=TRUE)))==1) {
        waferList <- "unable to parse"
        sourceLot <- "unable to parse"
    } else {
        waferList <- gsub(" ", "", unlist(strsplit(waferList, split="(", fixed=TRUE))[1])
        waferList <- paste0(sapply(unlist(strsplit(waferList, ",")), function(x){if(as.numeric(x)<10) {x <- paste("0", as.numeric(x), sep="")} else {x}}), collapse="", sep=",")
        sourceLot <- gsub(" ", "", substring(sourceLot, 1, 15))
    }
    
    waferList <- str_replace(waferList, ",$", "")
    
    return(list("source_lot" = sourceLot, "wafer_list" = waferList))
}
