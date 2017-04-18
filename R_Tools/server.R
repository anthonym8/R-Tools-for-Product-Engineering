
# Define server logic
shinyServer(function(input, output, session) {
    
    # Load custom functions
    source("./Functions/search_ARF_database.R")
    source("./Functions/parse_ARF.R")
    source("./Functions/search_maxvision_database.R")
    source("./Functions/read_SPD.R")
    source("./Functions/build_pareto.R")
    
    #######  ARF Search Tool ###############
    
    # table transform functions
    complete <- function(x) {
        x %>%
            mutate(`ARF NUMBER` = as.factor(`ARF NUMBER`),
                   `DEVICE TYPE` = as.factor(`DEVICE TYPE`),
                   `DIE TYPE` = as.factor(`DIE TYPE`),
                   `LOT NUMBER` = as.factor(`LOT NUMBER`),
                   `DIE QTY` = as.numeric(`DIE QTY`),
                   PACKAGE = as.factor(PACKAGE),
                   ASSEMBLER = as.factor(ASSEMBLER),
                   `TEST FACILITY` = as.factor(`TEST FACILITY`),
                   `SOURCE LOT` = as.factor(`SOURCE LOT`))
    }
    bySplit <- function(x) {
        x %>% 
            select(`DIE TYPE`, `LOT NUMBER`, `SOURCE LOT`, `WAFERS`) %>% 
            mutate(`DIE TYPE` = as.factor(`DIE TYPE`),
                   `LOT NUMBER` = as.factor(`LOT NUMBER`),
                   `SOURCE LOT` = as.factor(`SOURCE LOT`),
                   WAFERS = as.factor(WAFERS))
    }
    byWafer <- function(x) {
        y <- data_frame()
        for(i in 1:nrow(x)) {
            if(nrow(y) == 0) {
                y <- data_frame(`DIE TYPE` = x$`DIE TYPE`[i], 
                                `LOT NUMBER` = x$`LOT NUMBER`[i],
                                `SOURCE LOT` = x$`SOURCE LOT`[i],
                                WAFER = str_split(x[["WAFERS"]][i], pattern = ",")[[1]])
            } else {
                y <- bind_rows(y,
                               data_frame(`DIE TYPE` = x$`DIE TYPE`[i], 
                                          `LOT NUMBER` = x$`LOT NUMBER`[i],
                                          `SOURCE LOT` = x$`SOURCE LOT`[i],
                                          WAFER = str_split(x[["WAFERS"]][i], pattern = ",")[[1]])
                )
            }
        } 
        
        y %>% 
            arrange(`LOT NUMBER`, WAFER) %>%
            mutate(`DIE TYPE` = as.factor(`DIE TYPE`),
                   `LOT NUMBER` = as.factor(`LOT NUMBER`),
                   `SOURCE LOT` = as.factor(`SOURCE LOT`),
                   WAFER = as.factor(WAFER))
    }
    
    # pull ARF data after button click
    ARF_results <- eventReactive(input$searchButton, {
        withProgress(message = 'Processing...', value = 0, {
            incProgress(0, detail = "Searching database")
            
            # search ARF database using input search parameters
            ARF_results <- search_ARF_database(die_type = input$die_type,
                       lot_number = input$lot_number,
                       device_type = input$device_type) 
            
            if(nrow(ARF_results) > 0) {
                ARF_results <- ARF_results %>% 
                    mutate(`SOURCE LOT` = NA,
                           `WAFERS` = NA)
                
                # parse ARF for source lot and wafer list for each entry
                for(i in 1:nrow(ARF_results)) {
                    incProgress(1/nrow(ARF_results), detail = paste0("Parsing ARF ", i, " out of ", nrow(ARF_results)))
                    parse_results <- parse_ARF(ARF_number = ARF_results$`ARF NUMBER`[i],
                                               lot_number = ARF_results$`LOT NUMBER`[i])
                    ARF_results$`SOURCE LOT`[i] <- parse_results[["source_lot"]]
                    ARF_results$`WAFERS`[i] <- parse_results[["wafer_list"]]
                }
                
            }
            
            incProgress(1, detail = "Done")
            return(ARF_results)
        })
    })
    
    # display ARF search results
    output$table <- DT::renderDataTable({
        dispTable <- as_data_frame(ARF_results())
        my_transform <- switch(input$format,
                               bySplit = bySplit,
                               byWafer = byWafer,
                               complete = complete)
        
        if(nrow(dispTable) > 0) {
            my_transform(dispTable)
        } else {
            dispTable
        }
        
        
    },
    filter = 'bottom',
    options = list(
        pageLength = 25, autoWidth = TRUE
    )
    )
    
    # export as csv
    output$export <- downloadHandler(
       
        filename = function() {
            paste0(paste(input$die_type, 
                         input$lot_number, 
                         input$device_type, 
                         Sys.Date(), 
                         sep = "_"),
                   ".csv")
        },
        
        
        content = function(file) {
            dispTable <- as_data_frame(ARF_results())
            my_transform <- switch(input$format,
                                   bySplit = bySplit,
                                   byWafer = byWafer,
                                   complete = complete)
            exportFile <- my_transform(dispTable)
            write_csv(exportFile, path = file)
        }
    )
    
    
    #######  Datalog Downloader Tool  ###################
    
    # pull ARF data after button click
    datalog_search <- eventReactive(input$search_Maxvision, {
        
        withProgress(message = 'Processing...', value = 0, {
            incProgress(0, detail = "Searching database")
            
            search_results <- data_frame()
            
            if(abs(diff(input$dateRange)) <= 10) {
                search_results <- search_maxvision_database(date_begin = input$dateRange[1], 
                                                            date_end = input$dateRange[2],
                                                            file_type = input$file_type,
                                                            step = input$test_step,
                                                            file_name = input$filename)
            } else {
                dates <- seq(input$dateRange[1], input$dateRange[2], by = 10)
                for(i in 1:(length(dates)-1)) {
                    
                    incProgress(1/(length(dates)-1), detail = paste(format(100*i/(length(dates)-1), digits = 4), "%"))
                    temp_results <- search_maxvision_database(date_begin = dates[i], 
                                                              date_end = dates[i+1],
                                                              file_type = input$file_type,
                                                              step = input$test_step,
                                                              file_name = input$filename)
                    if(nrow(search_results) == 0) {
                        search_results <- temp_results
                    } else {
                        search_results <- bind_rows(search_results, temp_results)
                    }
                }
            }
            
            return(search_results)
        })
        
    })
    
    output$datalog_table <- DT::renderDataTable({
        search_results <- datalog_search()
        if(nrow(search_results) > 0) {
            search_results %>%
                select(-FILE_LINK, -GBD, -EDC_FAC, -ATTEMPTS, -RELOADS) %>%
                mutate(FILE_SIZE = as.numeric(as.character(FILE_SIZE))) %>% 
                mutate(TIME = date(ymd_hms(TIME))) %>%
                mutate(TYPE = as.factor(TYPE),
                       FACILITY = as.factor(FACILITY),
                       DEVICE = as.factor(DEVICE),
                       PARENT_LOT = as.factor(PARENT_LOT),
                       LOT = as.factor(LOT),
                       WAFER = as.factor(WAFER),
                       STEP = as.factor(STEP))
        } else {
            search_results
        }
        
        
    },
    filter = 'bottom',
    options = list(
        pageLength = 25, autoWidth = TRUE
    )
    )
    
    
    observeEvent(input$download_all, {
        search_results <- datalog_search()
        search_results <- search_results[input$datalog_table_rows_all, ]
        
        download_folder <- rchoose.dir()
        if(length(download_folder)) {
            withProgress(message = 'Downloading...', value = 0, {
                # Number of times we'll go through the loop
                n <- nrow(search_results)
                
                for (i in 1:n) {
                    # Increment the progress bar, and update the detail text.
                    incProgress(1/n, detail = paste("File", i, "out of", n))
                    
                    print(paste("Downloading file ", i, " out of ", n, sep=""))
                    destFile <- paste0(download_folder, "/", search_results$FILE[i])
                    sourceFile <- as.character(search_results$FILE_LINK[i])
                    tryCatch(
                        {
                            download.file(url=sourceFile, destfile=destFile, quiet = TRUE)
                        },
                        error = function(cond) {
                            message(cond)
                        }
                    )
                    rm(sourceFile)
                    
                    
                    
                }
            })
        }
        
    })
    
    observeEvent(input$download_selected, {
        search_results <- datalog_search()
        search_results <- search_results[input$datalog_table_rows_selected, ]
        
        download_folder <- rchoose.dir()
        
        if(length(download_folder)) {
            
            withProgress(message = 'Downloading...', value = 0, {
                # Number of times we'll go through the loop
                n <- nrow(search_results)
                
                for (i in 1:n) {
                    # Increment the progress bar, and update the detail text.
                    incProgress(1/n, detail = paste("File", i, "out of", n))
                    
                    print(paste("Downloading file ", i, " out of ", n, sep=""))
                    destFile <- paste0(download_folder, "/", search_results$FILE[i])
                    sourceFile <- as.character(search_results$FILE_LINK[i])
                    tryCatch(
                        {
                            download.file(url=sourceFile, destfile=destFile, quiet = TRUE)
                        },
                        error = function(cond) {
                            message(cond)
                        }
                    )
                    rm(sourceFile)
                }
            })
        }
    })
    
    
    #######  SPD Test Pareto  ###########################
    
    # define local objects
    files_list <- data_frame()
    compiled_datalogs <- list()

    # `Load Files` button is pressed
    observeEvent(input$load_files, {
        files_list <<- rchoose.files(filters = structure(c("SPD datalog files (*.SPD)", 
                                                           "compressed SPD files (*.SPD.gz)", 
                                                           "*.SPD", "*.SPD.gz"),
                                                         .Dim = c(2, 2))
        )
        files_list <<- data_frame("Path" = files_list) %>%
            mutate(Path = gsub("\\\\", "/", Path),
                   Size = file.size(Path))
        
        output$file_table <- renderDataTable({
            files_list
        },
        options = list(
                pageLength = 15,
                autoWidth = TRUE,
                dom = "tp"
            )
        )
    })
    
    
    # render annotated data table
    raw_data_table <- eventReactive(input$parse_files, {
        compiled_datalogs$testData
        })
    
    output$raw_data_table <- renderDataTable({
        raw_data_table()
        },
        filter = 'top',
        options = list(
            pageLength = 10,
            autoWidth = TRUE,
            dom = "rtip"
            )
    )
    
    
    # `Parse files` button is pressed
    observeEvent(input$parse_files, {
        
        # read in each file
        parsed_datalogs <- list()
        withProgress(
            message = 'Parsing...', 
            value = 0, 
            {
                n <- nrow(files_list)
                for(i in 1:n) {
                    incProgress(1/n, detail = paste("File", i, "out of", n))
                    parsed_datalogs[[i]] <- read_SPD(files_list$Path[i])
                }
                rm(i, n)
            }
        )
        
        # collate files
        withProgress(
            message = 'Merging files...', 
            value = 0, 
            {
                n <- nrow(files_list)
                compiled_test_data <- data_frame()
                compiled_spec <- data_frame()
                for(i in 1:n) {
                    incProgress(1/n, detail = paste("File", i, "out of", n))
                    if(nrow(compiled_test_data) == 0) {
                        compiled_test_data <- parsed_datalogs[[i]][["testData"]]
                        compiled_spec <- parsed_datalogs[[i]][["spec"]]
                    } else {
                        compiled_test_data <- bind_rows(compiled_test_data,
                                                        parsed_datalogs[[i]][["testData"]])
                        compiled_spec <- bind_rows(compiled_spec,
                                                   parsed_datalogs[[i]][["spec"]])
                    }
                }
                rm(i, n, parsed_datalogs)
            }
        )
        
        compiled_test_data <- compiled_test_data %>% mutate(Bin = as.factor(Bin))
        compiled_spec <- unique(compiled_spec)
        
        compiled_datalogs <<- list("testData" = compiled_test_data, "spec" = compiled_spec)
        rm(compiled_spec, compiled_test_data)
        
        compiled_datalogs <<- build_pareto( compiled_datalogs )
        compiled_datalogs$testData <- compiled_datalogs$testData %>% 
            select(Bin, `First Fail`, everything(), -Serial)
        
        
        # switch active page
        updateTabsetPanel(session, "SPD_app", selected = "pareto_page")
        
        # render bin pareto
        withProgress(message = 'Generating Bin Pareto...', value = 0.3, {
            output$bin_pareto <- renderPlot({
                bin_pareto_plot <- compiled_datalogs[["testData"]] %>%
                    group_by(Bin) %>%
                    summarize(Count = n()) %>%
                    mutate(Percentage = Count/sum(Count)) %>%
                    arrange(desc(Count)) %>% 
                    mutate(Cumulative_Percentage = cumsum(Percentage)) %>% 
                    ggplot(aes(x = reorder(Bin, desc(Percentage)), y = Percentage, fill = Bin)) +
                    geom_bar(stat = "identity") +
                    geom_line(aes(x = Bin, y = Cumulative_Percentage, group = 1)) +
                    labs(x = "Bin",
                         y = "Percent Fallout",
                         title = "Bin Pareto") +
                    scale_y_continuous(labels = scales::percent, 
                                       limits = c(0,1),
                                       breaks = seq(0,1, by = 0.1)) +
                    theme(plot.title = element_text(hjust = 0.5))
                
                bin_pareto_plot
            })
            
            output$bin_fallout_summary <- DT::renderDataTable({
                compiled_datalogs[["testData"]][, "Bin"] %>%
                    group_by(Bin) %>% 
                    summarize(Count = n()) %>% 
                    mutate(Percentage = 100 * Count / sum(Count)) %>% 
                    mutate(Percentage = round(Percentage, digits = 2)) %>% 
                    arrange(desc(Count)) %>% 
                    datatable(extensions = c('Buttons'),
                              options = list(
                                  deferRender = TRUE,
                                  pageLength = -1,
                                  dom = "Bfrti",
                                  buttons = c('copy', 'csv', 'excel', 'pdf')
                              ))
            })
        })
        
        # render test pareto
        withProgress(message = 'Generating Test Pareto...', value = 0.6, {
            output$test_pareto <- renderPlot({
                test_pareto_plot <- compiled_datalogs$testData %>%
                    select(-LOT, -MEASURE_TIME) %>% 
                    filter(Bin != 1) %>% 
                    group_by(`First Fail`, Bin) %>% 
                    summarize(`Fail Count` = n()) %>% 
                    ggplot(aes(x = reorder(`First Fail`, desc(`Fail Count`)), 
                               y = `Fail Count`, 
                               fill = Bin,
                               label = `First Fail`)) +
                    geom_bar(stat = "identity") +
                    ylab("Fail Count") +
                    xlab("Test Name") +
                    ggtitle("Test Failures") +
                    theme(plot.title = element_text(hjust = 0.5),
                          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
                
                test_pareto_plot
            })
            
            output$test_fail_data <- DT::renderDataTable({
                compiled_datalogs$testData %>% 
                    group_by(`First Fail`, Bin) %>% 
                    summarize(`Fail Count` = n()) %>%
                    ungroup() %>% 
                    arrange(desc(`Fail Count`)) %>% 
                    mutate(`% Yield Loss` = 100 * `Fail Count` / sum(`Fail Count`)) %>%
                    filter(Bin != 1) %>% 
                    mutate(`% Fail` = 100 * `Fail Count` / sum(`Fail Count`)) %>%
                    mutate(`% Yield Loss` = round(`% Yield Loss`, digits = 2),
                           `% Fail` = round(`% Fail`, digits = 2)) %>% 
                    datatable(extensions = c('Buttons'),
                              options = list(
                                  deferRender = TRUE,
                                  pageLength = -1,
                                  dom = "Brti",
                                  buttons = c('copy', 'csv', 'excel', 'pdf')
                              ))
            })
        })
        
        # load raw data table
        withProgress(
            message = 'Preparing data table...', 
            value = 0.9,
            {
                raw_data_table()
                incProgress(1)
            }
        )
        
        
        # update plotting choices for distribution plot
        updateSelectInput(session, "dist_plot_x_data",
                          choices = names(compiled_datalogs$testData))
        updateSelectInput(session, "dist_plot_split_by",
                          choices = c("None", names(compiled_datalogs$testData)),
                          selected = "None")
        
        # update plotting choices for trend chart
        updateSelectInput(session, "trend_chart_y_data",
                          choices = names(compiled_datalogs$testData))
        updateSelectInput(session, "trend_chart_sort_by",
                          choices = c("None", names(compiled_datalogs$testData)),
                          selected = "None")
        updateSelectInput(session, "trend_chart_color_by",
                          choices = c("None", names(compiled_datalogs$testData)),
                          selected = "None")
        
        # update plotting choices for scatter plot
        updateSelectInput(session, "scatter_plot_x_data",
                          choices = names(compiled_datalogs$testData))
        updateSelectInput(session, "scatter_plot_y_data",
                          choices = names(compiled_datalogs$testData))
        updateSelectInput(session, "scatter_plot_color_by",
                          choices = c("None", names(compiled_datalogs$testData)),
                          selected = "None")
    })
    
    


    
    # render distribution plot
    render_dist_plot <- eventReactive(input$update_plot, {
        
        LSL <- compiled_datalogs[["spec"]] %>%
            filter(COLUMNNAME == input$dist_plot_x_data) %>% 
            select(SPEC_LOW) %>% 
            as.numeric()
        
        USL <- compiled_datalogs[["spec"]] %>%
            filter(COLUMNNAME == input$dist_plot_x_data) %>% 
            select(SPEC_HIGH) %>% 
            as.numeric()
        
        plot_data <- compiled_datalogs[["testData"]][input$raw_data_table_rows_all, ]
        
        plot_min <- min(c(select(plot_data, Data = starts_with(input$dist_plot_x_data))[["Data"]], LSL, USL))
        plot_max <- max(c(select(plot_data, Data = starts_with(input$dist_plot_x_data))[["Data"]], LSL, USL))
        plot_range <- plot_max - plot_min
        
        plot_min <- round(plot_min - 0.01*plot_range, 3)
        plot_max <- round(plot_max + 0.01*plot_range, 3)
        plot_range <- round(plot_range, 3)
        

        updateSliderInput(session, "dist_plot_x",
                          min = plot_min,
                          max = plot_max,
                          value = c(plot_min, plot_max),
                          step = round(plot_range/1000, 3)
                          )
        
        if(input$dist_plot_split_by == "None") {
            plot_object <- plot_data %>% 
                select(Data = starts_with(input$dist_plot_x_data)) %>% 
                arrange(Data) %>% 
                mutate(PPoints = ppoints(n())) %>% 
                ggplot(aes(x = Data, y = PPoints)) +
                geom_point(size = 1, alpha = 0.8) +
                labs(x = input$dist_plot_x_data,
                     y = "",
                     title = input$dist_plot_x_data) +
                scale_y_continuous(trans = "probit", 
                                   breaks = c(0.001,0.01,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.99,0.999),
                                   minor_breaks = NULL,
                                   labels = scales::percent,
                                   expand = c(0,0)) +
                theme(plot.title = element_text(hjust = 0.5)) +
                geom_vline(col = "red", xintercept = LSL) +
                geom_vline(col = "red", xintercept = USL)
        } else {
            plot_object <- plot_data %>% 
                select(Data = starts_with(input$dist_plot_x_data),
                       Split = starts_with(input$dist_plot_split_by)) %>% 
                group_by(Split) %>% 
                arrange(Data) %>% 
                mutate(PPoints = ppoints(n())) %>% 
                ggplot(aes(x = Data, y = PPoints, col = Split)) +
                geom_point(size = 1, alpha = 0.8) +
                labs(x = input$dist_plot_x_data,
                     y = "",
                     title = input$dist_plot_x_data) +
                scale_y_continuous(trans = "probit", 
                                   breaks = c(0.001,0.01,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.99,0.999),
                                   minor_breaks = NULL,
                                   labels = scales::percent,
                                   expand = c(0,0)) +
                theme(plot.title = element_text(hjust = 0.5)) +
                geom_vline(col = "red", xintercept = LSL) +
                geom_vline(col = "red", xintercept = USL)
        }
        
        plot_object <- plot_object +
            xlim(plot_min, plot_max)
        
        return(plot_object)
    })
    
    output$distribution_plot <- renderPlot({
        render_dist_plot() +
            xlim(input$dist_plot_x[1], input$dist_plot_x[2])
    })
    
    
    # render trend chart
    render_trend_chart <- eventReactive(input$trend_chart_update, {
        
        LSL <- compiled_datalogs[["spec"]] %>%
            filter(COLUMNNAME == input$trend_chart_y_data) %>% 
            select(SPEC_LOW) %>% 
            as.numeric()
        
        USL <- compiled_datalogs[["spec"]] %>%
            filter(COLUMNNAME == input$trend_chart_y_data) %>% 
            select(SPEC_HIGH) %>% 
            as.numeric()
        
        plot_data <- compiled_datalogs[["testData"]][input$raw_data_table_rows_all, ]
        
        plot_min <- min(c(select(plot_data, Data = starts_with(input$trend_chart_y_data))[["Data"]], LSL, USL))
        plot_max <- max(c(select(plot_data, Data = starts_with(input$trend_chart_y_data))[["Data"]], LSL, USL))
        plot_range <- plot_max - plot_min
        
        plot_min <- round(plot_min - 0.01*plot_range, 3)
        plot_max <- round(plot_max + 0.01*plot_range, 3)
        plot_range <- round(plot_range, 3)
        
        
        updateSliderInput(session, "trend_chart_y_limits",
                          min = plot_min,
                          max = plot_max,
                          value = c(plot_min, plot_max),
                          step = round(plot_range/1000, 3)
        )
        
        if(input$trend_chart_sort_by == "None") {
            if(input$trend_chart_color_by == "None") {
                plot_object <- plot_data %>%
                    arrange(LOT, Serial) %>%
                    select(Data = starts_with(input$trend_chart_y_data)) %>% 
                    mutate(Index = 1:nrow(.))
            } else {
                plot_object <- plot_data %>%
                    arrange(LOT, Serial) %>%
                    select(Data = starts_with(input$trend_chart_y_data),
                           Split = starts_with(input$trend_chart_color_by)) %>%
                    mutate(Split = as.factor(Split)) %>% 
                    mutate(Index = 1:nrow(.))
            }
        } else {
            if(input$trend_chart_color_by == "None") {
                plot_object <- data_frame(Data = plot_data[[input$trend_chart_y_data]],
                                          Index = plot_data[[input$trend_chart_sort_by]]) %>%
                    arrange(Index) %>% 
                    mutate(Index = 1:nrow(.))
            } else {
                plot_object <- data_frame(Data = plot_data[[input$trend_chart_y_data]],
                                          Split = plot_data[[input$trend_chart_color_by]],
                                          Index = plot_data[[input$trend_chart_sort_by]]) %>% 
                    arrange(Index) %>% 
                    mutate(Split = as.factor(Split)) %>% 
                    mutate(Index = 1:nrow(.))
            }
        }
        
        if(input$trend_chart_color_by == "None") {
            plot_object <- plot_object %>% 
                ggplot(aes(x = Index, y = Data)) +
                geom_point(size = 1, alpha = 0.8) +
                geom_line(aes(group = 1)) +
                labs(y = input$trend_chart_y_data,
                     x = "Index",
                     title = input$trend_chart_y_data) +
                theme(plot.title = element_text(hjust = 0.5)) +
                geom_hline(col = "red", yintercept = LSL) +
                geom_hline(col = "red", yintercept = USL)
        } else {
            plot_object <- plot_object %>% 
                ggplot(aes(x = Index, y = Data, col = Split)) +
                geom_point(size = 1, alpha = 0.8) +
                geom_line(aes(group = Split)) +
                labs(y = input$trend_chart_y_data,
                     x = "Index",
                     title = input$trend_chart_y_data) +
                theme(plot.title = element_text(hjust = 0.5)) +
                geom_hline(col = "red", yintercept = LSL) +
                geom_hline(col = "red", yintercept = USL)
        }
        
        plot_object <- plot_object +
            ylim(plot_min, plot_max)
        
        return(plot_object)
    })
    
    output$trend_chart_output <- renderPlot({
        render_trend_chart() +
            ylim(input$trend_chart_y_limits[1], input$trend_chart_y_limits[2])
    })
    
    # render scatter plot
    render_scatter_plot <- eventReactive(input$scatter_plot_update, {
        x_LSL <- compiled_datalogs[["spec"]] %>%
            filter(COLUMNNAME == input$scatter_plot_x_data) %>% 
            select(SPEC_LOW) %>% 
            as.numeric()
        
        x_USL <- compiled_datalogs[["spec"]] %>%
            filter(COLUMNNAME == input$scatter_plot_x_data) %>% 
            select(SPEC_HIGH) %>% 
            as.numeric()
        
        y_LSL <- compiled_datalogs[["spec"]] %>%
            filter(COLUMNNAME == input$scatter_plot_y_data) %>% 
            select(SPEC_LOW) %>% 
            as.numeric()
        
        y_USL <- compiled_datalogs[["spec"]] %>%
            filter(COLUMNNAME == input$scatter_plot_y_data) %>% 
            select(SPEC_HIGH) %>% 
            as.numeric()
        
        plot_data <- compiled_datalogs[["testData"]][input$raw_data_table_rows_all, ]
        
        x_plot_min <- min(c(select(plot_data, Data = starts_with(input$scatter_plot_x_data))[["Data"]], LSL, USL))
        x_plot_max <- max(c(select(plot_data, Data = starts_with(input$scatter_plot_x_data))[["Data"]], LSL, USL))
        x_plot_range <- x_plot_max - x_plot_min
        
        x_plot_min <- round(x_plot_min - 0.01*x_plot_range, 3)
        x_plot_max <- round(x_plot_max + 0.01*x_plot_range, 3)
        x_plot_range <- round(x_plot_range, 3)
        
        y_plot_min <- min(c(select(plot_data, Data = starts_with(input$scatter_plot_y_data))[["Data"]], LSL, USL))
        y_plot_max <- max(c(select(plot_data, Data = starts_with(input$scatter_plot_y_data))[["Data"]], LSL, USL))
        y_plot_range <- y_plot_max - y_plot_min
        
        y_plot_min <- round(y_plot_min - 0.01*y_plot_range, 3)
        y_plot_max <- round(y_plot_max + 0.01*y_plot_range, 3)
        y_plot_range <- round(y_plot_range, 3)
        
        
        updateSliderInput(session, "scatter_plot_x_limits",
                          min = x_plot_min,
                          max = x_plot_max,
                          value = c(x_plot_min, x_plot_max),
                          step = round(x_plot_range/1000, 3)
        )
        
        updateSliderInput(session, "scatter_plot_y_limits",
                          min = y_plot_min,
                          max = y_plot_max,
                          value = c(y_plot_min, y_plot_max),
                          step = round(y_plot_range/1000, 3)
        )
        
        if(input$scatter_plot_color_by == "None") {
            plot_object <- data_frame(x_Data = plot_data[[input$scatter_plot_x_data]],
                                      y_Data = plot_data[[input$scatter_plot_y_data]]) %>% 
                ggplot(aes(x = x_Data, y = y_Data)) +
                geom_point(size = 1, alpha = 0.8) +
                labs(y = input$scatter_plot_y_data,
                     x = input$scatter_plot_x_data) +
                theme(plot.title = element_text(hjust = 0.5)) +
                geom_hline(col = "red", yintercept = y_LSL) +
                geom_hline(col = "red", yintercept = y_USL) +
                geom_vline(col = "red", xintercept = x_LSL) +
                geom_vline(col = "red", xintercept = x_USL)
        } else {
            plot_object <- data_frame(x_Data = plot_data[[input$scatter_plot_x_data]],
                                      y_Data = plot_data[[input$scatter_plot_y_data]],
                                      split = plot_data[[input$scatter_plot_color_by]]) %>% 
                mutate(split = as.factor(split)) %>% 
                ggplot(aes(x = x_Data, y = y_Data, col = split)) +
                geom_point(size = 1, alpha = 0.8) +
                labs(y = input$scatter_plot_y_data,
                     x = input$scatter_plot_x_data) +
                theme(plot.title = element_text(hjust = 0.5)) +
                geom_hline(col = "red", yintercept = y_LSL) +
                geom_hline(col = "red", yintercept = y_USL) +
                geom_vline(col = "red", xintercept = x_LSL) +
                geom_vline(col = "red", xintercept = x_USL)
        }
      
        
        plot_object <- plot_object +
            ylim(y_plot_min, y_plot_max) +
            xlim(x_plot_min, x_plot_max)
        
        return(plot_object)
    })
    
    output$scatter_plot <- renderPlotly({
        plot_object <- render_scatter_plot() +
            xlim(input$scatter_plot_x_limits[1], input$scatter_plot_x_limits[2]) +
            ylim(input$scatter_plot_y_limits[1], input$scatter_plot_y_limits[2])
        
        ggplotly(plot_object)
    })
    
    
    
    
    #######  Bulk Processing Tool  ######################
    
    # define file type structure objects
    file_type_SPD <- structure(c("SPD datalog files (*.SPD)", 
                                 "compressed SPD files (*.SPD.gz)", 
                                 "*.SPD", "*.SPD.gz"),
                               .Dim = c(2, 2))
    file_type_STDF <- structure(c("STDF datalog files (*.STD)", 
                                 "compressed STDF files (*.STD.gz)", 
                                 "*.STD", "*.STD.gz"),
                               .Dim = c(2, 2))

    
    # `Load Files` button is pressed
    bulk_files_list <- data_frame()
    bulk_test_choices <- character()
    observeEvent(input$bulk_select_files, {
        
        # choose file type filters
        bulk_processing_file_type <- if(input$bulk_file_type == "SPD") {
            file_type_SPD
        } else {
            file_type_STDF
        }
        
        # file input dialog box
        bulk_files_list <<- rchoose.files(filters = bulk_processing_file_type)
        bulk_files_list <<- data_frame("Path" = bulk_files_list) %>%
            mutate(Path = gsub("\\\\", "/", Path),
                   Size = file.size(Path)) 

        # render file table
        output$bulk_file_table <- renderDataTable({
            if(nrow(bulk_files_list) > 0) {
                bulk_files_list %>%
                    mutate(File = str_split(Path, "/")[[1]][length(str_split(Path, "/")[[1]])]) %>%
                    select(File, Size)
            } else {
                bulk_files_list
            }
            
        },
        options = list(
            pageLength = 15,
            autoWidth = FALSE,
            dom = "t"
        )
        )
        
        # read 1st file
        bulk_test_choices <<- read_SPD(bulk_files_list$Path[1])[["testData"]] %>% 
            select(-(LOT:Serial)) %>% 
            names()
        
        output$bulk_test_stat_choices <- DT::renderDataTable({
            data_frame(Column = bulk_test_choices)
        },
        # filter = "bottom",
        rownames = FALSE,
        colnames = NULL,
        extensions = "Scroller",
        options = list(
            scroller = TRUE,
            scrollY = 350,
            pageLength = 10,
            autowidth = FALSE,
            dom = "t"
        )
        )
    })
    
    # test statistics tab: run button is pressed
    test_stat_summary <- data_frame()
    observeEvent(input$bulk_test_stat_run, {
        
        # initialize variables
        bulk_compiled_data <- data_frame()
        bulk_test_selected <- bulk_test_choices[input$bulk_test_stat_choices_rows_selected]
        bulk_split_selected <- toupper(input$bulk_select_test_split)
        
        # parse datalogs and extract chosen columns
        withProgress(message = 'Parsing datalogs...', value = 0, {
            n <- nrow(bulk_files_list)
            for(i in 1:n) {
                incProgress(1/n, detail = paste0("File ", i, " out of ", n))
                bulk_parsed_data <- read_SPD(bulk_files_list$Path[i])[["testData"]] %>% 
                    select(Split = one_of(bulk_split_selected), one_of(bulk_test_selected))
                
                if(nrow(bulk_compiled_data) == 0) {
                    bulk_compiled_data <- bulk_parsed_data
                } else {
                    bulk_compiled_data <- bind_rows(bulk_compiled_data,
                                                    bulk_parsed_data)
                }
                
                rm(bulk_parsed_data)
            }
        })
        
        
        # define summary functions
        bulk_function_list <- list(function(x) mean(x, na.rm = TRUE),
                                   function(x) sd(x, na.rm = TRUE),
                                   function(x) min(x, na.rm = TRUE),
                                   function(x) max(x, na.rm = TRUE),
                                   function(x) median(x, na.rm = TRUE),
                                   function(x) diff(range(x, na.rm = TRUE)),
                                   function(x) length(x),
                                   function(x) sum(x, na.rm = TRUE),
                                   function(x) var(x, na.rm = TRUE),
                                   function(x) quantile(x, probs = 0.25),
                                   function(x) quantile(x, probs = 0.75))
        bulk_function_names <- c("Average",
                                 "Std Deviation",
                                 "Minimum",
                                 "Maximum",
                                 "Median",
                                 "Range",
                                 "Count",
                                 "Sum",
                                 "Variance",
                                 "1st Quartile",
                                 "3rd Quartile")

        # compute test summary
        bulk_output_summary_table <- bulk_compiled_data %>%
            gather(COLUMN, VALUE, -Split) %>% 
            mutate(VALUE = as.numeric(VALUE)) %>% 
            filter(VALUE != 0) %>% 
            filter(!is.na(VALUE)) %>% 
            group_by(Split, COLUMN) %>% 
            summarize(stat = bulk_function_list[[as.integer(input$bulk_stat_choices)]](VALUE)) %>% 
            ungroup() %>% 
            spread(COLUMN, stat) %>% 
            mutate(Function = bulk_function_names[as.integer(input$bulk_stat_choices)]) %>% 
            select(Split, Function, everything())
        
        # render test summary output table
        output$bulk_test_summary <- DT::renderDataTable({
            bulk_output_summary_table %>% 
                datatable(extensions = c('Buttons'),
                          options = list(
                              deferRender = TRUE,
                              pageLength = -1,
                              dom = "Brti",
                              buttons = c('copy', 'csv', 'excel', 'pdf')
                          ))
        })
    }
    )
})