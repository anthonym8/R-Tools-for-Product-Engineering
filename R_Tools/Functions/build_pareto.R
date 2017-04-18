
build_pareto <- function(parsed_SPD) {
    
    # split test data and spec data
    message("build_pareto: Splitting test data")
    spec <- parsed_SPD$spec
    test_data <- parsed_SPD$testData

    # label each row/unit with unique serial number
    message("build_pareto: labelling each record with unique serial number")
    test_data_serialized <- test_data %>% 
        mutate(my_serial = 1:nrow(test_data)) %>% 
        select(my_serial, everything())
    
    # long format of reject units
    message("build_pareto: transforming data from wide to long format")
    test_data_labelled_long <- test_data_serialized %>%
        select(-Serial) %>% 
        filter(Bin != 1) %>%
        gather(COLUMNNAME, VALUE, -LOT, -MEASURE_TIME, -FILENAME, -my_serial, -Bin) %>%
        arrange(my_serial) %>%
        left_join(spec, by = "COLUMNNAME") %>%
        select(my_serial, Bin, COLUMNNAME, TEST_BLOCK, SUBTEST_NUM, VALUE, SPEC_LOW, SPEC_HIGH) %>%
        mutate(FAIL = ifelse((VALUE > SPEC_LOW & VALUE < SPEC_HIGH), FALSE, TRUE),
               ZERO = ifelse(VALUE == 0, TRUE, FALSE)) 
    
    # determine the failing test block for each unit
    message("build_pareto: determining failing test block for each DUT")
    fail_test_block <- test_data_labelled_long %>%
        na.omit() %>%
        group_by(my_serial, Bin, TEST_BLOCK) %>%
        arrange(my_serial, TEST_BLOCK, SUBTEST_NUM) %>% 
        summarize(num_test = n(),
                  zero_count = sum(ZERO),
                  fail_count = sum(FAIL)) %>%
        mutate(zero_ratio = zero_count / num_test) %>%
        filter(zero_ratio != 1) %>%
        filter(TEST_BLOCK == max(TEST_BLOCK)) %>%
        ungroup()
    
    # summarize Bin 1 values
    message("build_pareto: summarizing good bin values")
    good_bin_values <- test_data_serialized %>%
        filter(Bin == 1) %>% 
        select(-Serial, -Bin, -my_serial, -LOT, -MEASURE_TIME, -FILENAME) %>% 
        gather(COLUMNNAME, VALUE) %>%
        na.omit() %>% 
        group_by(COLUMNNAME) %>% 
        summarize(Min = min(VALUE, na.rm = TRUE),
                  Median = median(VALUE, na.rm = TRUE),
                  Max = max(VALUE, na.rm = TRUE))
    
    # determine first fail for each unit
    message("build_pareto: determining first failing test for each DUT")
    first_fail <- test_data_labelled_long %>%
        semi_join(fail_test_block, by = "my_serial") %>%
        na.omit() %>%
        filter(FAIL == TRUE) %>%
        left_join(good_bin_values, by = "COLUMNNAME") %>%
        filter(!(Min == 0 & Median == 0 & Max == 0 & VALUE == 0)) %>%
        filter(VALUE != Median) %>%
        group_by(my_serial) %>%
        filter(COLUMNNAME == min(COLUMNNAME)) %>%
        ungroup()
    
    # annotate serialized test data by first fail
    message("build_pareto: annotating original dataset by first fail")
    test_data_annotated <- test_data_serialized %>% 
        left_join(select(first_fail, my_serial = my_serial, `First Fail` = COLUMNNAME), by = "my_serial") %>% 
        mutate(Bin = as.factor(Bin),
               `First Fail` = as.factor(`First Fail`)) %>% 
        select(LOT, MEASURE_TIME, Bin, `First Fail`, everything(), -my_serial)
    
    # return value
    annotated_datalog <- list(testData = test_data_annotated,
                              spec = spec)
    
    return(annotated_datalog)
    
}