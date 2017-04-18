# Check if required packages are already installed
list.of.packages <- c("shiny", "ggplot2", "dplyr", 
                      "httr", "XML", "stringr", 
                      "purrr", "readr", "lubridate", 
                      "shinyFiles", "rChoiceDialogs",
                      "DT", "tidyr", "RCurl",
                      "plotly")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
print(paste0("Packages to be installed: ", length(new.packages)))
if(length(new.packages)) {
    install.packages(new.packages)
} 

# Load required packages
library(shiny)
library(shinyFiles)
library(rChoiceDialogs)
library(DT)
library(plotly)

library(dplyr)
library(readr)
library(lubridate)
library(stringr)
library(ggplot2)
library(purrr)
library(tidyr)

library(httr)
library(XML)
library(RCurl)



# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  navbarPage("R Tools 0.3.1", windowTitle = "R Tools by Rey Masilang", position = "static-top", inverse = TRUE,
             collapsible = TRUE,
      
      ###### ARF SEARCH TOOL ################
      tabPanel("ARF Search",
               # Sidebar with text input boxes
               sidebarLayout(
                   
                   span(
                       sidebarPanel(
                           textInput("die_type", "Die Type", ""),
                           textInput("lot_number", "Lot Number", ""),
                           textInput("device_type", "Device Type", ""),
                           radioButtons("format", "Format", 
                                        choices = c("By Split Lot" = "bySplit", 
                                                    "By Wafer" = "byWafer", 
                                                    "Complete ARF Data" = "complete"
                                        )
                           ),
                           actionButton("searchButton", "Search"),
                           downloadButton("export", "Export as CSV"),
                           
                           width = 3
                       ),
                       style = "font-size:9pt"
                   ),
                   
                   
                   # Show interactive table of search results
                   mainPanel(
                       span(DT::dataTableOutput("table"), style = "font-size:8pt")
                       
                   )
            )
        ),
      
      ###### DATALOG DOWNLOADER  ############
      tabPanel("Datalog Downloader",
               
               sidebarLayout(
                   sidebarPanel(
                       span(dateRangeInput('dateRange',
                                          label = 'Date range',
                                          start = Sys.Date() - 2, end = Sys.Date() + 2
                           ),
                           textInput("filename", "File Name"),
                           selectInput("file_type", "File Type",
                                        choices = list("ALL" = "ALL",
                                                       "ASCIIDL_LTX" = "ASCIIDL_LTX",
                                                       "ASML" = "ASML",
                                                       "CANON" = "CANON",
                                                       "DL4" = "DL4",
                                                       "E142" = "E142",
                                                       "FABPT" = "FABPT",
                                                       "G85" = "G85",
                                                       "GAGE" = "GAGE",
                                                       "GMS" = "GMS",
                                                       "INF" = "INF",
                                                       "INLINE" = "INLINE",
                                                       "JAZZ" = "JAZZ",
                                                       "KLAASCII" = "KLAASCII",
                                                       "KLADEFECT" = "KLADEFECT",
                                                       "KVD" = "KVD",
                                                       "KVD_FT" = "KVD_FT",
                                                       "KVDCSV" = "KVDCSV",
                                                       "LOG_EAGLE" = "LOG_EAGLE",
                                                       "LS4" = "LS4",
                                                       "LWM" = "LWM",
                                                       "MH_XML" = "MH_XML",
                                                       "MMIO" = "MMIO",
                                                       "MVXML_CATALYST" = "MVXML_CATALYST",
                                                       "MVXML_DUO" = "MVXML_DUO",
                                                       "MVXML_FLEX" = "MVXML_FLEX",
                                                       "MVXML_KTS" = "MVXML_KTS",
                                                       "MVXML_LTX" = "MVXML_LTX",
                                                       "MVXML_MAVERICK" = "MVXML_MAVERICK",
                                                       "MVXML_SZM3650" = "MVXML_SZM3650",
                                                       "MVXML_TMT" = "MVXML_TMT",
                                                       "MVXML_WINBATH" = "MVXML_WINBATH",
                                                       "SAF" = "SAF",
                                                       "SEPROBE" = "SEPROBE",
                                                       "SINF" = "SINF",
                                                       "SMDF" = "SMDF",
                                                       "SPD" = "SPD",
                                                       "SQLITE" = "SQLITE",
                                                       "STDF_93000" = "STDF_93000",
                                                       "STDF_A5" = "STDF_A5",
                                                       "STDF_CATALYST" = "STDF_CATALYST",
                                                       "STDF_EAGLE" = "STDF_EAGLE",
                                                       "STDF_FLEX" = "STDF_FLEX",
                                                       "STDF_FLEX_GEX" = "STDF_FLEX_GEX",
                                                       "STDF_FLEX_GK" = "STDF_FLEX_GK",
                                                       "STDF_J750" = "STDF_J750",
                                                       "STDF_KALOS" = "STDF_KALOS",
                                                       "STDF_KVD" = "STDF_KVD",
                                                       "STDF_LTX" = "STDF_LTX",
                                                       "STDF_LTX_GEX" = "STDF_LTX_GEX",
                                                       "STDF_MAVERICK" = "STDF_MAVERICK",
                                                       "STDF_MAVERICK_GEX" = "STDF_MAVERICK_GEX",
                                                       "STDF_MAVERICK_GK" = "STDF_MAVERICK_GK",
                                                       "STDF_SPEA" = "STDF_SPEA",
                                                       "STDF_SZ" = "STDF_SZ",
                                                       "STDF_TMT" = "STDF_TMT",
                                                       "STDF_TMT_GEX" = "STDF_TMT_GEX",
                                                       "STDF_TMT_GK" = "STDF_TMT_GK",
                                                       "STRIP" = "STRIP",
                                                       "SUMM_93000" = "SUMM_93000",
                                                       "SUMM_A5" = "SUMM_A5",
                                                       "SUMM_FLEX" = "SUMM_FLEX",
                                                       "SUMM_J750" = "SUMM_J750",
                                                       "SZ_TXT" = "SZ_TXT",
                                                       "TMTBIN" = "TMTBIN",
                                                       "TMTLSR" = "TMTLSR",
                                                       "TSKBIN" = "TSKBIN",
                                                       "TSMC" = "TSMC",
                                                       "TXT_DLOG_J750" = "TXT_DLOG_J750",
                                                       "VPM" = "VPM",
                                                       "XML" = "XML"
                                                       
                                        ),
                                        selected = "ALL"),
                            selectInput("test_step", "Step",
                                        choices = list("ALL" = "ALL",
                                                       "AIR_CAL" = "AIR_CAL",
                                                       "AUCAL_COLD" = "AUCAL_COLD",
                                                       "AUCAL_HOT" = "AUCAL_HOT",
                                                       "AUCAL_ROOM" = "AUCAL_ROOM",
                                                       "BLIND" = "BLIND",
                                                       "BLINDMAP" = "BLINDMAP",
                                                       "CHAR" = "CHAR",
                                                       "CHAR_COLD" = "CHAR_COLD",
                                                       "CHAR_HOT" = "CHAR_HOT",
                                                       "CHAR_ROOM" = "CHAR_ROOM",
                                                       "COMP" = "COMP",
                                                       "DC_PROBE" = "DC_PROBE",
                                                       "DC_PROBE_COMP" = "DC_PROBE_COMP",
                                                       "DELETE" = "DELETE",
                                                       "FQC_VISUAL" = "FQC_VISUAL",
                                                       "FQC_VISUAL_COMP" = "FQC_VISUAL_COMP",
                                                       "FQC_VISUAL_PAT" = "FQC_VISUAL_PAT",
                                                       "FQC_VISUAL_PAT_COMP" = "FQC_VISUAL_PAT_COMP",
                                                       "FT1" = "FT1",
                                                       "FT2" = "FT2",
                                                       "FT_COLD" = "FT_COLD",
                                                       "FT_COLD1" = "FT_COLD1",
                                                       "FT_COLD1_R" = "FT_COLD1_R",
                                                       "FT_COLD2" = "FT_COLD2",
                                                       "FT_COLD2_R" = "FT_COLD2_R",
                                                       "FT_COLD3" = "FT_COLD3",
                                                       "FT_COLD3_R" = "FT_COLD3_R",
                                                       "FT_COLD_R" = "FT_COLD_R",
                                                       "FT_HOT" = "FT_HOT",
                                                       "FT_HOT1" = "FT_HOT1",
                                                       "FT_HOT1_R" = "FT_HOT1_R",
                                                       "FT_HOT2" = "FT_HOT2",
                                                       "FT_HOT2_R" = "FT_HOT2_R",
                                                       "FT_HOT3" = "FT_HOT3",
                                                       "FT_HOT3_R" = "FT_HOT3_R",
                                                       "FT_HOT_R" = "FT_HOT_R",
                                                       "FT_ROOM" = "FT_ROOM",
                                                       "FT_ROOM1" = "FT_ROOM1",
                                                       "FT_ROOM1_R" = "FT_ROOM1_R",
                                                       "FT_ROOM2" = "FT_ROOM2",
                                                       "FT_ROOM2_R" = "FT_ROOM2_R",
                                                       "FT_ROOM3" = "FT_ROOM3",
                                                       "FT_ROOM3_R" = "FT_ROOM3_R",
                                                       "FT_ROOMHOT" = "FT_ROOMHOT",
                                                       "FT_ROOMHOT1" = "FT_ROOMHOT1",
                                                       "FT_ROOMHOT1_R" = "FT_ROOMHOT1_R",
                                                       "FT_ROOMHOT2" = "FT_ROOMHOT2",
                                                       "FT_ROOMHOT2_R" = "FT_ROOMHOT2_R",
                                                       "FT_ROOMHOT3" = "FT_ROOMHOT3",
                                                       "FT_ROOMHOT3_R" = "FT_ROOMHOT3_R",
                                                       "FT_ROOMHOT_R" = "FT_ROOMHOT_R",
                                                       "FT_ROOM_R" = "FT_ROOM_R",
                                                       "FT_UNKNOWN" = "FT_UNKNOWN",
                                                       "GAGE" = "GAGE",
                                                       "HW_CHECKER" = "HW_CHECKER",
                                                       "INS_BUMP" = "INS_BUMP",
                                                       "INS_BUMP_COMP" = "INS_BUMP_COMP",
                                                       "INS_BUMP_PAT" = "INS_BUMP_PAT",
                                                       "INS_BUMP_PAT_COMP" = "INS_BUMP_PAT_COMP",
                                                       "INS_CAP" = "INS_CAP",
                                                       "INS_CAP_COMP" = "INS_CAP_COMP",
                                                       "INS_EWLB" = "INS_EWLB",
                                                       "INS_EWLB_COMP" = "INS_EWLB_COMP",
                                                       "INS_FAB" = "INS_FAB",
                                                       "INS_FAB_COMP" = "INS_FAB_COMP",
                                                       "INS_MEMS" = "INS_MEMS",
                                                       "INS_MEMS_COMP" = "INS_MEMS_COMP",
                                                       "INS_POSTCOAT" = "INS_POSTCOAT",
                                                       "INS_POSTCOAT_COMP" = "INS_POSTCOAT_COMP",
                                                       "INS_PRECOAT" = "INS_PRECOAT",
                                                       "INS_PRECOAT_COMP" = "INS_PRECOAT_COMP",
                                                       "INS_SAW" = "INS_SAW",
                                                       "INS_SKELETON" = "INS_SKELETON",
                                                       "INS_SORT" = "INS_SORT",
                                                       "INS_SORT_COMP" = "INS_SORT_COMP",
                                                       "INS_SORT_PAT" = "INS_SORT_PAT",
                                                       "INS_SORT_PAT_COMP" = "INS_SORT_PAT_COMP",
                                                       "INS_WAFER" = "INS_WAFER",
                                                       "IQC_VISUAL" = "IQC_VISUAL",
                                                       "IQC_VISUAL_COMP" = "IQC_VISUAL_COMP",
                                                       "IQC_VISUAL_PAT" = "IQC_VISUAL_PAT",
                                                       "IQC_VISUAL_PAT_COMP" = "IQC_VISUAL_PAT_COMP",
                                                       "LSR" = "LSR",
                                                       "LSR1" = "LSR1",
                                                       "LSR1_PAT" = "LSR1_PAT",
                                                       "LSR1_PAT_COMP" = "LSR1_PAT_COMP",
                                                       "LSR2" = "LSR2",
                                                       "LSR2_PAT" = "LSR2_PAT",
                                                       "LSR2_PAT_COMP" = "LSR2_PAT_COMP",
                                                       "LSR_COMP" = "LSR_COMP",
                                                       "LSR_INS" = "LSR_INS",
                                                       "LSR_INS_COMP" = "LSR_INS_COMP",
                                                       "LSR_INS_PAT" = "LSR_INS_PAT",
                                                       "LSR_INS_PAT_COMP" = "LSR_INS_PAT_COMP",
                                                       "LSR_KVD" = "LSR_KVD",
                                                       "LSR_PAT" = "LSR_PAT",
                                                       "LSR_PAT_COMP" = "LSR_PAT_COMP",
                                                       "LSR_PWR" = "LSR_PWR",
                                                       "MANUAL_INK" = "MANUAL_INK",
                                                       "MASK" = "MASK",
                                                       "MASK_COMP" = "MASK_COMP",
                                                       "MATLHIST" = "MATLHIST",
                                                       "MPE_PROBE_PAT" = "MPE_PROBE_PAT",
                                                       "MPE_PROBE_PAT_COMP" = "MPE_PROBE_PAT_COMP",
                                                       "MPE_PST_BUMP_AV_PAT" = "MPE_PST_BUMP_AV_PAT",
                                                       "MPE_PST_BUMP_AV_PAT_COMP" = "MPE_PST_BUMP_AV_PAT_COMP",
                                                       "MPE_PST_BUMP_PAT" = "MPE_PST_BUMP_PAT",
                                                       "MPE_PST_BUMP_PAT_COMP" = "MPE_PST_BUMP_PAT_COMP",
                                                       "PNP_PICKMAP" = "PNP_PICKMAP",
                                                       "PNP_PICKMAP_COMP" = "PNP_PICKMAP_COMP",
                                                       "PRE_BAKE" = "PRE_BAKE",
                                                       "PRE_BAKE_COMP" = "PRE_BAKE_COMP",
                                                       "PRE_BAKE_PAT" = "PRE_BAKE_PAT",
                                                       "PRE_BAKE_PAT_COMP" = "PRE_BAKE_PAT_COMP",
                                                       "PRE_BUMP" = "PRE_BUMP",
                                                       "PRE_BUMP_PAT" = "PRE_BUMP_PAT",
                                                       "PRE_BUMP_PAT_COMP" = "PRE_BUMP_PAT_COMP",
                                                       "PRE_BURN" = "PRE_BURN",
                                                       "PRE_LSR" = "PRE_LSR",
                                                       "PRE_LSR_COMP" = "PRE_LSR_COMP",
                                                       "PRE_LSR_PAT" = "PRE_LSR_PAT",
                                                       "PRE_LSR_PAT_COMP" = "PRE_LSR_PAT_COMP",
                                                       "PROBE" = "PROBE",
                                                       "PROBE_2" = "PROBE_2",
                                                       "PROBE_2_COMP" = "PROBE_2_COMP",
                                                       "PROBE_2_PAT" = "PROBE_2_PAT",
                                                       "PROBE_2_PAT_COMP" = "PROBE_2_PAT_COMP",
                                                       "PROBE_3" = "PROBE_3",
                                                       "PROBE_3_COMP" = "PROBE_3_COMP",
                                                       "PROBE_3_PAT" = "PROBE_3_PAT",
                                                       "PROBE_3_PAT_COMP" = "PROBE_3_PAT_COMP",
                                                       "PROBE_COMP" = "PROBE_COMP",
                                                       "PROBE_PAT" = "PROBE_PAT",
                                                       "PROBE_PAT_COMP" = "PROBE_PAT_COMP",
                                                       "PST_BAKE" = "PST_BAKE",
                                                       "PST_BAKE2" = "PST_BAKE2",
                                                       "PST_BAKE2_COMP" = "PST_BAKE2_COMP",
                                                       "PST_BAKE2_PAT" = "PST_BAKE2_PAT",
                                                       "PST_BAKE2_PAT_COMP" = "PST_BAKE2_PAT_COMP",
                                                       "PST_BAKE_COMP" = "PST_BAKE_COMP",
                                                       "PST_BAKE_PAT" = "PST_BAKE_PAT",
                                                       "PST_BAKE_PAT_COMP" = "PST_BAKE_PAT_COMP",
                                                       "PST_BUMP" = "PST_BUMP",
                                                       "PST_BUMP2" = "PST_BUMP2",
                                                       "PST_BUMP2_COMP" = "PST_BUMP2_COMP",
                                                       "PST_BUMP2_PAT" = "PST_BUMP2_PAT",
                                                       "PST_BUMP2_PAT_COMP" = "PST_BUMP2_PAT_COMP",
                                                       "PST_BUMP3" = "PST_BUMP3",
                                                       "PST_BUMP3_COMP" = "PST_BUMP3_COMP",
                                                       "PST_BUMP3_PAT" = "PST_BUMP3_PAT",
                                                       "PST_BUMP3_PAT_COMP" = "PST_BUMP3_PAT_COMP",
                                                       "PST_BUMP_AV" = "PST_BUMP_AV",
                                                       "PST_BUMP_AV_COMP" = "PST_BUMP_AV_COMP",
                                                       "PST_BUMP_AV_PAT" = "PST_BUMP_AV_PAT",
                                                       "PST_BUMP_AV_PAT_COMP" = "PST_BUMP_AV_PAT_COMP",
                                                       "PST_BUMP_COMP" = "PST_BUMP_COMP",
                                                       "PST_BUMP_PAT" = "PST_BUMP_PAT",
                                                       "PST_BUMP_PAT_COMP" = "PST_BUMP_PAT_COMP",
                                                       "PST_BURN" = "PST_BURN",
                                                       "PST_EWLB" = "PST_EWLB",
                                                       "PST_EWLB_COMP" = "PST_EWLB_COMP",
                                                       "PST_EWLB_PAT" = "PST_EWLB_PAT",
                                                       "PST_EWLB_PAT_COMP" = "PST_EWLB_PAT_COMP",
                                                       "PST_UV" = "PST_UV",
                                                       "PST_UV_COMP" = "PST_UV_COMP",
                                                       "PST_UV_PAT" = "PST_UV_PAT",
                                                       "PST_UV_PAT_COMP" = "PST_UV_PAT_COMP",
                                                       "PT" = "PT",
                                                       "QA_COLD" = "QA_COLD",
                                                       "QA_COLD1" = "QA_COLD1",
                                                       "QA_COLD1_R" = "QA_COLD1_R",
                                                       "QA_COLD2" = "QA_COLD2",
                                                       "QA_COLD2_R" = "QA_COLD2_R",
                                                       "QA_COLD3" = "QA_COLD3",
                                                       "QA_COLD3_R" = "QA_COLD3_R",
                                                       "QA_COLD_R" = "QA_COLD_R",
                                                       "QA_HOT" = "QA_HOT",
                                                       "QA_HOT1" = "QA_HOT1",
                                                       "QA_HOT1_R" = "QA_HOT1_R",
                                                       "QA_HOT2" = "QA_HOT2",
                                                       "QA_HOT2_R" = "QA_HOT2_R",
                                                       "QA_HOT3" = "QA_HOT3",
                                                       "QA_HOT3_R" = "QA_HOT3_R",
                                                       "QA_HOT_R" = "QA_HOT_R",
                                                       "QA_ROOM" = "QA_ROOM",
                                                       "QA_ROOM1" = "QA_ROOM1",
                                                       "QA_ROOM1_R" = "QA_ROOM1_R",
                                                       "QA_ROOM2" = "QA_ROOM2",
                                                       "QA_ROOM2_R" = "QA_ROOM2_R",
                                                       "QA_ROOM3" = "QA_ROOM3",
                                                       "QA_ROOM3_R" = "QA_ROOM3_R",
                                                       "QA_ROOMHOT" = "QA_ROOMHOT",
                                                       "QA_ROOMHOT1" = "QA_ROOMHOT1",
                                                       "QA_ROOMHOT1_R" = "QA_ROOMHOT1_R",
                                                       "QA_ROOMHOT2" = "QA_ROOMHOT2",
                                                       "QA_ROOMHOT2_R" = "QA_ROOMHOT2_R",
                                                       "QA_ROOMHOT3" = "QA_ROOMHOT3",
                                                       "QA_ROOMHOT3_R" = "QA_ROOMHOT3_R",
                                                       "QA_ROOMHOT_R" = "QA_ROOMHOT_R",
                                                       "QA_ROOM_R" = "QA_ROOM_R",
                                                       "QC" = "QC",
                                                       "QC2" = "QC2",
                                                       "QC2_COMP" = "QC2_COMP",
                                                       "QC2_PAT" = "QC2_PAT",
                                                       "QC2_PAT_COMP" = "QC2_PAT_COMP",
                                                       "QC3" = "QC3",
                                                       "QC3_COMP" = "QC3_COMP",
                                                       "QC3_PAT" = "QC3_PAT",
                                                       "QC3_PAT_COMP" = "QC3_PAT_COMP",
                                                       "QC_COMP" = "QC_COMP",
                                                       "QC_LOTSAMPLE" = "QC_LOTSAMPLE",
                                                       "QC_LOTSAMPLE2" = "QC_LOTSAMPLE2",
                                                       "QC_LOTSAMPLE2_COMP" = "QC_LOTSAMPLE2_COMP",
                                                       "QC_LOTSAMPLE_COMP" = "QC_LOTSAMPLE_COMP",
                                                       "QC_LOTSAMPLE_FULL" = "QC_LOTSAMPLE_FULL",
                                                       "QC_LOTSAMPLE_FULL_COMP" = "QC_LOTSAMPLE_FULL_COMP",
                                                       "QC_PAT" = "QC_PAT",
                                                       "QC_PAT_COMP" = "QC_PAT_COMP",
                                                       "REL_COLD" = "REL_COLD",
                                                       "REL_HOT" = "REL_HOT",
                                                       "REL_ROOM" = "REL_ROOM",
                                                       "RF_PROBE" = "RF_PROBE",
                                                       "RF_PROBE_COMP" = "RF_PROBE_COMP",
                                                       "RF_PROBE_PAT" = "RF_PROBE_PAT",
                                                       "RF_PROBE_PAT_COMP" = "RF_PROBE_PAT_COMP",
                                                       "RST_BUMP" = "RST_BUMP",
                                                       "RST_BUMP_PAT" = "RST_BUMP_PAT",
                                                       "RST_BUMP_PAT_COMP" = "RST_BUMP_PAT_COMP",
                                                       "SAMPLE_SORT" = "SAMPLE_SORT",
                                                       "SAM_LSR" = "SAM_LSR",
                                                       "SAM_LSR_PAT" = "SAM_LSR_PAT",
                                                       "SAM_LSR_PAT_COMP" = "SAM_LSR_PAT_COMP",
                                                       "STEPPER" = "STEPPER",
                                                       "STRIP" = "STRIP",
                                                       "TAR_LSR" = "TAR_LSR",
                                                       "TAR_LSR_PAT" = "TAR_LSR_PAT",
                                                       "TAR_LSR_PAT_COMP" = "TAR_LSR_PAT_COMP",
                                                       "WAFER_FT_COLD" = "WAFER_FT_COLD",
                                                       "WAFER_FT_HOT" = "WAFER_FT_HOT",
                                                       "WAFER_FT_ROOM" = "WAFER_FT_ROOM"
                                        ),
                                        selected = "ALL"),
                            style = "font-size:9pt"),
                       br(),
                       actionButton("search_Maxvision", "Search"),
                       width = 3
                   ),
                   
                   mainPanel(
                       actionButton("download_all", "Download All Files"),
                       actionButton("download_selected", "Download Selected Files"),
                       br(), br(),
                       span(DT::dataTableOutput("datalog_table"), style = "font-size:8pt")
                   )
               )
        ),
      
      ##### SPD TEST PARETO  ###############
      tabPanel(
          title = "SPD Pareto",
          # Sidebar with text input boxes
          fluidPage(
              
              mainPanel(
                  
                  tabsetPanel(
                      type = "pills", 
                      id = "SPD_app",
                      
                      tabPanel(
                          title = "Files", 
                          value = "files_page",

                          br(),
                          actionButton("load_files", "Select Files", width = "120px"),
                          actionButton("parse_files", "Parse Files", width = "130px"),
                          br(),
                          br(),
                          span(DT::dataTableOutput("file_table"), style = "font-size:8pt"),
                          br(), br(), br(), br()
                      ),
                      
                      tabPanel(
                          title = "Pareto", 
                          value = "pareto_page",

                          br(),
                          tabsetPanel(
                              tabPanel(
                                  title = "Graphs",
                                  
                                  br(),
                                  plotOutput("bin_pareto"),
                                  br(), br(),
                                  plotOutput("test_pareto"),
                                  br(), br(), br(), br()
                              ),
                              tabPanel(
                                  title = "Tables",
                                  
                                  br(),
                                  h5("Bin Fallout Summary"),
                                  span(DT::dataTableOutput("bin_fallout_summary"), style = "font-size:8pt"),
                                  br(), br(),
                                  h5("Test Fail Data"),
                                  span(DT::dataTableOutput("test_fail_data"), style = "font-size:8pt"),
                                  br(), br(), br(), br()
                              )
                          )
                      ),
                      
                      tabPanel(
                          title = "Exploratory Graphs", 
                          value = "exploratory_graphs_page",

                          br(),
                          tabsetPanel(
                              tabPanel(
                                  title = "Distribution Plot",
                                  fluidPage(
                                      br(),
                                      fluidRow(
                                          column(
                                              width = 6,
                                              span(
                                                  selectInput("dist_plot_x_data", "Choose Test",
                                                              choices = NA),
                                                  selectInput("dist_plot_split_by", "Split By",
                                                              choices = NA),
                                                  style = "font-size:9pt"
                                              ),
                                              br(),
                                              br(),
                                              actionButton("update_plot", "Render Plot")
                                          ),
                                          column(
                                              width = 6,
                                              span(
                                                  sliderInput("dist_plot_x", "X-axis limits",
                                                              min = 0, max = 100, value = c(0,0)),
                                                  style = "font-size:9pt"
                                              )
                                          )
                                      ),
                                      br(),
                                      fluidRow(
                                          plotOutput("distribution_plot", height = "400px")
                                      )
                                  )
                              ),
                              tabPanel(
                                  title = "Trend Chart",
                                  fluidPage(
                                      br(),
                                      fluidRow(
                                          column(
                                              width = 6,
                                              span(
                                                  selectInput("trend_chart_y_data", "Y-axis data",
                                                              choices = NA),
                                                  sliderInput("trend_chart_y_limits", "Y-axis limits",
                                                              min = 0, max = 100, value = c(0,0)),
                                                  style = "font-size:9pt"
                                              ),
                                              br(),
                                              actionButton("trend_chart_update", "Render Plot")
                                          ),
                                          column(
                                              width = 6,
                                              span(
                                                  selectInput("trend_chart_sort_by", "Sort X-axis By",
                                                              choices = NA),
                                                  selectInput("trend_chart_color_by", "Color By",
                                                              choices = NA),
                                                  style = "font-size:9pt"
                                              )
                                          )
                                      ),
                                      fluidRow(
                                          plotOutput("trend_chart_output", height = "400px")
                                      )
                                  )
                              ),
                              tabPanel(
                                  title = "Scatter Plot",
                                  fluidPage(
                                      br(),
                                      fluidRow(
                                          column(
                                              width = 6,
                                              span(
                                                  selectInput("scatter_plot_x_data", "X-axis data",
                                                              choices = NA),
                                                  sliderInput("scatter_plot_x_limits", "X-axis limits",
                                                              min = 0, max = 100, value = c(0,0)),
                                                  selectInput("scatter_plot_color_by", "Color By",
                                                              choices = NA),
                                                  style = "font-size:9pt"
                                              ),
                                              br(),
                                              actionButton("scatter_plot_update", "Render Plot")
                                          ),
                                          column(
                                              width = 6,
                                              span(
                                                  selectInput("scatter_plot_y_data", "Y-axis data",
                                                              choices = NA),
                                                  sliderInput("scatter_plot_y_limits", "Y-axis limits",
                                                              min = 0, max = 100, value = c(0,0)),
                                                  style = "font-size:9pt"
                                              )
                                          )
                                      ),
                                      fluidRow(
                                          plotlyOutput("scatter_plot", height = "400px")
                                      )
                                  )
                              )
                          )
                      )
                  ),
                  br(),
                  tabsetPanel(
                      tabPanel(
                          title = "Data Filter",
                          mainPanel(
                              span(DT::dataTableOutput("raw_data_table"), style = "font-size:8pt"),
                              br(), br(), br(), br(), br(), br(), br()
                          )
                      )
                  )
              )
          )
      ),
      
      tabPanel(
          title = "Bulk Processing",
          
          fluidPage(
              mainPanel(
                  tabsetPanel(
                      type = "pills", 
                      id = "SPD_app",
                      
                      tabPanel(
                          title = "Files",
                          value = "bulk_files",
                          
                          fluidPage(
                              br(),
                              fluidRow(
                                  column(
                                      width = 3,
                                      span(
                                          
                                          selectInput("bulk_file_type", 
                                                       label = strong("File type:"),
                                                       choices = list("SPD", "STDF"),
                                                       selected = 1,
                                                       width = "100%"
                                                       ),
                                          actionButton("bulk_select_files", "Select Files", width = "120px"),
                                          br(),
                                          br(),
                                          span(DT::dataTableOutput("bulk_file_table"), style = "font-size:8pt"),
                                          br(), br(), br(), br(),
                                          style = "font-size:9pt"
                                      )
                                  )
                              )
                          )
                      ),
                      tabPanel(
                          title = "Bin Summary",
                          value = "bulk_bin_summary",
                          
                          fluidPage(
                              br(),
                              fluidRow(
                                  column(
                                      width = 3,
                                      span(
                                          selectInput("bulk_select_split_column", "Split by",
                                                      choices = c("Lot", "File")),
                                          actionButton("bulk_bin_summary_run", 
                                                       label = "Run", 
                                                       width = "100%",
                                                       icon = icon("play-circle")),
                                          style = "font-size:9pt"
                                      )
                                  ),
                                  
                                  column(
                                      width = 9,
                                      span(
                                          DT::dataTableOutput("bulk_bin_summary"), 
                                          style = "font-size:8pt"
                                      )
                                  )
                              )
                          )
                      ),
                      tabPanel(
                          title = "Test Statistics",
                          value = "bulk_test_stats",
                          
                          fluidPage(
                              br(),
                              fluidRow(
                                  column(
                                      width = 4,
                                      span(
                                          strong("Select columns to summarize:"),
                                          span(
                                              DT::dataTableOutput("bulk_test_stat_choices"), 
                                              br(),
                                              style = "font-size:8pt"
                                          ),
                                          selectInput("bulk_select_test_split", "Split by:",
                                                      choices = list("LOT",
                                                                     "FILENAME")),
                                          br(), br(), br(),
                                          
                                          style = "font-size:9pt"
                                      )
                                  ),
                                  
                                  column(
                                      width = 3,
                                      span(
                                          radioButtons("bulk_stat_choices",
                                                             label = strong("Summary Statistics:"),
                                                             choices = list("Average" = 1,
                                                                            "Std Deviation" = 2,
                                                                            "Minimum" = 3,
                                                                            "Maximum" = 4,
                                                                            "Median" = 5,
                                                                            "Range" = 6,
                                                                            "Count" = 7,
                                                                            "Sum" = 8,
                                                                            "Variance" = 9,
                                                                            "Q1" = 10,
                                                                            "Q3" = 11),
                                                             inline = FALSE,
                                                             selected = c(1)),
                                          br(),
                                          actionButton("bulk_test_stat_run", 
                                                       label = "Run", 
                                                       width = "80%",
                                                       icon = icon("play-circle")),
                                          style = "font-size:9pt"
                                      )
                                  ),
                                  
                                  column(
                                      width = 5,
                                      span(
                                          br(),
                                          DT::dataTableOutput("bulk_test_summary"), 
                                          style = "font-size:8pt"
                                      )
                                  )
                              )
                          )
                      )
                  )
              )
          )
      )
  )
  
))
