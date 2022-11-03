# Indoor Positioning System data tidying

#knitr::opts_chunk$set(
#   echo = FALSE,                 # don't show code
#   warning = FALSE,              # don't show warnings
#   message = FALSE,              # don't show messages (less serious warnings)
#   cache = FALSE,                # set to TRUE to save results from last compilation
#   fig.align = "center",         # center figures
#   fig.width = ,                 # Adjust figure width
#   fig.height = ,                # Adjust figure height
#   attr.source = '.numberLines'  # add line numbers to code
#   class.output = "numberLines"  # add line numbers to code output
# )

library(tidyverse)  # Load core packages:
# ggplot2,   for data visualization.
# dplyr,     for data manipulation.
# tidyr,     for data tidying.
# purrr,     for functional programming.
# tibble,    for tibbles, a modern re-imagining of data frames.
# stringr,   for strings.
# forcats,   for factors.
# lubridate, for date/times.
# readr,     for reading .csv, .tsv, and .fwf files.
# readxl,    for reading .xls, and .xlxs files.
# feather,   for sharing with Python and other languages.
# haven,     for SPSS, SAS and Stata files.
# httr,      for web apis.
# jsonlite   for JSON.
# rvest,     for web scraping.
# xml2,      for XML.
# modelr,    for modelling within a pipeline
# broom,     for turning models into tidy data
# hms,       for times.

library(magrittr)   # Pipeline operator
library(lobstr)     # Visualizing abstract syntax trees, stack trees, and object sizes
library(pander)     # Exporting/converting complex pandoc documents, EX: df to Pandoc table
library(ggforce)    # More plot functions on top of ggplot2
library(ggpubr)     # Automatically add p-values and significance levels  plots.
# Arrange and annotate multiple plots on the same page.
# Change graphical parameters such as colors and labels.
library(sf)         # Geo-spatial vector manipulation: points, lines, polygons
library(kableExtra) # Generate 90 % of complex/advanced/self-customized/beautiful tables
library(latex2exp)  # Latex axis titles in ggplot2
library(ellipse)    # Simultaneous confidence interval region to check C.I. of 2 slope parameters
library(plotly)     # User interactive plots

set.seed(27)        # make random results reproducible

# change working directory as needed
setwd("/home/tyrus/Projects/wifi_trilateration/")

# Data Cleaning

# Load txt files
offline_data_txt <- readLines("raw_data/offline.final.trace.txt")
online_data_txt  <- readLines("raw_data/online.final.trace.txt")

# To split the text into rows, then piece together column names and rows of data
processLine <- function(x) {
  #Regex to split data on ;=, characters (determined by looking at data)
  tokens = strsplit(x, "[;=,]")[[1]]
  #If no signals are recorded (token length is 10) then remove the row
  if (length(tokens) == 10)
    return(NULL)
  #For each signal recording, tokens 1, 3, 5, and 9 are column names
  tmp = matrix(tokens[-(1:10)], ncol=4, byrow=T)
  #Column bind column names with the other rows in matrix form
  cbind(matrix(tokens[c(2,4,6:8,10)], nrow=nrow(tmp), ncol=6, byrow=T), tmp)
}

# Run the processLine function over the entire dataset to build dataframe
offlines <- offline_data_txt[substr(offline_data_txt, 1,1) != "#"]  #Removes comments from data
offline_tmp   <- lapply(offlines, processLine)
onlines <- online_data_txt[substr(online_data_txt, 1,1) != "#"]  #Removes comments from data
online_tmp   <- lapply(onlines, processLine)

tidy_offline <- as.data.frame(do.call("rbind", offline_tmp), stringsAsFactors=F)
tidy_online  <- as.data.frame(do.call("rbind", online_tmp), stringsAsFactors=F)

# Add column names
names(tidy_offline) <- c("timestamp", "receiver", "posX", "posY", "posZ", "orientation", "ap_mac", "rssi", "frequency", "mode")
names(tidy_online) <- c("timestamp", "receiver", "posX", "posY", "posZ", "orientation", "ap_mac", "rssi", "frequency", "mode")

# Data Adjustment
# Data coercion chr -> numeric
varList <- c("timestamp", "posX", "posY", "posZ", "orientation", "rssi")
tidy_offline[varList] <- lapply(tidy_offline[varList], as.numeric)
tidy_online[varList] <- lapply(tidy_online[varList], as.numeric)

# Convert timestamp to millisecond for Unix time conversions
tidy_offline$timestamp <- tidy_offline$timestamp*0.001
tidy_offline$timestamp <- as.POSIXct(tidy_offline$time, tz="UTC", origin = "1970-01-01")

tidy_online$timestamp <- tidy_online$timestamp/1000
tidy_online$timestamp <- as.POSIXct(tidy_online$time, tz="UTC", origin = "1970-01-01")

summary(sapply(tidy_offline[,c("receiver", "frequency", "ap_mac", "mode")], as.factor))
summary(sapply(tidy_online[,c("receiver", "frequency", "ap_mac", "mode")], as.factor))

#Since the scan device and the elevation (posZ) are constant, they can be dropped
length(unique(tidy_offline$receiver))  #Output: 1
length(unique(tidy_offline$posZ))       #Output: 1
tidy_offline$scanap_mac <- NULL
tidy_offline$posZ <- NULL

length(unique(tidy_online$receiver))   #Output: 1
length(unique(tidy_online$posZ))        #Output: 1
tidy_online$scanap_mac <- NULL
tidy_online$posZ <- NULL

#Clear unneeded objects
remove(offlines)
remove(onlines)
remove(offline_data_txt)
remove(online_data_txt)

# Inspecting the length or amount of records in the dataframe
summary(tidy_offline) # note that the length is 1,181,628 records


# Inspecting the length or amount of records in the dataframe
summary(tidy_online) # note that the length is 53,303 records

# Creating a vector for the desired access points to filter out unwanted records
ap_list <- c("00:0f:a3:39:e1:c0",
             "00:14:bf:b1:97:8a",
             "00:14:bf:3b:c7:c6",
             "00:14:bf:b1:97:90",
             "00:14:bf:b1:97:8d",
             "00:14:bf:b1:97:81")

# removes unwanted records of access points and ad-hoc devices
tidy_offline <- tidy_offline[tidy_offline$ap_mac %in% ap_list, ]
# reinspecting the length or amount of records in the dataframe
summary(tidy_offline) # note that the length changed from 1,181,628 to 769,332 records

# removes unwanted records of access points and ad-hoc devices
tidy_online <- tidy_online[tidy_online$ap_mac %in% ap_list, ]
# reinspecting the length or amount of records in the dataframe
summary(tidy_online) # note that the length changed from 53,303 to 34,778 records

# Adding (x,y) access point coordinates
tidy_offline <- mutate(tidy_offline, ap_x = ifelse(ap_mac %in% "00:0f:a3:39:e1:c0", 7.5,
                                                   ifelse(ap_mac %in% "00:14:bf:b1:97:8a", 2.5,
                                                          ifelse(ap_mac %in% "00:14:bf:3b:c7:c6", 12.8,
                                                                 ifelse(ap_mac %in% "00:14:bf:b1:97:90", 1.0,
                                                                        ifelse(ap_mac %in% "00:14:bf:b1:97:8d", 33.5,
                                                                               ifelse(ap_mac %in% "00:14:bf:b1:97:81", 33.5, NA))))))) %>%
  mutate(tidy_offline, ap_y = ifelse(ap_mac %in% "00:0f:a3:39:e1:c0", 6.3,
                                     ifelse(ap_mac %in% "00:14:bf:b1:97:8a", -0.8,
                                            ifelse(ap_mac %in% "00:14:bf:3b:c7:c6", -2.8,
                                                   ifelse(ap_mac %in% "00:14:bf:b1:97:90", 14.0,
                                                          ifelse(ap_mac %in% "00:14:bf:b1:97:8d", 9.3,
                                                                 ifelse(ap_mac %in% "00:14:bf:b1:97:81", 2.8, NA)))))))
head(tidy_offline)

# Adding (x,y) access point coordinates
tidy_online <- mutate(tidy_online, ap_x = ifelse(ap_mac %in% "00:0f:a3:39:e1:c0", 7.5,
                                                 ifelse(ap_mac %in% "00:14:bf:b1:97:8a", 2.5,
                                                        ifelse(ap_mac %in% "00:14:bf:3b:c7:c6", 12.8,
                                                               ifelse(ap_mac %in% "00:14:bf:b1:97:90", 1.0,
                                                                      ifelse(ap_mac %in% "00:14:bf:b1:97:8d", 33.5,
                                                                             ifelse(ap_mac %in% "00:14:bf:b1:97:81", 33.5, NA))))))) %>%
  mutate(tidy_online, ap_y = ifelse(ap_mac %in% "00:0f:a3:39:e1:c0", 6.3,
                                    ifelse(ap_mac %in% "00:14:bf:b1:97:8a", -0.8,
                                           ifelse(ap_mac %in% "00:14:bf:3b:c7:c6", -2.8,
                                                  ifelse(ap_mac %in% "00:14:bf:b1:97:90", 14.0,
                                                         ifelse(ap_mac %in% "00:14:bf:b1:97:8d", 9.3,
                                                                ifelse(ap_mac %in% "00:14:bf:b1:97:81", 2.8, NA)))))))
head(tidy_online)

# Writing Data

# Export df to csv
write.csv(tidy_offline,"tidy_data/tidy_offline.csv", row.names = FALSE)

# Export df to csv
write.csv(tidy_online,"tidy_data/tidy_online.csv", row.names = FALSE)


## Importing Data

# Import csv
#library(readr)
#offline <- read_csv("tidy_online.csv")
#online <- read_csv("tidy_online.csv")

