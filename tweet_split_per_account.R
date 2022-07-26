# Find all tweets with certain search term and save as new subset,
# full set as well as grouped by twitter account name
# Tina Keil, t.keil@lancaster.ac.uk, April 2022

options(java.parameters = "-Xmx8000m") #8GB ram
options(scipen=999) #turn off scientific notation

#load required libraries
library(dplyr)
library(data.table) #fread is much faster for reading csv
library(readr)

#set working directory to directory of script
path = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(path)

############ END OF SETTINGS #############

#define some dirs (trailing slash required)
dir = "cleaned/" #path to input file
file = "all_tweets_en_only.csv"
filepath <- paste0(dir,file)

############## process ##############

if (file.exists(filepath)) {
  data <- fread(filepath)
} else {
  stop("Can't find input file(s). Please check.")
}

accounts <- unique(data$account)

for (acc in accounts) {
  adata <- data[data$account == acc,]
  #save new csv per account
  write.csv(adata, paste0(dir, "clean_", acc,".csv"), row.names = FALSE)
}
