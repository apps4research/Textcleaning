# Find all tweets with certain search terms (using OR) and save as new subset,
# full set as well as grouped by twitter account name
# Tina Keil, t.keil@lancaster.ac.uk, April 2022

options(java.parameters = "-Xmx8000m") #8GB ram
options(scipen=999) #turn off scientific notation

#load required libraries
library(dplyr)
library(filesstrings)
library(data.table) #fread is much faster for reading csv
library(readr)

#set working directory to directory of script
path = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(path)

############# SET THIS ###################

#search terms
search_term1 = "RSE" 
search_term2 = "research software"

############ END OF SETTINGS #############

#define some dirs (trailing slash required)
dir = "cleaned/" #path to input file
file = "all_tweets_en_only.csv"
filepath <- paste0(dir,file)
out_dir = "cleaned/subsets/"

#search regex using OR
search_regex = paste0("(?:^|\\W)",search_term1,"|", search_term2,"(?:$|\\W)")

#create dir if it does not already exist
create_dir(out_dir)

############## process ##############

if (file.exists(filepath)) {
  data <- fread(filepath)
} else {
  stop("Can't find input file(s). Please check.")
}

data$content = gsub('\\"+','\\"', data$content, perl=TRUE) #replace multiple quotes with one

#find all content with search term and add to new data.frame
subset_data <- data %>% 
  filter(str_detect(content, regex(search_regex, ignore_case=TRUE))) #case insensitive

#save complete subset in csv
write.csv(subset_data, paste0(out_dir, "clean_subset_all_tweets.csv"), row.names = FALSE)

#save csv as subset grouped by account
by(subset_data, subset_data$account, FUN=function(i) 
  write.csv(i, paste0(out_dir, "clean_subset_", i$account[1], ".csv"), row.names = FALSE))
