# text cleaning script using trinker/textclean library
# see https://github.com/trinker/textclean#functions for instructions
# Tina Keil, t.keil@lancaster.ac.uk, February 2022

# data can be big, so increase java heap but note that R will crash 
# if you try to load more data than you have RAM!
options(java.parameters = "-Xmx8000m") #8GB ram
options(scipen=999) #turn off scientific notation

#load required libraries
library(dplyr)
library(filesstrings)
library(data.table) #fread is much faster for reading csv
library(readr)
library(textclean)
library(beepr)

#set working directory to directory of script
path <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(path)
source("functions.R")

#define some dirs (trailing slash required)
orig_dir = "originals/" #where the originals csv to process are kept
out_dir = "cleaned/" #folder where cleaned csv files are saved

#read files in original folder into array
in_files <- list.files(path="originals", all.files=FALSE, full.names=FALSE, pattern = ".csv$")

count <- 0

for (file in in_files) {
  
  now <- start_time()
  count = count + 1
  cat(paste0("\n", count, ")", " Starting to process ", file, "...\n\n"))
  
  ############ settings ##############
  out_name <- tools::file_path_sans_ext(file)
  out_file <- paste0(out_dir,"clean_", paste0(out_name,".csv")) #name of file after cleaning
  row_start <- 1
  infilepath <- paste0(orig_dir,file)
  
  ############## process ##############
  
  #get data from csv file
  if (file.exists(infilepath)) {
    cat("- Reading csv file...\n")
    #raw_data <- read.csv(infilepath, sep=",", numerals = c("no.loss"))[ ,cols]
    raw_data <- fread(infilepath)
    num_rows <- nrow(raw_data)
  } else {
    stop("Can't find input file. Please check.")
  }
  
  url <- paste0("https://twitter.com/",raw_data$author_username,"/status/",raw_data$tweet_id)
    
  #convert date
  cat("* Converting date\n")
  pubdate = raw_data$created_at
  pubdate <- as.POSIXct(pubdate, origin="1970-01-01", tz = "Europe/London")
    
  #clean text
  cat("* Starting to process body\n")
  content <- cleantext(raw_data$tweet)
  cat("\n")
  
  #source,num_text_chars,reply_to_tweet_id,reply_to_user_id,reply_to_screen_name,
  #is_quote,is_retweet,reply_count,retweet_count,like_count,quote_count,lang
  
  source <- raw_data$source
 
  #count chars
  orig_char_num <- raw_data$num_text_chars
  clean_raw_char_num <- str_count(content)
  stripped_clean_content <- gsub("subHashtag|subURLaddress|subTwittername|subEmailaddress","",content)
  clean_strip_char_num <- str_count(stripped_clean_content)
  
  reply_to_tweet_id <- raw_data$reply_to_tweet_id
  reply_to_user_id <- raw_data$reply_to_user_id
  reply_to_screen_name <- raw_data$reply_to_screen_name
  is_quote <- raw_data$is_quote
  is_retweet <- raw_data$is_retweet
  reply_count <- raw_data$reply_count
  retweet_count <- raw_data$retweet_count
  like_count <- raw_data$like_count
  quote_count <- raw_data$quote_count
  lang <- raw_data$lang
  
  #add to new data frame
  clean_data <- data.frame(url,content,pubdate,source,orig_char_num,clean_raw_char_num,clean_strip_char_num,reply_to_tweet_id,reply_to_user_id,reply_to_screen_name,is_quote,is_retweet,reply_count,retweet_count,like_count,quote_count,lang)

  #save it to the clean directory
  save2file(clean_data, out_name, out_file)
  
  show_alltime(now)
  beep(1)
}
