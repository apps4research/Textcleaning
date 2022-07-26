#Tina Keil, t.keil@lancaster.ac.uk, February 2022

#need to use custom replace_emoticon function as built-in has problems
replace_emoticon_new <- function (x, emoticon_dt = lexicon::hash_emoticons, ...) {
  regex_escape <- function(string) {
    gsub("([][{}()+*^${|\\\\?.])", "\\\\\\1", string)
  }
  
  stringi::stri_replace_all(x, 
                            regex = paste0("\\s+", regex_escape(emoticon_dt[["x"]])),
                            replacement = paste0(" ", emoticon_dt[['y']]),   
                            vectorize_all = FALSE)
}

convdate <- function(date) {
  dateout <- gsub(" ([0-9]) ([a-z]m)"," \\1:00 \\2", date)
  suppressWarnings(test <- dmy_hm(dateout))
  if (is.na(test)) {
    dateout <- dmy_hm(substr(date,1,nchar(date)-3))
  } else {
    dateout <- dmy_hm(dateout)
  }
  return(dateout)
}

cleantext <- function(input) {
  #don't change the order, it may cause problems
  
  now <- start_time()
  cat("- Removing extra line breaks...")
  out <- gsub(' ', ' ', input, perl=TRUE) #replace non-breaking spaces with normal spaces
  out <- gsub(' \n', '\n', out, perl=TRUE) #remove space before line break
  out <- gsub('\n\n\n', '\n\n', out, perl=TRUE) #replace 3 line-breaks by one
  out <- str_replace_all(out, '\n\n', '. ') #replace any left of double line-break with one
  show_subtime(now)
  
  now <- start_time()
  cat("- Fixing money, saints and doctors...")
  out <- gsub("€", "Euro", out, perl=TRUE)
  out <- gsub("(\\£)(\\d*,\\d*|\\d*)","\\2 pounds", out, perl=TRUE) #exchange pounds sign with pound word after amount
  out <- gsub("(\\$)(\\d*,\\d*|\\d*)","\\2 dollars", out, perl=TRUE) #exchange dollar sign with dollar word after amount
  out <- gsub(" St. ", " St ", out, perl=TRUE) #to eliminate problems with names
  out <- gsub(" Dr. ", " ", out, perl=TRUE)
  out <- gsub(" Dr ", " ", out, perl=TRUE)
  show_subtime(now)
  
  now <- start_time()
  cat("- Removing photo credits in first line...")
  out <- gsub("^(Photo|Image)\\s(by|from|courtesy of)([^.?!]*[.?!])", "", out, perl=TRUE) #remove first sentence beginning with photo by
  out <- gsub("^\\. ","",out, perl=TRUE)
  show_subtime(now)

  now <- start_time()
  cat("- Removing extra white spaces...")
  out <- replace_white(out) #replace regex white space characters
  show_subtime(now)
  
  now <- start_time()
  cat("- Replacing non-ascii chars with a text representation...")
  out <- replace_non_ascii(out) #replace non-ASCII with equivalent or remove
  show_subtime(now)
  
  now <- start_time()
  cat("- Replacing internet slang with normal words (be patient)...")
  replace_internet_slang(out)
  show_subtime(now)
  
  now <- start_time()
  cat("- Replacing Twitter handles...")
  out <- str_replace_all(out, "\\B\\@\\w+", "subTwittername") #replace Twitter style handle tag (e.g., @TrinkerJohn)
  show_subtime(now)

  now <- start_time()
  cat("- Replacing Hashtags...")
  out <- replace_hash(out, replacement = "subHashtag") #replace Twitter style hash tags (e.g., #rstats)
  show_subtime(now)
  
  now <- start_time()
  cat("- Replacing URLs...")
  out <- replace_url(out, replacement = "subURLaddress") #replace URLs
  show_subtime(now)
  
  now <- start_time()
  cat("- Replacing Email addresses...")
  out <- replace_email(out, replacement = "subEmailaddress") #replace email addresses
  show_subtime(now)
  
  now <- start_time()
  cat("- Replacing Emjois with word form equivalents (be patient)...")
  out <- replace_emoji(out)
  show_subtime(now)
  
  now <- start_time()
  cat("- Replacing Emoticons with word form equivalents...")
  out <- replace_emoticon_new(out)
  show_subtime(now)
  
  now <- start_time()
  cat("- Removing HTML tags...")
  out <- replace_html(out)
  show_subtime(now)

  now <- start_time()
  cat("- Fixing bits and pieces and replacing abbreviations...")
  out <- gsub("NULL, not void", "", out, perl=TRUE)
  out <- gsub(" & ", " and ", out, perl=TRUE)
  out <- gsub(" w/ ", " with ", out, perl=TRUE)
  out <- gsub(" b/ ", " between ", out, perl=TRUE)
  out <- gsub(" 'cause ", " because ", out, perl=TRUE)
  out <- gsub(" an' ", " and ", out, perl=TRUE)
  out <- gsub(" 'n ", " and ", out, perl=TRUE)
  out <- gsub(" mos\\b", " months", out, perl=TRUE)
  out <- gsub(" sec\\b", " second", out, perl=TRUE)
  out <- gsub(" secs\\b", " seconds", out, perl=TRUE)
  out <- gsub(" Rm\\b", " Room", out, perl=TRUE)
  out <- gsub(" Part 1 \\| Part 2 \\| Part 3", "", out, perl=TRUE)
  out <- gsub(" @ ", " at ", out, perl=TRUE)
  out <- gsub("%", " percent", out, perl=TRUE)
  out <- gsub("\\.{3,}", "…", out, perl=TRUE) #replace 3 dots with proper ellipses char
  show_subtime(now)

  now <- start_time()
  cat("- Fixing spaces, sentence endings and fractions...")
  out <- gsub("\\. \\.", "\\. ", out, perl=TRUE) #replace 2 full stops with one
  out <- gsub("Euro([0-9])","Euro \\1", out, perl=TRUE) #add a space between Euro and the number in case there isn't one
  out <- gsub("([^:0-90-9\\s.\\s0-9])(\\?|\\!|:)(\\w)","\\1\\2 \\3", out, perl=TRUE) #add space behind any question or exclamation mark or double colon without a space, except if its a time.
  out <- gsub("([^\\d{1}])(\\.)(\\w[^\\.|\\,])","\\1\\2 \\3", out, perl=TRUE) #add full stop behind any sentence without a space.
  out <- gsub("([A-Za-z]):([A-Za-z])","\\1: \\2", out, perl=TRUE) #add space between double colon if none
  out <- gsub(")([A-Z0-9])", "). \\1", out) #add full stop and space after bracket followed by capital letter if none.
  out <- str_replace_all (out, "[^\\d{2}\\/\\d{2}\\/\\d{4}](?:[1-9][0-9]*|0)\\/[1-9][0-9]*", "") #remove any fractions, exclude dates
  out <- gsub("\\s\\s", " ", out, perl=TRUE) #replace double spaces with single space
  show_subtime(now)
  
  now <- start_time()
  cat("- Fixing space before commas...")
  out <- add_comma_space(out) #Replace non-space after comma
  out <- gsub("(\\d),\\s(\\d*)\\s","\\1,\\2 ", out, perl=TRUE) #compensate adding comma for large numbers
  show_subtime(now)
  
  now <- start_time()
  cat("- Final fix of periods, colons and full stops...")
  out <- gsub("\\.\\.","\\.", out, perl=TRUE) #remove double period that might occur from previous replacement
  out <- gsub("\\. \\.", "\\. ", out, perl=TRUE) #replace 2 full stops with one again if not caught above
  out <- gsub("\\s\\s", " ", out, perl=TRUE) #replace double spaces with single space if not caught above
  out <- gsub("\\:\\.","\\:", out, perl=TRUE) #remove full stop after double colon - don't move from here
  show_subtime(now)
  
  now <- start_time()
  cat("- Removing multiple quotes...")
  out <- gsub('\\"+','\\"', out, perl=TRUE) #replace multiple quotes with one
  show_subtime(now)

  return (out)
}

content2author <- function(content) {
  now <- start_time()
  cat("- Extracting author from blog content")
  author <- gsub("^(By)\\s(\\w+)\\s(\\w+)([^.?!]*[.?!])(.+)","\\2 \\3", content, perl=TRUE)
  author <- gsub("^(By)\\s(NA)([^.?!]*[.?!])(.+)","\\2", author, perl=TRUE)
  author <- sub("\\s\\.","",author, perl=TRUE)
  show_subtime(now)
  return(author)
}

body_no_author <- function(data) {
  now <- start_time()
  cat("- Remove author(s) from blog content")
  data <- gsub("^(By)\\s(\\w+\\s\\w+)([^.?!]*[.?!])(\\s)(.+)","\\5", data, perl=TRUE)
  data <- gsub("^(By)\\s(NA)([^.?!]*[.?!])(\\s)(.+)", "\\5", data, perl=TRUE)
  show_subtime(now)
  return (data)
}

fullauthor <- function(content) {
  now <- start_time()
  cat("- Extracting all authors from blog content")
  fauthor <- gsub("^(By\\s)*([^.?!]*[.?!])(.+)","\\2", content, perl=TRUE)
  fauthor <- sub("NA.","NA", fauthor)
  show_subtime(now)
  return(fauthor)
}

show_alltime <- function(start) {
  time <- paste0(round(as.numeric(difftime(time1 = Sys.time(), time2 = start, units = "secs")), 3))
  cat(paste0("Finished processing ", file, " [", convert_secs(time) ,"]\n"))
}

show_subtime <- function(start) {
  time <- round(as.numeric(difftime(time1 = Sys.time(), time2 = start, units = "secs")), 3)
  writeLines(paste0(" done [", convert_secs(time) ,"]"))
}

start_time <- function() {
  return(Sys.time())
}

convert_secs <- function(seconds) {
  secs = as.integer(seconds)
  hour = min = 0
  if (secs >= 60) { min = floor(secs/60) }
  if (min >= 60) { hour = floor(min/60) }
  secs_left = secs - (min*60)
  if (secs_left < 10 ) { secs_left = str_pad(secs_left, 2, pad = "0") }
  if (min < 10) { min = str_pad(min, 2, pad = "0") }
  if (hour < 10 ) { hour = str_pad(hour, 2, pad = "0") }
  if (hour == "00" & min == "00" & secs_left == "00") {
    nice_time = paste0(hour, ":", min, ":", secs_left, " ", seconds)
  } else {
    nice_time = paste0(hour, ":", min, ":", secs_left)
  }
  return(nice_time)
}

save2file <- function(data, out_name, out_file) {
  if (is.data.frame(data)) {
    cat("-----------------------------------------------------------------------\n")
    cat(paste0("Writing cleaned csv file ",out_name,".csv\n"))
    write.csv(data, out_file)
  }
}