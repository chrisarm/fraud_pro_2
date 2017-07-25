# #################
# Generate OWNER_ID and then match properties to owners with the same name
###################

# Prelim
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(stringdist))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(uuid))

dat <- readRDS("ny_data.rds")
if(!exists(x = "dat")) {
  source("missing.r")
}

# Calculate the similarity between two single tokens
string_matcher <- function(word1, word2, match_rate = .87){
  match <- FALSE
  dis <- stringdist(word1, word2, method = "dl")
  if (1 - dis / max(nchar(word1), nchar(word2)) > match_rate){
    match <- TRUE
  }
  return(match)
}

#Puts names in a more easily compared format for fuzzy matching, MOAR WERK to do
fix_names <- function(name){
  # lowercase everything
  name <- tolower(name)
  
  # put last names last
  if(grepl(",", name)){
    split <- strsplit(name, ",")
    typeof(split)
    name <- paste(split[[1]][2], split[[1]][1])
  }
  
  # remove anything that isn't a letter
  name <- gsub(pattern = "[^[:alpha:]]", replacement = "", x = name, perl = T)
}

#----------------------------------------
# Matching names to group different OWNERS together
# Currently only using 10K sample since it takes a long time to run
#----------------------------------------
# dat_wrk <- sample_n(dat, 10000)
dat_wrk <- dat

dat_wrk$RECORD_UUID <- replicate(n = nrow(dat_wrk), expr = UUIDgenerate())
dat_wrk$OWNER_ID <- 0
dat_wrk[which(is.na(dat_wrk$OWNER)),]$OWNER <- "unknown"
owner_matcher <- readRDS("owner_matcher.rds")
owner_matcher[nchar(OWNER) <= 0]$OWNER <- "blank"
owner_matcher <- dat_wrk %>% select(OWNER, RECORD_UUID, OWNER_ID) %>% data.table()
owner_matcher$OWNER <- sapply(X = owner_matcher$OWNER, FUN = fix_names)
#----------------------------------------
# Loop over owners and compare them all
#----------------------------------------
match_rate <- .9

for (owner_id in 1:nrow(owner_matcher)) {
  #If OWNER_ID == 0 then it hasn't matched with a previous owner name yet and we need to check it. 
  if(owner_matcher[owner_id,]$OWNER_ID == 0){
    #Assign an OWNER_ID
    owner_matcher[owner_id,]$OWNER_ID <- owner_id
    owner <- owner_matcher[owner_id,]$OWNER
    
    #Filter to match against only the rows that have the same starting letter, same length, and that are after the current owner
    regex_pattern <- sprintf("^%s",substring(owner_matcher[owner_id,]$OWNER,1,1))
    initial_owners <- grepl(pattern = regex_pattern, x = owner_matcher$OWNER, perl = T)
    initial_owners[1:owner_id] <- FALSE
    initial_owners <- owner_matcher[which(initial_owners),] %>% filter(nchar(OWNER) == nchar(owner))
    
    matches <- sapply(X = initial_owners$OWNER, FUN =  string_matcher, word2 = owner, match_rate = match_rate)
    if(length(matches) > 0){
      if(sum(matches) > 0){
        initial_owners[which(matches),]$OWNER_ID <- owner_id
        owner_matcher[which(owner_matcher$RECORD_UUID %in% initial_owners$RECORD_UUID),]$OWNER_ID <- initial_owners$OWNER_ID
      }
    }
  }
}

# saveRDS(owner_matcher, "../../MGTA-495 Fraud Analytics/owner_matcher2.rds")