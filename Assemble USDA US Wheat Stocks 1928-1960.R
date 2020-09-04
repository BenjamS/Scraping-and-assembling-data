library(tabulizer)
#library(rvest)
library(tidyverse)
this_dir <- "C:/Users/bensc/OneDrive/Documents/Data/"
this_folder <- "USDA Data/"
this_wd <- paste0(this_dir, this_folder)
setwd(this_wd)
#-----------------------------------------------------------------------------
# Get wheat stocks data for years 1934-1960.
# Data for 1934-38 is scraped from pg 24 of the 1952 USDA Ag Stats pdf. 
# Data for 1939-60 is scraped from pg 21 of the 1962 USDA Ag Stats pdf. 
# These pdfs can be downloaded from https://www.nass.usda.gov/Publications/Ag_Statistics/
# In the 1952 Ag Stats pdf, off farm stocks are defined as:
#"Stocks  in  interior  mills,  elevators,  and  warehouses,  merchant  mills,  commercial  stocks  at 
#terminals,  and  stocks  owned  by  Commodity  Credit  Corporation  in  steel  and  wooden  bins,  in 
#transit to ports and in Canadian elevators."
# The US wheat harvests are in Oct. and Jul., so I imagine "Beginning stocks"
# corresponds to Oct. 1 stocks, while ending stocks correspond to the Jul. 1
# stocks.
#-----------------------------------------------------------------------------
# https://www.nass.usda.gov/Publications/Ag_Statistics/
# These early "Agricultural Statistics" annual reports were put online
# in 2012 or so.
#-----------------------------------------------------------------------------
# Conversion factors (from the USDA publication)
acres_to_has <- 0.405
wheatBushels_to_mTons <- 60 * 0.454 * 1 / 1000
#-----------------------------------------------------------------------------
# Get 1934-1938 wheat stocks data
#this_dir <- "C:/Users/bensc/OneDrive/Documents/Data/"
#this_folder <- "USDA Data/"
this_file <- "Agstat-04-23-1952_1866-1950.pdf"
#this_filepath <- paste0(this_dir, this_folder, this_file)
this_filepath <- this_file
outlist <- extract_tables(this_filepath, pages = c(24), method = "stream")
#View(outlist[[1]])

df_raw <- as.data.frame(outlist[[1]])
df_raw <- df_raw %>% mutate_if(is.factor, as.character)
df_raw <- df_raw[c(8:12), ]
for(i in 1:ncol(df_raw)){
  df_raw[, i] <- gsub(' ,|, |,|-|_|\\.', '', df_raw[, i])
  df_raw[, i] <- gsub('  |    ', ' ', df_raw[, i])
  
}

list_str <- strsplit(df_raw[, 1], " ")
df_col12 <- as.data.frame(do.call(rbind, list_str))
list_str <- strsplit(df_raw[, 7], " ")
list_str[[1]] <- list_str[[1]][-1]
df_col69 <- as.data.frame(do.call(rbind, list_str))

df_stocks_34_38 <- data.frame(df_col12,
                              df_raw[, c(2, 3, 5)],
                              df_col69)

col_names <- c("Year",
               "On farm Oct 1 (1000 metric tons)",
               "On farm Jan 1 (1000 metric tons)",
               "On farm Apr 1 (1000 metric tons)",
               "On farm Jul 1 (1000 metric tons)",
               "Off farm Oct 1 (1000 metric tons)",
               "Off farm Jan 1 (1000 metric tons)",
               "Off farm Apr 1 (1000 metric tons)",
               "Off farm Jul 1 (1000 metric tons)")
colnames(df_stocks_34_38) <- col_names
df_stocks_34_38 <- df_stocks_34_38 %>% mutate_if(is.factor, as.character)
df_stocks_34_38[1, ncol(df_stocks_34_38)] <- "101838"
df_stocks_34_38[, -1] <- as.data.frame(apply(df_stocks_34_38[, -1], 2, as.numeric))
df_stocks_34_38$Year <- as.integer(df_stocks_34_38$Year)
#-----------------------------------------------------------------------------
# Now get wheat stocks for 1939-1960
this_file <- "Agstat-04-23-1962_1866-1960.pdf"
#this_filepath <- paste0(this_dir, this_folder, this_file)
this_filepath <- this_file
outlist <- extract_tables(this_filepath, pages = c(21), method = "stream")
#View(outlist[[1]])

df_raw <- as.data.frame(outlist[[1]])
df_raw <- df_raw %>% mutate_if(is.factor, as.character)
df_raw <- df_raw[-c(1:6, nrow(df_raw)), ]
for(i in 1:ncol(df_raw)){
  df_raw[, i] <- gsub(' ,|, |,|-|_|·|\\.', '', df_raw[, i])
  df_raw[, i] <- gsub('  |    ', ' ', df_raw[, i])
  df_raw[, i] <- gsub('!|l|I', '1', df_raw[, i])
  
}

list_str <- strsplit(df_raw[, 3], " ")
df_col34 <- as.data.frame(do.call(rbind, list_str))
list_str <- strsplit(df_raw[, 6], " ")
df_col78 <- as.data.frame(do.call(rbind, list_str))

df_stocks_39_60 <- data.frame(df_raw[, c(1, 2)],
                              df_col34,
                              df_raw[, c(4, 5)],
                              df_col78,
                              df_raw[, 7])

colnames(df_stocks_39_60) <- col_names
df_stocks_39_60 <- df_stocks_39_60 %>% mutate_if(is.factor, as.character)
df_stocks_39_60[, -1] <- as.data.frame(apply(df_stocks_39_60[, -1], 2, as.numeric))
df_stocks_39_60$Year <- as.integer(df_stocks_39_60$Year)
#-----------------------------------------------------------------------------
# Merge the two
df_stocks_34_60 <- as.data.frame(rbind(df_stocks_34_38,
                                       df_stocks_39_60))

# Add 1961 from the 1963 Ag stat pdf so as to be able to compare 1961 to the 
# USDA PSD stocks data
df_stocks_61 <- data.frame(1961,
                           466844,
                           359484,
                           211652,
                           102308,
                           1849986,
                           1623079,
                           1430357,
                           1202450)
colnames(df_stocks_61) <- colnames(df_stocks_34_60)
df_stocks_34_61 <- as.data.frame(rbind(df_stocks_34_60,
                                       df_stocks_61))
# Convert to metric tons
df_stocks_34_61[, -1] <- as.data.frame(apply(df_stocks_34_61[, -1], 2, function(x){x * wheatBushels_to_mTons}))

rm(df_stocks_34_38, df_stocks_34_60, df_stocks_39_60, df_stocks_61, df_raw)

write.csv(df_stocks_34_61, "USDA US Wheat stocks 1934-1961.csv", row.names = F)
