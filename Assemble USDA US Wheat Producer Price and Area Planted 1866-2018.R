library(tabulizer)
#library(rvest)
library(tidyverse)
#library(pdftools)
this_dir <- "C:/Users/bensc/OneDrive/Documents/Data/"
this_folder <- "USDA Data/"
this_wd <- paste0(this_dir, this_folder)
setwd(this_wd)
#-----------------------------------------------------------------------------
# Get wheat producer price data
# Also get area planted while you're at it, since this is included in the table
#-----------------------------------------------------------------------------
# Notes
# Producer price is referred to in the Ag Stat pdfs as
# "Season average price per bushel received by farmers"
# It is an average across months and space (and weighted across space) but is
# very important because the USDA uses this to calculate price support levels ("loan" and "target").
# See "Langley et al USDA ERS cica 1984 Commodity Program Perspectives.pdf" for details.
# "Year" is the marketing year. Begins on July 1 for years 1949-1975,
# on June 1 for 1976 onward. (See footnote to Table 9 of the 1990 Ag Stat pdf.)
# % of parity is measured at beginning of year.
# https://www.nass.usda.gov/Publications/Ag_Statistics/
# The early "Agricultural Statistics" annual reports were first put online
# in 2012 or so.
#-----------------------------------------------------------------------------
# Wheat prod price 1866-1965 was retrieved along with production, yield, area, etc.,
# from 1941, 1942, and 1967 USDA Ag Stat pdfs, and saved as "USDA US Wheat 1866-1965.csv"
# Wheat prod price 1965-1980 can be retrieved from the 1982 USDA Ag Stat pdf p 11
# Wheat prod price 1975-1988 can be retrieved from the 1990 USDA Ag Stat pdf p 11
# Wheat prod price 1989-1998 can be retrieved from the 1999 USDA Ag Stat pdf p 10
# Wheat prod price 1999-2008 can be retrieved from the 2009 USDA Ag Stat pdf p 12
# Wheat prod price 2009-2018 can be retrieved from the 2019 USDA Ag Stat pdf p 9
#-----------------------------------------------------------------------------
# Conversion factors (from the USDA publication)
acres_to_has <- 0.405
wheatBushels_to_mTons <- 60 * 0.454 * 1 / 1000
# 1 bushel of wheat = 60 lbs = 27.2 kg (2019 USDA Ag Stat pdf p 6)
#=============================================================================
# Get wheat producer price data for 1866-1907
this_file <- "Agstat-04-23-1941_1866-1939.pdf"
this_filepath <- this_file
outlist <- extract_tables(this_filepath, pages = c(11), method = "stream")
#View(outlist[[1]])

df_raw <- as.data.frame(outlist[[1]])
df_raw <- df_raw %>% mutate_if(is.factor, as.character)
df_raw <- df_raw[c(3:47), ]
df_raw$V1 <- gsub("(\\d{2}) (\\d{2})", "\\1\\2", df_raw$V1)
df_raw$V1 <- gsub("(\\d{1}) (\\d{3})", "\\1\\2", df_raw$V1)
df_raw$V1 <- gsub("(\\d+)(.*)", "\\1", df_raw$V1)
ind_1879 <- which(df_raw$V1 == "879")
ind_1886 <- which(df_raw$V1 == "886")
df_raw$V1[ind_1886] <- paste0("1", df_raw$V1[ind_1886])
df_raw$V1[ind_1879] <- paste0("1", df_raw$V1[ind_1879])
ind_rm <- which(duplicated(df_raw$V1)) - 1
df_raw <- df_raw[-ind_rm, ]
replace_this <- "-|_|, | ,|,|Â·|â???¢ |'"; with_this <- ""
df_raw <- as.data.frame(apply(df_raw,
                              2, function(x) gsub(replace_this, with_this, x)))
replace_this <- '"'; with_this <- ""
df_raw <- as.data.frame(apply(df_raw,
                              2, function(x) gsub(replace_this, with_this, x)))
replace_this <- "!|I|l"; with_this <- "1"
df_raw <- as.data.frame(apply(df_raw,
                              2, function(x) gsub(replace_this, with_this, x)))
replace_this <- "S"; with_this <- "5"
df_raw <- as.data.frame(apply(df_raw,
                              2, function(x) gsub(replace_this, with_this, x)))
df_raw <- df_raw %>% mutate_if(is.factor, as.character)
ind_1878 <- which(df_raw$V1 == "1878")
df_raw[ind_1878, 8:11] <- c("150503", "2074", "150253", "33.5")
ind_1888 <- which(df_raw$V1 == "1888")
df_raw[ind_1888, c(2:3, 6)] <- c("34969 12.1", "423867", "393000 95")
ind_1897 <- which(df_raw$V1 == "1897")
df_raw[ind_1897, 8:11] <- c("221143", "2060", "220965", "36.5")
ind_1898 <- which(df_raw$V1 == "1898")
df_raw[ind_1898, 8:10] <- gsub("(\\d+) (\\d+)", "\\2", df_raw[ind_1898, 8:10])
df_raw[ind_1898, 11] <- "29.6"

list_str <- strsplit(df_raw$V2, " ")
df_cols23 <- as.data.frame(do.call(rbind, list_str))
list_str <- strsplit(df_raw$V6, " ")
df_cols67 <- as.data.frame(do.call(rbind, list_str))
df <- data.frame(df_raw$V1,
                 NA,
                 df_cols23,
                 df_raw[, c(3, 5)], 
                 df_cols67,
                 df_raw[, c(7:9)])

col_names_1866_1965 <- c("Year", "Area planted (1000 hectares)", 
                         "Area harvested (1000 hectares)", 
                         "Yield (metric tons / hectare)",
                         "Production (1000 metric tons)",
                         "Producer Price (USD / metric ton)",
                         "Farm value (1000 USD)",
                         "Kansas Cty market price (USD / metric ton)",
                         "Minneapolis market price (USD / metric ton)",
                         "Exports (1000 metric tons)",
                         "Imports (1000 metric tons)")

colnames(df) <- col_names_1866_1965
df <- as.data.frame(apply(df, 2, as.numeric))
df$Year <- as.integer(df$Year)
df_1866_1898 <- subset(df, Year <= 1898)
rm(df_cols23, df_cols67)
#-----------------------------------------------------------------------------
# Get wheat producer price data for 1898-1919
this_file <- "Agstat-04-23-1942_1899-1940.pdf"
this_filepath <- this_file
outlist <- extract_tables(this_filepath, pages = c(11), method = "stream")
#View(outlist[[1]])
df_raw <- as.data.frame(outlist[[1]])
df_raw <- df_raw %>% mutate_if(is.factor, as.character)
df_raw <- df_raw[c(4:24), ]
df_raw$V1 <- gsub("(\\d{4})(.*)", "\\1", df_raw$V1)
replace_this <- "-|_|, | ,|,|Â·|â???¢ |'"; with_this <- ""
df_raw <- as.data.frame(apply(df_raw,
                              2, function(x) gsub(replace_this, with_this, x)))
# replace_this <- "!|I|l"; with_this <- "1"
# df_raw <- as.data.frame(apply(df_raw,
#                               2, function(x) gsub(replace_this, with_this, x)))
df_raw <- df_raw %>% mutate_if(is.factor, as.character)
ind_1909 <- grep("1909", df_raw$V1)
df_raw[ind_1909, 2:5] <- c("44262 15.5", "683927", "99.1", "677726")
ind_1919 <- which(df_raw$V1 == "1919")
df_raw[ind_1919, 2:8] <- c("73700 12.9", "952097", "216.3",
                           "2059421", "227", "300", "222030 5511")

list_str <- strsplit(df_raw$V2, " ")
df_cols23 <- as.data.frame(do.call(rbind, list_str))
list_str <- strsplit(df_raw$V8, " ")
df_cols910 <- as.data.frame(do.call(rbind, list_str))

df <- data.frame(df_raw$V1,
                 NA,
                 df_cols23,
                 df_raw[, 3:7],
                 df_cols910)

colnames(df) <- col_names_1866_1965
df <- as.data.frame(apply(df, 2, as.numeric))
df$Year <- as.integer(df$Year)
df_1899_1919 <- df
df_1899_1919$`Area planted (1000 hectares)` <- 77440 #From 1967 Ag Stat pdf
rm(df_cols23, df_cols910)
#-----------------------------------------------------------------------------
# Get wheat producer price data for 1920-1965
this_file <- "Agstat-04-23-1967_1866-1965.pdf"
this_filepath <- this_file
outlist <- extract_tables(this_filepath, pages = c(10), method = "stream")
#View(outlist[[3]])

df_raw <- as.data.frame(outlist[[3]])
df_raw <- df_raw %>% mutate_if(is.factor, as.character)
replace_this <- "-|_|, | ,|,|Â·|â???¢ |'"; with_this <- ""
df_raw <- as.data.frame(apply(df_raw,
                              2, function(x) gsub(replace_this, with_this, x)))
replace_this <- "!|I|l|L"; with_this <- "1"
df_raw <- as.data.frame(apply(df_raw,
                              2, function(x) gsub(replace_this, with_this, x)))
replace_this <- '"'; with_this <- ""
df_raw <- as.data.frame(apply(df_raw,
                              2, function(x) gsub(replace_this, with_this, x)))
replace_this <- "\\. | \\."; with_this <- "\\."
df_raw <- as.data.frame(apply(df_raw,
                              2, function(x) gsub(replace_this, with_this, x)))
df_raw <- df_raw %>% mutate_if(is.factor, as.character)
df_raw$V1 <- gsub("^(1) (.*)", "\\2", df_raw$V1)
ind_1922 <- grep("1922", df_raw$V1)
df_raw[ind_1922, 1:2] <- gsub("(\\d{2}) (\\d+)", "\\1\\2", df_raw[ind_1922, 1:2])
df_raw$V1[ind_1922] <- gsub("  ", " ", df_raw$V1[ind_1922])

ind_1924 <- 5
df_raw$V1[ind_1924] <- "1924 55706"
replace_this <- "(\\d+) (\\d+)"; with_this <- "\\2"
df_raw[ind_1924, c(2, 4)] <- gsub(replace_this, with_this, df_raw[ind_1924, c(2, 4)])
replace_this <- "(\\d+\\.\\d{1}) (\\d+\\.\\d{1})"; with_this <- "\\2"
df_raw[ind_1924, c(3)] <- gsub(replace_this, with_this, df_raw[ind_1924, c(3)])
df_raw[ind_1924, 5] <- "1.25 1049443"

#1929, 1934, 1936, 1939, 1944, 49, 50, 54, 57, 58, 64, 65








df_raw$V1 <- gsub("(\\d{1}) (\\d{3})", "\\1\\2", df_raw$V1)
df_raw$V1 <- gsub("(\\d+)(.*)", "\\1", df_raw$V1)
ind_1879 <- which(df_raw$V1 == "879")
ind_1886 <- which(df_raw$V1 == "886")
df_raw$V1[ind_1886] <- paste0("1", df_raw$V1[ind_1886])
df_raw$V1[ind_1879] <- paste0("1", df_raw$V1[ind_1879])
ind_rm <- which(duplicated(df_raw$V1)) - 1
df_raw <- df_raw[-ind_rm, ]
replace_this <- '"'; with_this <- ""
df_raw <- as.data.frame(apply(df_raw,
                              2, function(x) gsub(replace_this, with_this, x)))
replace_this <- "S"; with_this <- "5"
df_raw <- as.data.frame(apply(df_raw,
                              2, function(x) gsub(replace_this, with_this, x)))
df_raw <- df_raw %>% mutate_if(is.factor, as.character)

#-----------------------------------------------------------------------------
# Get wheat producer price data for 1966-1974
this_file <- "Agstat-04-23-1982.pdf"
this_filepath <- this_file
outlist <- extract_tables(this_filepath, pages = c(11), method = "stream")
#View(outlist[[1]])
df_raw <- as.data.frame(outlist[[1]])
df_raw <- df_raw %>% mutate_if(is.factor, as.character)
df_raw <- df_raw[c(11:19), ]
df_raw$V1 <- as.numeric(gsub("(\\d{4})(.*)", "\\1", df_raw$V1))
replace_this <- "-|_|, | ,|,|Â·|â???¢ |'"; with_this <- ""
df_raw <- as.data.frame(apply(df_raw,
                              2, function(x) gsub(replace_this, with_this, x)))
df_raw <- df_raw %>% mutate_if(is.factor, as.character)
df_raw <- df_raw[, c(1:4, 7)]
list_str <- strsplit(df_raw$V2, " ")
df_cols23 <- as.data.frame(do.call(rbind, list_str))
df <- data.frame(df_raw$V1, df_cols23, df_raw[, c(3:ncol(df_raw))])
col_names <- c("Year", "Area planted (1000 hectares)",
               "Area harvested (1000 hectares)",
               "Yield (metric tons / hectare)",
               "Production (1000 metric tons)",
               "Producer price (USD / metric ton)")
colnames(df) <- col_names
df <- df %>% mutate_if(is.factor, as.character)
df <- as.data.frame(apply(df, 2, as.numeric))
df$Year <- as.integer(df$Year)
df_66_74 <- df
#----------------------------------------------------------------------------
# Get wheat producer price data for 1975-1988
this_file <- "Agstat-04-23-1990.pdf"
this_filepath <- this_file
outlist <- extract_tables(this_filepath, pages = c(11), method = "stream")
#View(outlist[[2]])
df_raw <- as.data.frame(outlist[[2]])
df_raw <- df_raw %>% mutate_if(is.factor, as.character)
df_raw <- df_raw[c(12:25), ]
df_raw$V1 <- as.numeric(gsub("(\\d{4})(.*)", "\\1", df_raw$V1))
replace_this <- "-|_|, | ,|,|Â·|â???¢ |'"; with_this <- ""
df_raw <- as.data.frame(apply(df_raw,
                              2, function(x) gsub(replace_this, with_this, x)))
df_raw <- df_raw %>% mutate_if(is.factor, as.character)
df_raw <- df_raw[, c(1:4, 7)]
list_str <- strsplit(df_raw$V2, " ")
df_cols23 <- as.data.frame(do.call(rbind, list_str))
df <- data.frame(df_raw$V1, df_cols23, df_raw[, c(3:ncol(df_raw))])
colnames(df) <- col_names
df <- df %>% mutate_if(is.factor, as.character)
df <- as.data.frame(apply(df, 2, as.numeric))
df$Year <- as.integer(df$Year)
df_75_88 <- df
#----------------------------------------------------------------------------
# Get wheat producer price data for 1989-1998
this_file <- "Agstat-05-03-1999.pdf"
this_filepath <- this_file
outlist <- extract_tables(this_filepath, pages = c(10), method = "stream")
#View(outlist[[2]])
df_raw <- as.data.frame(outlist[[2]])
df_raw <- df_raw %>% mutate_if(is.factor, as.character)
df_raw <- df_raw[c(8:17), ]
#df_raw$V1 <- as.numeric(gsub("(\\d{4})(.*)", "\\1", df_raw$V1))
replace_this <- "-|_|, | ,|,|Â·|â???¢ |'"; with_this <- ""
df_raw <- as.data.frame(apply(df_raw,
                              2, function(x) gsub(replace_this, with_this, x)))
df_raw <- df_raw %>% mutate_if(is.factor, as.character)
df_raw <- df_raw[, c(1, 3, 5:8)]
df <- df_raw
colnames(df) <- col_names
df <- df %>% mutate_if(is.factor, as.character)
df <- as.data.frame(apply(df, 2, as.numeric))
df$Year <- as.integer(df$Year)
df_89_98 <- df
#----------------------------------------------------------------------------
# Get wheat producer price data for 1999-2008
this_file <- "Agstat-05-04-2009.pdf"
this_filepath <- this_file
outlist <- extract_tables(this_filepath, pages = c(12), method = "stream")
#View(outlist[[2]])
df_raw <- as.data.frame(outlist[[2]])
df_raw <- df_raw %>% mutate_if(is.factor, as.character)
df_raw <- df_raw[c(7:16), ]
#df_raw$V1 <- as.numeric(gsub("(\\d{4})(.*)", "\\1", df_raw$V1))
replace_this <- "-|_|, | ,|,|Â·|â???¢ |'"; with_this <- ""
df_raw <- as.data.frame(apply(df_raw,
                              2, function(x) gsub(replace_this, with_this, x)))
df_raw <- df_raw %>% mutate_if(is.factor, as.character)
df_raw <- df_raw[, c(1, 3, 4, 6:8)]
df <- df_raw
colnames(df) <- col_names
df <- df %>% mutate_if(is.factor, as.character)
df <- as.data.frame(apply(df, 2, as.numeric))
df$Year <- as.integer(df$Year)
df_99_08 <- df
#----------------------------------------------------------------------------
# Get wheat producer price data for 2009-2018
this_file <- "2019_complete_publication.pdf"
this_filepath <- this_file
outlist <- extract_tables(this_filepath, pages = c(9), method = "stream")
#View(outlist[[2]])
df_raw <- as.data.frame(outlist[[2]])
df_raw <- df_raw %>% mutate_if(is.factor, as.character)
df_raw <- df_raw[c(6:nrow(df_raw)), ]
#df_raw$V1 <- as.numeric(gsub("(\\d{4})(.*)", "\\1", df_raw$V1))
replace_this <- "-|_|, | ,|,|Â·|â???¢ |'"; with_this <- ""
df_raw <- as.data.frame(apply(df_raw,
                              2, function(x) gsub(replace_this, with_this, x)))
df_raw <- df_raw %>% mutate_if(is.factor, as.character)
df_raw <- df_raw[, c(1, 3, 4, 6:8)]
df <- df_raw
colnames(df) <- col_names
df <- df %>% mutate_if(is.factor, as.character)
df <- as.data.frame(apply(df, 2, as.numeric))
df$Year <- as.integer(df$Year)
df_09_18 <- df
#===========================================================================
# Merge 1966-2018
list_df <- list(df_66_74, df_75_88, df_89_98, df_99_08, df_09_18)
df_1966_2018 <- as.data.frame(do.call(rbind, list_df))
#===========================================================================
# Convert units
# Only 1966-2018 needs converting
df_1966_2018$`Area planted (1000 hectares)` <-
  acres_to_has * df_1966_2018$`Area planted (1000 hectares)`
df_1966_2018$`Area harvested (1000 hectares)` <-
  acres_to_has * df_1966_2018$`Area harvested (1000 hectares)`
df_1966_2018$`Yield (metric tons / hectare)` <-
  wheatBushels_to_mTons / acres_to_has * df_1966_2018$`Yield (metric tons / hectare)`
df_1966_2018$`Production (1000 metric tons)` <- 
  wheatBushels_to_mTons * df_1966_2018$`Production (1000 metric tons)`
df_1966_2018$`Producer price (USD / metric ton)` <-
  df_1966_2018$`Producer price (USD / metric ton)` / wheatBushels_to_mTons
#===========================================================================
# Merge full data set
list_df <- list(df_1866_1965, df_1966_2018)
df_1866_2018 <- as.data.frame(do.call(rbind, list_df))
#===========================================================================
# Deflate dollar amounts
# https://liberalarts.oregonstate.edu/spp/polisci/faculty-staff/robert-sahr/inflation-conversion-factors-years-1774-estimated-2024-dollars-recent-years/download-conversion-factors
# © 2018 Robert C. Sahr, Political Science, Oregon State University, e-mail: Robert.Sahr@oregonstate.edu
this_file <- "USD Deflator 1774-present.csv"
this_filepath <- this_file
df_deflator <- read.csv(this_filepath, stringsAsFactors = F)
df_deflator <- df_deflator[-c(1:29), 1:27]
colnames(df_deflator) <- df_deflator[1, ]
df_deflator <- df_deflator[-1, ]
colnames(df_deflator)[1] <- "Year"
df_deflator <- subset(df_deflator, Year %in% df_1866_2018$Year)
df_deflator <- df_deflator[, c("Year", "2017$ CF")]
colnames(df_deflator)[2] <- "2017 USD conversion factor"
df_deflator$`2017 USD conversion factor` <- as.numeric(df_deflator$`2017 USD conversion factor`)

df_1866_2018 <- merge(df_1866_2018, df_deflator, by = "Year")
df_1866_2018$`Producer price (2017 USD / metric ton)` <-
  df_1866_2018$`Producer price (USD / metric ton)` /
  df_1866_2018$`2017 USD conversion factor`
#===========================================================================
# Create per capita variables
this_file <- "US census population 1610-2020.csv"
this_filepath <- this_file
df_pop <- read.csv(this_filepath, stringsAsFactors = F)
df_pop$`Population (1000 persons)` <- df_pop$Population / 1000
df_pop <- df_pop[, c("Year", "Population (1000 persons)")]

df_1866_2018 <- merge(df_1866_2018, df_pop, by = "Year", all.x = T)
# Make per capita vars
df_1866_2018$`Production per capita (metric tons)` <-
  df_1866_2018$`Production (1000 metric tons)` / 
  df_1866_2018$`Population (1000 persons)`
df_1866_2018$`Area planted per capita (hectares)` <-
  df_1866_2018$`Area planted (1000 hectares)` /
  df_1866_2018$`Population (1000 persons)`
#===========================================================================
# Write file
this_file <- "USDA US Wheat Producer Price Area Yield Production per Capita 1866-2018.csv"
this_filepath <- this_file
write.csv(df_1866_2018, this_filepath, row.names = F)