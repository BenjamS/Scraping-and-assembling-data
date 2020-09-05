library(tabulizer)
#library(rvest)
library(tidyverse)
library(pdftools)
this_dir <- "C:/Users/bensc/OneDrive/Documents/Data/"
this_folder <- "USDA Data/"
this_wd <- paste0(this_dir, this_folder)
setwd(this_wd)
#-----------------------------------------------------------------------------
# Get wheat price and price support data
# Wheat price supports 1949-1960 can be retrieved from the 1962 USDA Ag Stat pdf p 26
# Wheat price supports 1956-1964 can be retrieved from the 1966 USDA Ag Stat pdf p 23
# Wheat price supports 1965-1981 can be retrieved from the 1982 USDA Ag Stat pdf p 17
# Wheat price supports 1976-1989 can be retrieved from the 1990 USDA Ag Stat pdf p 17
# Wheat price supports 1990-1998 can be retrieved from the 2000 USDA Ag Stat pdf p 16
# Wheat price supports 1999-2007 can be retrieved from the 2009 USDA Ag Stat pdf p 18
# Wheat price supports 2008-2017 can be retrieved from the 2018 USDA Ag Stat pdf p 17
# Wheat price supports 2019 can be retrieved from the 2019 USDA Ag Stat pdf p 17
#-----------------------------------------------------------------------------
# "Year" is the marketing year. Begins on July 1 for years 1949-1975,
# on June 1 for 1976 onward. (See footnote to Table 9 of the 1990 Ag Stat pdf.)
# % of parity is measured at beginning of year.
# https://www.nass.usda.gov/Publications/Ag_Statistics/
# The early "Agricultural Statistics" annual reports were first put online
# in 2012 or so.
#-----------------------------------------------------------------------------
# Conversion factors (from the USDA publication)
acres_to_has <- 0.405
wheatBushels_to_mTons <- 60 * 0.454 * 1 / 1000
#=============================================================================
# Get wheat price support data for 1949-1955
this_file <- "Agstat-04-23-1962 pages-21-22,26.pdf"
this_filepath <- this_file
outlist <- extract_tables(this_filepath, pages = c(3), method = "stream")
#View(outlist[[1]])
df_raw <- as.data.frame(outlist[[2]])
df_raw <- df_raw %>% mutate_if(is.factor, as.character)
df_raw <- df_raw[-c(1, 2, 15:nrow(df_raw)), ]
replace_this <- "-|_|, | ,|,"; with_this <- ""
df_raw <- as.data.frame(apply(df_raw,
                    2, function(x) gsub(replace_this, with_this, x)))
replace_this <- "\\. | \\."; with_this <- "\\."
df_raw <- as.data.frame(apply(df_raw,
                              2, function(x) gsub(replace_this, with_this, x)))
df_raw$V1 <- gsub("\\.", "", df_raw$V1)
df_raw$V6 <- gsub("\\.", "", df_raw$V6)
df_raw <- df_raw %>% mutate_if(is.factor, as.character)

list_str <- strsplit(df_raw$V6, " ")
df_cols67 <- as.data.frame(do.call(rbind, list_str))

list_str <- strsplit(df_raw$V4, " ")
df_cols45 <- as.data.frame(do.call(rbind, list_str))

df_49_60 <- data.frame(df_raw[, 1:3], df_cols45, df_raw[, 5], df_cols67)
#--------------
col_names_62 <- c("Year", "Support price (2017 USD / metric ton)", "Parity (%)",
               "Season average producer price (2017 USD/ metric ton)",
               "Qty. put under support (million metric tons)",
               "Qty. put under support (% of production)",
               "Acquisitions under support program (million metric tons)",
               "Owned by CCC June 30 (million metric tons)")
#--------------
colnames(df_49_60) <- col_names_62
df_49_60 <- as.data.frame(apply(df_49_60, 2, as.numeric))
df_49_60$Year <- as.integer(df_49_60$Year)
df_49_55 <- subset(df_49_60, Year <= 1955)
rm(df_49_60, df_cols45, df_cols67)
#----------------------------------------------------------------------------
# Get wheat price support data for 1956-1964
this_file <- "Agstat-04-23-1966.pdf"
this_filepath <- this_file
outlist <- extract_tables(this_filepath, pages = c(23), method = "stream")
#View(outlist[[1]])
df_raw <- as.data.frame(outlist[[1]])
df_raw <- df_raw %>% mutate_if(is.factor, as.character)
df_raw <- df_raw[c(9:17), ]
replace_this <- "-|_|, | ,|,|â???¢"; with_this <- ""
df_raw <- as.data.frame(apply(df_raw,
                              2, function(x) gsub(replace_this, with_this, x)))
replace_this <- "\\. | \\."; with_this <- "\\."
df_raw <- as.data.frame(apply(df_raw,
                              2, function(x) gsub(replace_this, with_this, x)))
df_raw$V1 <- gsub("L", "1", df_raw$V1)
df_raw$V2 <- gsub("t", "1", df_raw$V2)
df_raw$V2 <- gsub("4 ", "", df_raw$V2)
df_raw$V3 <- trimws(df_raw$V3)
df_raw <- df_raw %>% mutate_if(is.factor, as.character)
list_str <- strsplit(df_raw$V2, " ")
df_cols23 <- as.data.frame(do.call(rbind, list_str))
list_str <- strsplit(df_raw$V3, " ")
df_cols456 <- as.data.frame(do.call(rbind, list_str))
df_56_64 <- data.frame(df_raw$V1, df_cols23, df_cols456, df_raw[, 4:5])
colnames(df_56_64) <- col_names_62
df_56_64 <- as.data.frame(apply(df_56_64, 2, as.numeric))
df_56_64$Year <- as.integer(df_56_64$Year)
rm(df_cols456, df_cols23)
#----------------------------------------------------------------------------
# Get wheat price support data for 1965-1975
this_file <- "Agstat-04-23-1982.pdf"
this_filepath <- this_file
outlist <- extract_tables(this_filepath, pages = c(17), method = "stream")
#View(outlist[[1]])
df_raw <- as.data.frame(outlist[[1]])
df_raw <- df_raw %>% mutate_if(is.factor, as.character)
df_raw <- df_raw[c(7:17), ]
replace_this <- "-|_|, | ,|,|Â·|'"; with_this <- ""
df_raw <- as.data.frame(apply(df_raw,
                              2, function(x) gsub(replace_this, with_this, x)))
replace_this <- "\\. | \\."; with_this <- "\\."
df_raw <- as.data.frame(apply(df_raw,
                              2, function(x) gsub(replace_this, with_this, x)))
df_raw$V1 <- gsub("\\.", "", df_raw$V1)
df_raw <- df_raw %>% mutate_if(is.factor, as.character)
list_str <- strsplit(df_raw$V2, " ")
df_cols2345 <- as.data.frame(do.call(rbind, list_str))
df_65_75 <- data.frame(df_raw$V1, df_cols2345, df_raw[, 3:4])
col_names_82 <- setdiff(col_names_62, "Season average producer price (2017 USD/ metric ton)")
colnames(df_65_75) <- col_names_82
df_65_75 <- as.data.frame(apply(df_65_75, 2, as.numeric))
df_65_75$Year <- as.integer(df_65_75$Year)
rm(df_cols2345)
#----------------------------------------------------------------------------
# Get wheat price support data for 1976-1989
this_file <- "Agstat-04-23-1990.pdf"
this_filepath <- this_file
outlist <- extract_tables(this_filepath, pages = c(17), method = "stream")
#View(outlist[[2]])
df_raw <- as.data.frame(outlist[[2]])
df_raw <- df_raw %>% mutate_if(is.factor, as.character)
df_raw <- df_raw[c(9:22), ]
df_raw$V1 <- as.numeric(gsub("(\\d{4})(.*)", "\\1", df_raw$V1))
replace_this <- "-|_|, | ,|,|Â·|â???¢ |'"; with_this <- ""
df_raw <- as.data.frame(apply(df_raw,
                              2, function(x) gsub(replace_this, with_this, x)))
df_raw$V4 <- gsub("(\\d{1}) (\\d+)", "\\2", df_raw$V4)
df_raw$V3 <- gsub("\\.\\.| \\.", "", df_raw$V3)
df_raw$V3 <- gsub(" \\d{1} ", " ", df_raw$V3)
df_raw$V2 <- gsub("^\\d{1} ", "", df_raw$V2)
df_raw$V4 <- gsub("8192", "192", df_raw$V4)
df_raw <- df_raw %>% mutate_if(is.factor, as.character)
list_str <- strsplit(df_raw$V2, " ")
df_cols2345 <- as.data.frame(do.call(rbind, list_str))
list_str <- strsplit(df_raw$V3, " ")
df_cols678 <- as.data.frame(do.call(rbind, list_str))
df_cols678 <- df_cols678 %>% mutate_if(is.factor, as.character)
ind <- which(df_cols678$V3 == df_cols678$V1)
df_cols678$V3[ind] <- NA
df_76_89 <- data.frame(df_raw$V1, df_cols2345, df_cols678, df_raw[, ncol(df_raw)])
df_76_89 <- df_76_89 %>% mutate_if(is.factor, as.character)
col_names_90 <- c("Year", "Support price loan (2017 USD / metric ton)",
                  "Support price target (2017 USD / metric ton)",
                  "Parity loan (%)", "Parity target (%)",
                  "Qty. put under support (million metric tons)",
                  "Qty. put under support (% of production)",
                  "Acquisitions under support program (million metric tons)",
                  "Owned by CCC June 30 (million metric tons)")
colnames(df_76_89) <- col_names_90
df_76_89 <- as.data.frame(apply(df_76_89, 2, as.numeric))
df_76_89$Year <- as.integer(df_76_89$Year)
rm(df_cols2345, df_cols678)
#----------------------------------------------------------------------------
# Get wheat price support data for 1990-1998
this_file <- "Agstat-05-02-2000.pdf"
this_filepath <- this_file
outlist <- extract_tables(this_filepath, pages = c(16), method = "stream")
#View(outlist[[1]])
df_raw <- as.data.frame(outlist[[1]])
df_raw <- df_raw %>% mutate_if(is.factor, as.character)
df_raw <- df_raw[c(7:15), ]
df_raw$V1 <- as.numeric(gsub("(\\d{4})(.*)", "\\1", df_raw$V1))
df_raw <- df_raw %>% mutate_if(is.factor, as.character)
list_str <- strsplit(df_raw$V3, " ")
df_cols34 <- as.data.frame(do.call(rbind, list_str))
list_str <- strsplit(df_raw$V4, " ")
df_cols56 <- as.data.frame(do.call(rbind, list_str))
df_90_98 <- data.frame(df_raw[, 1:2], df_cols34, df_cols56, df_raw[, 5:6])
# Note: The actual col names 3 and 4 do not refer to "support price" but rather
# "program price level"
col_names_00 <- c("Year",
                  "Income support payment rates (2017 USD / metric ton)",
                  "Support price loan (2017 USD / metric ton)",
                  "Support price target (2017 USD / metric ton)",
                  "Qty. put under support (million metric tons)",
                  "Qty. put under support (% of production)",
                  "Acquisitions under support program (million metric tons)",
                  "Owned by CCC June 30 (million metric tons)")
colnames(df_90_98) <- col_names_00
df_90_98 <- as.data.frame(apply(df_90_98, 2, as.numeric))
df_90_98$Year <- as.integer(df_90_98$Year)
rm(df_cols34, df_cols56)
#----------------------------------------------------------------------------
# Get wheat price support data for 1999-2007
this_file <- "Agstat-05-04-2009.pdf"
this_filepath <- this_file
outlist <- extract_tables(this_filepath, pages = c(18), method = "stream")
#View(outlist[[1]])
df_raw <- as.data.frame(outlist[[1]])
df_raw <- df_raw %>% mutate_if(is.factor, as.character)
df_raw <- df_raw[c(6:14), ]
df_raw$V1 <- as.numeric(gsub("(\\d{4})(.*)", "\\1", df_raw$V1))
df_raw$V3 <- gsub("(\\d+)/(.*)", "\\1", df_raw$V3)
list_str <- strsplit(df_raw$V7, " ")
df_cols56 <- as.data.frame(do.call(rbind, list_str))
df_99_07 <- data.frame(df_raw[, c(1, 3, 5:6)], df_cols56, df_raw[, c(8, 10)])
# Note: The actual col names 3 and 4 do not refer to "support price" but rather
# "program price level"
colnames(df_99_07) <- col_names_00
df_99_07 <- as.data.frame(apply(df_99_07, 2, as.numeric))
df_99_07$Year <- as.integer(df_99_07$Year)
rm(df_cols56)
#----------------------------------------------------------------------------
# Get wheat price support data for 2008-2018
this_file <- "Ag_Stats_2018.pdf"
this_filepath <- this_file
outlist <- extract_tables(this_filepath, pages = c(17), method = "stream")
#View(outlist[[1]])
df_raw <- as.data.frame(outlist[[1]])
df_raw <- df_raw %>% mutate_if(is.factor, as.character)
df_raw <- df_raw[c(7:nrow(df_raw)), ]
df_raw$V1 <- as.numeric(gsub("(\\d{4})(.*)", "\\1", df_raw$V1))
df_raw$V2 <- gsub("(\\d+)/(.*)", "\\1", df_raw$V2)
list_str <- strsplit(df_raw$V3, " ")
df_cols34 <- as.data.frame(do.call(rbind, list_str))
list_str <- strsplit(df_raw$V4, " ")
df_cols56 <- as.data.frame(do.call(rbind, list_str))
df_08_17 <- data.frame(df_raw[, c(1, 2)], df_cols34, df_cols56, df_raw[, c(5, 6)])
# Note: The actual col names 3 and 4 do not refer to "support price" but rather
# "program price level"
colnames(df_08_17) <- col_names_00
df_08_17 <- as.data.frame(apply(df_08_17, 2, as.numeric))
df_08_17$Year <- as.integer(df_08_17$Year)
rm(df_cols34, df_cols56)
#----------------------------------------------------------------------------
# Get wheat price support data for 2018
this_file <- "2019_complete_publication.pdf"
this_filepath <- this_file
outlist <- extract_tables(this_filepath, pages = c(17), method = "stream")
#View(outlist[[1]])
df_raw <- as.data.frame(outlist[[1]])
df_raw <- df_raw %>% mutate_if(is.factor, as.character)
df_raw <- df_raw[13, ]
df_raw$V1 <- as.numeric(gsub("(\\d{4})(.*)", "\\1", df_raw$V1))
df_raw$V2 <- gsub("(\\d+)/(.*)", "\\1", df_raw$V2)
list_str <- strsplit(df_raw$V3, " ")
df_cols34 <- as.data.frame(do.call(rbind, list_str))
list_str <- strsplit(df_raw$V4, " ")
df_cols56 <- as.data.frame(do.call(rbind, list_str))
df_18 <- data.frame(df_raw[, 1:2], df_cols34, df_cols56, df_raw[, c(6, 7)])
colnames(df_18) <- col_names_00
df_18 <- df_18 %>% mutate_if(is.factor, as.character)
#df_18 <- as.data.frame(apply(df_18, 2, as.numeric))
df_18[1, ] <- as.numeric(df_18[1, ])
df_18$Year <- as.integer(df_18$Year)
rm(df_cols34, df_cols56)
#============================================================================
# Unify into single wheat price support data set covering 1949-2018
# (For 1976 onwards, what is the support price? The "loan" or "target" price?)
# Probably the loan rate, which is said to have acted as a price floor after
# the Agricultural and Consumer Protection Act of 1973 onwards -- see p 13 etc.
# of "Langley et al USDA ERS cica 1984 Commodity Program Perspectives.pdf"
# The target price is said to have acted as income support.
# (After 1989, what is "Income support payment rate"?)
df_49_64 <- as.data.frame(rbind(df_49_55, df_56_64))
df_65_75[, c("Year", "Support price (2017 USD / metric ton)")]
df_76_18 <- as.data.frame(rbind(df_76_89[, c("Year", "Support price loan (2017 USD / metric ton)")],
                                df_90_98[, c("Year", "Support price loan (2017 USD / metric ton)")],
                                df_99_07[, c("Year", "Support price loan (2017 USD / metric ton)")],
                                df_08_17[, c("Year", "Support price loan (2017 USD / metric ton)")],
                                df_18[, c("Year", "Support price loan (2017 USD / metric ton)")]))

colnames(df_76_18)[2] <- "Support price (2017 USD / metric ton)"
df_65_18 <- as.data.frame(rbind(df_65_75, df_76_18))
df_65_18$`Season average producer price (2017 USD / metric ton)` <- NA

keep_cols <- c("Year", "Support price (2017 USD / metric ton)",
               "Season average producer price (2017 USD / metric ton)",
               "Qty. put under support (million metric tons)",
               "Qty. put under support (% of production)")
df_49_18 <- as.data.frame(rbind(df_49_64[, c()],
                                df_65_75[, c("Year", "Support price (2017 USD / metric ton)")],
                                df_76_18))
#---------------------------------------------------------------------------
# Deflate USD values and convert units
# Yearly deflators from here:
# https://liberalarts.oregonstate.edu/spp/polisci/faculty-staff/robert-sahr/inflation-conversion-factors-years-1774-estimated-2024-dollars-recent-years/download-conversion-factors
# © 2018 Robert C. Sahr, Political Science, Oregon State University, e-mail: Robert.Sahr@oregonstate.edu
this_file <- "USD Deflator 1774-present.csv"
this_filepath <- this_file
df_deflator <- read.csv(this_filepath, stringsAsFactors = F)
df_deflator <- df_deflator[-c(1:29), 1:27]
colnames(df_deflator) <- df_deflator[1, ]
df_deflator <- df_deflator[-1, ]
colnames(df_deflator)[1] <- "Year"
df_deflator <- subset(df_deflator, Year %in% df_49_18$Year)
df_deflator <- df_deflator[, c("Year", "2017$ CF")]
colnames(df_deflator)[2] <- "2017 USD conversion factor"
df_deflator$`2017 USD conversion factor` <- as.numeric(df_deflator$`2017 USD conversion factor`)
#---
df_49_18 <- merge(df_49_18, df_deflator, by = "Year")
df_49_64 <- merge(df_49_64, df_deflator, by = "Year")