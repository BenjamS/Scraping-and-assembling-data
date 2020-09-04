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
# Wheat price supports 1976-1990 can be retrieved from the 1990 USDA Ag Stat pdf p 17
# Wheat price supports 1991-1999 can be retrieved from the 2000 USDA Ag Stat pdf p 16
# Wheat price supports 2000-2008 can be retrieved from the 2009 USDA Ag Stat pdf p 18
# Wheat price supports 2009-2018 can be retrieved from the 2018 USDA Ag Stat pdf p 17
# Wheat price supports 2019 can be retrieved from the 2019 USDA Ag Stat pdf p 17
#-----------------------------------------------------------------------------
# https://www.nass.usda.gov/Publications/Ag_Statistics/
# The early "Agricultural Statistics" annual reports were first put online
# in 2012 or so.
#-----------------------------------------------------------------------------
# Conversion factors (from the USDA publication)
acres_to_has <- 0.405
wheatBushels_to_mTons <- 60 * 0.454 * 1 / 1000
#-----------------------------------------------------------------------------

this_file <- "Agstat-04-23-1962 pages-21-22,26.pdf"
this_filepath <- this_file
outlist <- extract_tables(this_filepath, pages = c(3), method = "stream")
#View(outlist[[1]])

df_raw <- as.data.frame(outlist[[1]])
df_raw <- df_raw %>% mutate_if(is.factor, as.character)
df_raw <- df_raw[-c(1, 2, 10:nrow(df_raw)), ]
for(i in 1:ncol(df_raw)){
  df_raw[, i] <- gsub(' ,|, |,|-|_|\\.|·', '', df_raw[, i])
  df_raw[, i] <- gsub('  |    ', ' ', df_raw[, i])
  df_raw[, i] <- gsub('  |    ', ' ', df_raw[, i])
  
}
