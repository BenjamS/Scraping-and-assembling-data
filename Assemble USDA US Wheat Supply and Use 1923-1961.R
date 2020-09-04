library(tabulizer)
#library(rvest)
library(tidyverse)
library(pdftools)
this_dir <- "C:/Users/bensc/OneDrive/Documents/Data/"
this_folder <- "USDA Data/"
this_wd <- paste0(this_dir, this_folder)
setwd(this_wd)
#-----------------------------------------------------------------------------
# Get wheat use (demand) data for years 1934-1960.
# In the USDA Ag Stat pdfs, use is called "disappearance".
# Year begins in July.
# Data for 1923-29 is scraped from pg 24 of the 1941 USDA Ag Stats pdf. 
# Data for 1930-43 is scraped from pg 21 of the 1945 USDA Ag Stats pdf.
# Data for 1944-60 is scraped from pg 22 of the 1962 USDA Ag Stats pdf. 
# These pdfs can be downloaded from https://www.nass.usda.gov/Publications/Ag_Statistics/
#-----------------------------------------------------------------------------
# https://www.nass.usda.gov/Publications/Ag_Statistics/
# These early "Agricultural Statistics" annual reports were put online
# in 2012 or so.
#-----------------------------------------------------------------------------
# Conversion factors (from the USDA publication)
acres_to_has <- 0.405
wheatBushels_to_mTons <- 60 * 0.454 * 1 / 1000
#-----------------------------------------------------------------------------
# Get 1923-1929 wheat supply data
this_file <- "Agstat-04-23-1941_1866-1939.pdf"
this_filepath <- this_file
outlist <- extract_tables(this_filepath, pages = c(24), method = "stream")
#View(outlist[[1]])

df_raw <- as.data.frame(outlist[[1]])
df_raw <- df_raw %>% mutate_if(is.factor, as.character)
df_raw <- df_raw[-c(1, 2, 10:nrow(df_raw)), ]
for(i in 1:ncol(df_raw)){
  df_raw[, i] <- gsub(' ,|, |,|-|_|\\.|·', '', df_raw[, i])
  df_raw[, i] <- gsub('  |    ', ' ', df_raw[, i])
  df_raw[, i] <- gsub('  |    ', ' ', df_raw[, i])
  
}

list_str <- strsplit(df_raw[, 1], " ")
df_col12 <- as.data.frame(do.call(rbind, list_str))
list_str <- strsplit(df_raw[, 6], " ")
df_col67 <- as.data.frame(do.call(rbind, list_str))

df_supply_23_29 <- data.frame(df_col12,
                           df_raw[, c(2:5)],
                           df_col67,
                           df_raw[, 8])

col_names_supply <- c("Year",
                      "Stocks on farms (1000 metric tons)",
                      "Country elevators and mills (1000 metric tons)",
                      "Commercial stocks",
                      "Merchant elevators, mills, etc. (1000 metric tons)",
                      "Total stocks (1000 metric tons)",
                      "New crop (1000 metric tons)",
                      "Imports inc. flour (1000 metric tons)",
                      "Total supply (1000 metric tons)")

colnames(df_supply_23_29) <- col_names_supply
df_supply_23_29 <- df_supply_23_29 %>% mutate_if(is.factor, as.character)
df_supply_23_29[, -1] <- as.data.frame(apply(df_supply_23_29[, -1], 2, as.numeric))
df_supply_23_29$Year <- as.integer(df_supply_23_29$Year)
# Convert units
df_supply_23_29[, -1] <- as.data.frame(apply(df_supply_23_29[, -1], 2, function(x){x * wheatBushels_to_mTons}))
#----------------------------------------------------------------------------
# Get 1923-1929 wheat use data
df_raw <- as.data.frame(outlist[[2]])
df_raw <- df_raw %>% mutate_if(is.factor, as.character)
df_raw <- df_raw[-c(1:3, 11:nrow(df_raw)), ]
for(i in 1:ncol(df_raw)){
  df_raw[, i] <- gsub(' ,|, |,|-|_|·', '', df_raw[, i])
  df_raw[, i] <- gsub('  |    ', ' ', df_raw[, i])
  df_raw[, i] <- gsub('  |    ', ' ', df_raw[, i])
  df_raw[, i] <- gsub('\\. ', '\\.', df_raw[, i])
  
}
df_raw$V1 <- gsub("\\.", "", df_raw$V1)


list_str <- strsplit(df_raw[, 6], " ")
df_col67 <- as.data.frame(do.call(rbind, list_str))
list_str <- strsplit(df_raw[, 9], " ")
df_col910 <- as.data.frame(do.call(rbind, list_str))

df_use_23_29 <- data.frame(df_raw[, c(1, 5)],
                           df_col67,
                              df_raw[, 8],
                              df_col910,
                              df_raw[, 11:14])
col_names_use <- c("Year",
               "Wheat exports (1000 metric tons)",
               "Wheat flour exports (1000 metric tons)",
               "Shipments inc. flour (1000 metric tons)",
               "Total exports (1000 metric tons)",
               "Seed (1000 metric tons)",
               "Feed on farm (1000 metric tons)",
               "Food and commercial feed (1000 metric tons)",
               "Total domestic use (1000 metric tons)",
               "Stocks June 30 (1000 metric tons)",
               "Food use per capita (metric tons)")

colnames(df_use_23_29) <- col_names_use
df_use_23_29 <- df_use_23_29 %>% mutate_if(is.factor, as.character)
df_use_23_29[, -1] <- as.data.frame(apply(df_use_23_29[, -1], 2, as.numeric))
df_use_23_29$Year <- as.integer(df_use_23_29$Year)
# Convert units
df_use_23_29[, -1] <- as.data.frame(apply(df_use_23_29[, -1], 2, function(x){x * wheatBushels_to_mTons}))
#===========================================================================
# Now get wheat supply data for 1930-1938
this_file <- "Agstat-04-23-1945.pdf"
this_filepath <- this_file
outlist <- extract_tables(this_filepath, pages = c(21), method = "stream")
#View(outlist[[1]])

df_raw <- as.data.frame(outlist[[1]])
df_raw <- df_raw %>% mutate_if(is.factor, as.character)
df_raw <- df_raw[-c(1, nrow(df_raw)), ]
for(i in 1:ncol(df_raw)){
  df_raw[, i] <- gsub(' ,|, |,|-|_|·|\\.', '', df_raw[, i])
  df_raw[, i] <- gsub(' ', '', df_raw[, i])
}
df_raw[1, ] <- gsub("bushels| ", "", df_raw[1, ])
df_supply_30_38 <- df_raw[, -6]

colnames(df_supply_30_38) <- col_names_supply
df_supply_30_38 <- df_supply_30_38 %>% mutate_if(is.factor, as.character)
df_supply_30_38[, -1] <- as.data.frame(apply(df_supply_30_38[, -1], 2, as.numeric))
df_supply_30_38$Year <- as.integer(df_supply_30_38$Year)
df_supply_30_38 <- subset(df_supply_30_38, Year < 1939)
# Convert units
df_supply_30_38[, -1] <- as.data.frame(apply(df_supply_30_38[, -1], 2, function(x){x * wheatBushels_to_mTons}))
#----------------------------------------------------------------------------
# ...and wheat use data for 1930-1938
df_raw <- as.data.frame(outlist[[2]])
df_raw <- df_raw %>% mutate_if(is.factor, as.character)
df_raw <- df_raw[-c(1, 2, nrow(df_raw)), ]
for(i in 1:ncol(df_raw)){
  df_raw[, i] <- gsub(' ,|, |,|-|_|·|', '', df_raw[, i])
}
df_raw$V1 <- gsub(" 8 |\\.", "", df_raw$V1)
df_raw$V4 <- gsub("\\.", "", df_raw$V4)
df_raw$V2 <- gsub('(8) (\\d+)(.*)', '\\2', df_raw$V2)
df_raw$V12 <- gsub('(8) (\\d+\\.\\d+)', '\\2', df_raw$V12)
this_num <- gsub("(\\d{4})  (\\d+)", "\\2", df_raw$V1[nrow(df_raw)])
df_raw$V2[nrow(df_raw)] <- this_num
df_raw$V1[nrow(df_raw)] <- gsub("(\\d{4})  (\\d+)", "\\1", df_raw$V1[nrow(df_raw)])
this_num <- gsub("(\\d+) (\\d+) (\\d+)", "\\3", df_raw$V4[nrow(df_raw)])
df_raw$V5[nrow(df_raw)] <- this_num
df_raw$V4[nrow(df_raw)] <- gsub("(\\d+) (\\d+) (\\d+)", "\\1 \\2", df_raw$V4[nrow(df_raw)])
df_raw$V6 <- df_raw$V5
df_raw$V5 <- gsub("(\\d+) (\\d+)", "\\2", df_raw$V4)
df_raw$V5[1:3] <- NA
df_raw$V4 <- gsub("(\\d+) (\\d+)", "\\1", df_raw$V4)
df_use_30_38 <- df_raw

col_names_use2 <- c("Year",
                    "Wheat food (1000 metric tons)",
                    "Wheat feed (1000 metric tons)",
                    "Wheat seed (1000 metric tons)",
                    "Wheat industrial (1000 metric tons)",
                    "Total domestic use (1000 metric tons)",
                    "Wheat exports (1000 metric tons)",
                    "Wheat flour exports (1000 metric tons)",
                    "Shipments inc. flour (1000 metric tons)",
                    "Total exports (1000 metric tons)",
                    "Stocks June 30 (1000 metric tons)",
                    "Food use per capita (metric tons)")

colnames(df_use_30_38) <- col_names_use2
df_use_30_38 <- df_use_30_38 %>% mutate_if(is.factor, as.character)
df_use_30_38[, -1] <- as.data.frame(apply(df_use_30_38[, -1], 2, as.numeric))
df_use_30_38$Year <- as.integer(df_use_30_38$Year)
df_use_30_38 <- subset(df_use_30_38, Year < 1939)
# Convert units
df_use_30_38[, -1] <- as.data.frame(apply(df_use_30_38[, -1], 2, function(x){x * wheatBushels_to_mTons}))
#===========================================================================
# Now get wheat supply data for 1939-1961
# Note: The NA that appear in the last few rows is because those data were included
# under the "interior mill, elevator, and warehouse stocks" column.
this_file <- "Agstat-04-23-1962 pages-21-22,26.pdf"
this_filepath <- this_file
out_txt <- pdf_text(this_filepath)
#write(out, "test.txt")
#cat(out)
text <- read_lines(out_txt)
#text[grep("1939", text)]
ind_39 <- grep("1939", text)[4]
#text[ind:(ind_39+25)]
v <- gsub("-|_|,", "", text[ind_39])
v <- gsub("(\\d{1}) (\\d{1})", "\\1\\2", v)
row_39 <- strsplit(v, " ")[[1]]
row_39 <- row_39[which(row_39 != "")]
ind_40 <- ind_39 + 2
v <- gsub("-|_|,", "", text[ind_40])
v <- gsub("(\\d{1}) (\\d{1})", "\\1\\2", v)
row_40 <- strsplit(v, " ")[[1]]
row_40 <- row_40[which(row_40 != "")]
row_40[1] <- "1940"
ind_4142 <- (ind_39 + 3):(ind_39 + 20)
v <- gsub(", | ,|,|-|_| ", "", text[ind_4142])
row_41 <- c("1941", v[seq(1, length(v), 2)])
row_42 <- c("1942", v[seq(2, length(v), 2)])
row_41 <- row_41[row_41 != ""]
df_3942 <- as.data.frame(rbind(row_39, row_40, row_41))
df_3942 <- data.frame(df_3942[, 1:5], V6 = NA, df_3942[, 6:ncol(df_3942)])
df_3942 <- df_3942 %>% mutate_if(is.factor, as.character)
df_3942 <- as.data.frame(rbind(df_3942, row_42))
colnames(df_3942) <- paste0("V", c(1:ncol(df_3942)))
#text[grep("1943", text)[2]]
ind_43 <- grep("1943", text)[2]
rows_4356 <- text[ind_43:(ind_43 + 13)]
list_rows <- list()
for(i in 1:length(rows_4356)){
  this_row <- rows_4356[i]
  this_row <- gsub(", | ,|,|-|_|\\.|.|'", "", this_row)
  this_row <- gsub("I|l|!", "1", this_row)
  this_row <- strsplit(this_row, " ")[[1]]
  this_row <- this_row[which(this_row != "")]
  #print(this_row)
  list_rows[[i]] <- this_row
}
df_4356 <- as.data.frame(do.call(rbind, list_rows))
df_4356 <- df_4356 %>% mutate_if(is.factor, as.character)
colnames(df_4356) <- paste0("V", c(1:ncol(df_4356)))
#--
ind_5760 <- c(ind_43 + 15, ind_43 + 16, ind_43 + 18, ind_43 + 20)
rows_5760 <- text[ind_5760]
list_rows <- list()
for(i in 1:length(rows_5760)){
  this_row <- rows_5760[i]
  this_row <- gsub(", | ,|,|-|_|\\.|.|'", "", this_row)
  this_row <- gsub("(\\d+) (\\d+)", "\\1\\2", this_row)
  this_row <- gsub("I|l|!", "1", this_row)
  this_row <- strsplit(this_row, " ")[[1]]
  this_row <- this_row[which(this_row != "")]
  print(this_row)
  list_rows[[i]] <- this_row
}
list_rows[[1]][1] <- "1957"
list_rows[[2]] <- c("1958", list_rows[[2]])
list_rows[[3]][1] <- "1959"
list_rows[[4]] <- c("1960", list_rows[[4]])
df_5760 <- as.data.frame(do.call(rbind, list_rows))
df_5760 <- df_5760 %>% mutate_if(is.factor, as.character)
df_5760 <- data.frame(df_5760[, 1:4], V5 = NA, df_5760[, 5:ncol(df_5760)])
colnames(df_5760) <- paste0("V", c(1:ncol(df_5760)))
df_3960 <- as.data.frame(rbind(df_3942, df_4356, df_5760))

col_names_supply3 <- c("Year",
                       "Stocks on farms (1000 metric tons)",
                       "Interior mill elevator, warehouse (1000 metric tons)",
                       "Terminal market (1000 metric tons)",
                       "Merchant mill (1000 metric tons)",
                       "CCC (1000 metric tons)",
                       "Total stocks (1000 metric tons)",
                       "New crop (1000 metric tons)",
                       "Imports inc. flour (1000 metric tons)",
                       "Total supply (1000 metric tons)")

colnames(df_3960) <- col_names_supply3
df_supply_39_60 <- df_3960 %>% mutate_if(is.factor, as.character)
df_supply_39_60[, -1] <- as.data.frame(apply(df_supply_39_60[, -1], 2, as.numeric))
# Add 1961 (taken from the 1964 USDA Ag Stat pdf table 11 p 18)
df_supply_61 <- data.frame(Year = 1961,
                           farm_stocks = 136937,
                           int_mill = 1203682,
                           NA,
                           NA,
                           CCC = 70599,
                           tot_stocks = 1411178,
                           new_crop = 1234743,
                           imports = 5885,
                           tot_supply = 2651806)
colnames(df_supply_61) <- col_names_supply3
df_supply_39_61 <- as.data.frame(rbind(df_supply_39_60,
                                       df_supply_61))
df_supply_39_61$Year <- as.integer(df_supply_39_61$Year)
rm(df_supply_39_60)
# Convert units
df_supply_39_61[, -1] <- as.data.frame(apply(df_supply_39_61[, -1], 2, function(x){x * wheatBushels_to_mTons}))
#---------------------------------------------------------------------------
#...and wheat use data for 1939-1961
# Note: Total supply - Total use = year end carry over (a.k.a. ending stocks)
#text[grep("1939", text)]
ind_39 <- grep("1939", text)[6]
#text[ind_39:(ind_39+25)]
these_rows <- text[ind_39:(ind_39 + 21)]
process_row <- function(this_row, quietly = T){
  this_row <- gsub(", | ,|,|-|_|\\.|.|'|; | ;", "", this_row)
  #this_row <- gsub("(\\d+) (\\d+)", "\\1\\2", this_row)
  this_row <- gsub("I|l|!", "1", this_row)
  this_row <- strsplit(this_row, " ")[[1]]
  this_row <- this_row[which(this_row != "")]
  if(!quietly){print(this_row)}
  return(this_row)
}
list_rows <- purrr::map(these_rows, process_row)

df_3940 <- as.data.frame(do.call(rbind, list_rows[1:2]))
df_3940 <- data.frame(df_3940[, 1:6], NA, df_3940[, 7:ncol(df_3940)])
df_4160 <- as.data.frame(do.call(rbind, list_rows[3:length(list_rows)]))
colnames(df_3940) <- paste0("V", c(1:ncol(df_3940)))
df_3960 <- as.data.frame(rbind(df_3940, df_4160))
df_3960 <- df_3960 %>% mutate_if(is.factor, as.character)
df_3960 <- as.data.frame(apply(df_3960, 2, as.numeric))
# Get 1961 data (again taken from USDA Ag Stat 1964 pdf)
vec_61 <- c(1961,
                    490474,
                    55968,
                    64,
                    54393,
                    600899,
                    6855,
                    608079,
                    110903,
                    880,
                    719862,
                    2320,
                    1329936)

df_3961 <- as.data.frame(rbind(df_3960, vec_61))

# col_names_use
# col_names_use2
col_names_use3 <- c("Year",
                    "Wheat food (1000 metric tons)",
                    "Wheat seed (1000 metric tons)",
                    "Wheat industrial (1000 metric tons)",
                    "Wheat feed (1000 metric tons)",
                    "Total domestic use (1000 metric tons)",
                    "Military procurement (1000 metric tons)",
                    "Wheat exports (1000 metric tons)",
                    "Wheat flour exports (1000 metric tons)",
                    "Wheat other exports (1000 metric tons)",
                    "Total exports (1000 metric tons)",
                    "Shipments inc. flour (1000 metric tons)",
                    "Total use (1000 metric tons)")
    
colnames(df_3961) <- col_names_use3
df_3961$Year <- as.integer(df_3961$Year)
# Convert units
df_3961[, -1] <- as.data.frame(apply(df_3961[, -1], 2, function(x){x * wheatBushels_to_mTons}))
df_use_39_61 <- df_3961
rm(df_3961)
#============================================================================
# Assemble wheat use and supply 1923-1961
list_supply <- list(df_supply_23_29[, c("Year", "Total supply (1000 metric tons)")],
                df_supply_30_38[, c("Year", "Total supply (1000 metric tons)")],
                df_supply_39_61[, c("Year", "Total supply (1000 metric tons)")])
df_wheatSupply_1923_1961 <- as.data.frame(do.call(rbind, list_supply))

df_use_23_29$`Total use (1000 metric tons)` <-
  df_use_23_29$`Total exports (1000 metric tons)` +
  df_use_23_29$`Total domestic use (1000 metric tons)`

df_use_30_38$`Total use (1000 metric tons)` <- 
  df_use_30_38$`Total domestic use (1000 metric tons)` +
  df_use_30_38$`Total exports (1000 metric tons)`

list_use <- list(df_use_23_29[, c("Year", "Total use (1000 metric tons)")],
                 df_use_30_38[, c("Year", "Total use (1000 metric tons)")],
                 df_use_39_61[, c("Year", "Total use (1000 metric tons)")])

df_wheatUse_1923_1961 <- as.data.frame(do.call(rbind, list_use))
df_wheatSupplyUse_1923_1961 <- merge(df_wheatSupply_1923_1961,
                                     df_wheatUse_1923_1961, by = "Year")
#write.csv(df_wheatSupplyUse_1923_1961, "USDA US Wheat Supply and Use 1923-1961.csv")
#==========================================================================
df_plot <- df_wheatSupplyUse_1923_1961
df_plot$`Supply to demand ratio` <- df_plot$`Total supply (1000 metric tons)` /
  df_plot$`Total use (1000 metric tons)`
gg <- ggplot(df_plot, aes(x = Year, y = `Supply to demand ratio`))
gg <- gg + geom_line(lwd = 1.3)
gg

df_plot <- df_plot %>% gather(Variable, `Thousand metric tons`, `Total supply (1000 metric tons)`:`Total use (1000 metric tons)`)
gg <- ggplot(df_plot, aes(x = Year, y = `Thousand metric tons`,
                          group = Variable,
                          color = Variable))
gg <- gg + geom_line(lwd = 1.3)
gg
