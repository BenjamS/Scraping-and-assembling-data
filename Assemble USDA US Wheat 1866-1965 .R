library(tabulizer)
#library(rvest)
library(tidyverse)
#-----------------------------------------------------------------------------
# https://www.nass.usda.gov/Publications/Ag_Statistics/
# These early "Agricultural Statistics" annual reports were put online
# in 2012 or so.
#-----------------------------------------------------------------------------
# Conversion factors (from the USDA publication)
acres_to_has <- 0.405
wheatBushels_to_mTons <- 60 * 0.454 * 1 / 1000
#-----------------------------------------------------------------------------
# Get 1899-1940 wheat data
this_dir <- "C:/Users/bensc/OneDrive/Documents/Data/"
this_folder <- "USDA Data/"
this_file <- "Agstat-04-23-1942_1899-1940.pdf"
this_filepath <- paste0(this_dir, this_folder, this_file)
outlist <- extract_tables(this_filepath, pages = c(11), method = "stream")
#View(outlist[[1]])

df_raw <- as.data.frame(outlist[[1]])
df_raw <- df_raw %>% mutate_if(is.factor, as.character)
df_raw <- df_raw[-c(1:3, nrow(df_raw)), ]
colnames(df_raw)[1] <- "Year"
ind_dup <- which(duplicated(df_raw$Year)) - 1
ind_rm <- c(ind_dup,
            which(df_raw$Year == "191!9"),
            which(df_raw$Year == "19114"))
df_raw <- df_raw[-ind_rm, ]
for(i in 1:(ncol(df_raw) - 1)){
  df_raw[, i + 1] <- gsub(',| ,|, |â???¢ |"|; ', '', df_raw[, i + 1])
}

ind_1909 <- grep("1909", df_raw$Year)
#df_raw$Year[ind_1909] <- gsub("^(\\d{4}).*", "\\1", df_raw$Year[ind_1909])
df_raw[ind_1909, 1:5] <- c("1909", "44262 15.1", "683927", "99.1", "677726")

ind_1919 <- which(df_raw$Year == "1919 1919")
df_raw[ind_1919, 1:8] <- c("1919", "73700 12.9", "952097", "216.3",
                        "2059421", "227", "300", "222030 5511")

ind_34_36 <- which(df_raw$Year %in% c(1934:1936))
df_raw[ind_34_36, ncol(df_raw)] <- c("3602 na", "30709 na", "26340 na")

ind_38_40 <- which(df_raw$Year %in% c(1938:1940))
df_raw[ind_38_40, 8] <- c("115784 9623", "54274 10430", "40557 11024")

ind_29 <- which(df_raw$Year == "1929")
df_raw[ind_29, ncol(df_raw)] <- paste(df_raw[ind_29, ncol(df_raw)], "na", collapse = " ")

list_str <- strsplit(as.character(df_raw$V10), " ")
df_netEx <- as.data.frame(do.call(rbind, list_str))
colnames(df_netEx) <- c("Net export", "Net export (% of production)")
df_netEx$`Net export` <- gsub("\\.", "", df_netEx$`Net export`)

list_str <- strsplit(as.character(df_raw$V2), " ")
df_area_yd <- as.data.frame(do.call(rbind, list_str))
colnames(df_area_yd) <- c("Area harvested", "Yield")

list_str <- strsplit(as.character(df_raw$V8), " ")
df_ex_im <- as.data.frame(do.call(rbind, list_str))
colnames(df_ex_im) <- c("Export", "Import")

df_1899_1940 <- data.frame(Year = df_raw$Year, df_area_yd,
                           df_raw[, c(3:7)], df_ex_im, df_netEx)

df_1899_1940 <- df_1899_1940 %>% mutate_if(is.factor, as.character)

df_1899_1940 <- as.data.frame(apply(df_1899_1940, 2, as.numeric))
df_1899_1940$Year <- as.integer(df_1899_1940$Year)

colnames(df_1899_1940)[-1] <- c("Area harvested (1000 hectares)", 
                                "Yield (metric tons / hectare)",
                      "Production (1000 metric tons)",
                      "Producer Price (USD / metric ton)",
                      "Farm value (1000 USD)",
                      "Kansas Cty market price (USD / metric ton)",
                      "Minneapolis market price (USD / metric ton)",
                      "Exports (1000 metric tons)",
                      "Imports (1000 metric tons)",
                      "Net Exports (1000 metric tons)",
                      "Net Exports (% of production)")
#----------------------------------------------------------------------------
# Get 1886-1898 wheat data too
this_file <- "Agstat-04-23-1941_1866-1939.pdf"
this_filepath <- paste0(this_dir, this_folder, this_file)
outlist <- extract_tables(this_filepath, pages = c(11), method = "stream")
#View(outlist[[1]])
#View(outlist[[2]])

df_raw <- as.data.frame(outlist[[1]])
df_raw <- df_raw %>% mutate_if(is.factor, as.character)
df_raw <- df_raw[-c(1:2, 6, 17, 38:nrow(df_raw)), ]
colnames(df_raw)[1] <- "Year"
df_raw$Year <- gsub(" ", "", df_raw$Year)
df_raw$Year <- gsub("(\\d{4}).*", "\\1", df_raw$Year)
df_raw$Year[which(df_raw$Year == "886.----")] <- "1886"
for(i in 1:(ncol(df_raw) - 1)){
  df_raw[, i + 1] <- gsub(',| ,|, |-|"', '', df_raw[, i + 1])
}
ind_1897 <- which(df_raw$Year == "1897")
df_raw[ind_1897, 8:11] <- c("221143", "2060", "220965", "36.5")
ind_1898 <- nrow(df_raw)
df_raw[ind_1898, 8:10] <- gsub("(\\d+) (\\d+)", "\\2", df_raw[ind_1898, 8:10])
df_raw[ind_1898, 11] <- "29.6"
ind_1888 <- which(df_raw$Year == "1888")
df_raw[ind_1888, c(2:3, 6)] <- c("34969 12.1", "423867", "393000 95")
ind_1878 <- which(df_raw$Year == "1878")
df_raw[ind_1878, 8:11] <- c("150503", "2074", "150253", "33.5")
ind_1877 <- which(df_raw$Year == "1877")
df_raw[ind_1877, 6] <- gsub("I|l", "1", df_raw[ind_1877, 6])

list_str <- strsplit(df_raw$V2, " ")
df_area_yd <- as.data.frame(do.call(rbind, list_str))
colnames(df_area_yd) <- c("Area harvested", "Yield")

list_str <- strsplit(df_raw$V6, " ")
df_fValue_mktPrice <- as.data.frame(do.call(rbind, list_str))
colnames(df_fValue_mktPrice) <- c("Farm value", "Kansas Cty market price")

df_raw$V4 <- df_raw$V3
df_raw[, 2:3] <- df_area_yd
df_raw[, 6:7] <- df_fValue_mktPrice

df_raw <- data.frame(df_raw[, 1:7], Minneap_price = NA, df_raw[, 8:ncol(df_raw)])
colnames(df_raw) <- colnames(df_1899_1940)
df_raw <- df_raw %>% mutate_if(is.factor, as.character)
df_raw <- as.data.frame(apply(df_raw, 2, as.numeric))
df_raw$Year <- as.integer(df_raw$Year)
df_1886_1898 <- df_raw
#----------------------------------------------------------------------------
df_wheat_1866_1940 <- as.data.frame(rbind(df_1886_1898, df_1899_1940))
#----------------------------------------------------------------------------
# Get 1908-1965 wheat data
#this_file <- "Agstat-04-23-1962_1866-1960.pdf"
this_file <- "Agstat-04-23-1967_1866-1965.pdf"
this_filepath <- paste0(this_dir, this_folder, this_file)
outlist <- extract_tables(this_filepath, pages = c(10), method = "stream")
#View(outlist[[1]])

# df_look <- as.data.frame(outlist[[1]])
# df_look <- df_look %>% mutate_if(is.factor, as.character)
# df_look$V4

df_raw <- as.data.frame(outlist[[3]])
df_raw <- df_raw %>% mutate_if(is.factor, as.character)
#df_raw <- df_raw[23:41, ]
colnames(df_raw)[1] <- "Year"
for(i in 1:(ncol(df_raw))){
  df_raw[, i] <- gsub(' ,|, |,|"|; |Â· |Â· ', '', df_raw[, i])
  df_raw[, i] <- gsub("' | '", "", df_raw[, i])
  df_raw[, i] <- gsub("!|L|I|l", "1", df_raw[, i])
  df_raw[, i] <- gsub("\\. ", "\\.", df_raw[, i])
  df_raw[, i] <- gsub("-|_", "", df_raw[, i])
  df_raw[, i] <- trimws(df_raw[, i])
  df_raw[, i] <- gsub("  ", " ", df_raw[, i])
}
df_raw$Year <- gsub("\\.", "", df_raw$Year)
df_raw$V2 <- gsub("\\.", "", df_raw$V2)


ind_1924 <- 5
df_raw[ind_1924, 1:5] <- c("1924 55706", "52463", "16.0", "841617", "1.25 1049443")

fix_repeatYr <- function(this_vec){
  this_vec[1] <- gsub("(\\d{4}) (\\d{4})(.*)", "\\1 \\3", this_vec[1])
  this_vec[c(2, 4)] <- gsub("(\\d+) (\\d+)", "\\2", this_vec[c(2, 4)])
  this_vec[3] <- gsub("(\\d+\\.\\d{1}) (\\d+\\.\\d{1})", "\\2", this_vec[3])
  return(this_vec)
}

ind_1929 <- grep("1929", df_raw$Year)
this_vec <- df_raw[ind_1929, ]
this_vec <- fix_repeatYr(this_vec)
df_raw[ind_1929, ] <- this_vec

ind_1934 <- grep("1934", df_raw$Year)
df_raw <- df_raw[-ind_1934[1], ]
ind_1934 <- grep("1934", df_raw$Year)
df_raw$Year[ind_1934] <- "1934 64064"

ind_35_36 <- grep("1935", df_raw$Year)
df_3536 <- data.frame(Year = c("1935 69611", "1936 73970"),
                      V2 = c("51305", "49125"),
                      V3 = c("12.2", "12.8"),
                      V4 = c("628227", "629880"),
                      V5 = c(".831 521915", "1.02 645465"),
                      V6 = c("1.05 1.26", "1.21 1.47"),
                      V7 = c(NA, NA),
                      V8 = c(NA, NA),
                      V9 = c("15929 46638", "21584 47924"))

df_raw <- as.data.frame(rbind(df_raw[1:(ind_35_36 - 1), ],
                              df_3536,
                              df_raw[(ind_35_36 + 1):nrow(df_raw), ]))

ind_1939 <- grep("1939", df_raw$Year)
df_raw <- df_raw[-ind_1939[1], ]
ind_1939 <- grep("1939", df_raw$Year)
this_vec <- df_raw[ind_1939, 2:4]
df_raw[ind_1939, 2:4] <- gsub("(.*) (.*)", "\\2", this_vec)

ind_1944 <- grep("1944", df_raw$Year)
this_vec <- df_raw[ind_1944, ]
this_vec <- fix_repeatYr(this_vec)
df_raw[ind_1944, ] <- this_vec

ind_1946 <- grep("1946", df_raw$Year)
df_raw[ind_1946, 5] <- "1.90 2201036"

ind_1949 <- grep("1949", df_raw$Year)
this_vec <- df_raw[ind_1949, ]
this_vec <- fix_repeatYr(this_vec)
df_raw[ind_1949, ] <- this_vec


ind_5051 <- grep("1950", df_raw$Year)
this_vec <- df_raw[ind_5051, ]
vec_1950 <- c(); vec_1951 <- c()
vec_1950[1] <- gsub("(\\d{4}) (\\d{4}) (\\d{5}) (\\d{5})", "\\1 \\3", this_vec[1])
vec_1951[1] <- gsub("(\\d{4}) (\\d{4}) (\\d{5}) (\\d{5})", "\\2 \\4", this_vec[1])
vec_1950[c(2:4)] <- gsub("(.*) (.*)", "\\1", this_vec[c(2:4)])
vec_1951[c(2:4)] <- gsub("(.*) (.*)", "\\2", this_vec[c(2:4)])
vec_1950[5] <- gsub("(\\d+\\.\\d{2}) (\\d+\\.\\d{2}) (\\d+) (\\d+)", "\\1 \\3", this_vec[c(5)])
vec_1951[5] <- gsub("(\\d+\\.\\d{2}) (\\d+\\.\\d{2}) (\\d+) (\\d+)", "\\2 \\4", this_vec[c(5)])
vec_1950[6] <- gsub("(\\d+\\.\\d{2}) (\\d+\\.\\d{2}) (\\d+\\.\\d{2}) (\\d+\\.\\d{2})", "\\1 \\3", this_vec[c(6)])
vec_1951[6] <- gsub("(\\d+\\.\\d{2}) (\\d+\\.\\d{2}) (\\d+\\.\\d{2}) (\\d+\\.\\d{2})", "\\2 \\4", this_vec[c(6)])
vec_1950[9] <- gsub("(\\d+) (\\d+) (\\d+) (\\d+)", "\\1 \\3", this_vec[c(9)])
vec_1951[9] <- gsub("(\\d+) (\\d+) (\\d+) (\\d+)", "\\2 \\4", this_vec[c(9)])
df_5051 <- as.data.frame(rbind(vec_1950, vec_1951))
colnames(df_5051)[1] <- "Year"
df_raw <- as.data.frame(rbind(df_raw[1:(ind_5051 - 1), ],
                df_5051,
                df_raw[(ind_5051 + 1):nrow(df_raw), ]))


ind_1954 <- grep("1954", df_raw$Year)
df_raw$Year[ind_1954] <- gsub("Â· ", "", df_raw$Year[ind_1954])
this_vec <- df_raw[ind_1954, ]
this_vec <- fix_repeatYr(this_vec)
df_raw[ind_1954, ] <- this_vec


ind_5758 <- grep("1956", df_raw$Year) + 1
this_vec <- df_raw[ind_5758, ]
vec_1957 <- c(); vec_1958 <- c()
vec_1957[1] <- "1957 49843"
vec_1958[1] <- "1958 56017"
vec_1957[c(2:4)] <- gsub("(.*) (.*)", "\\1", this_vec[c(2:4)])
vec_1958[c(2:4)] <- gsub("(.*) (.*)", "\\2", this_vec[c(2:4)])
vec_1957[5] <- gsub("(\\d+\\.\\d{2}) (\\d+\\.\\d{2}) (\\d+) (\\d+)", "\\1 \\4", this_vec[c(5)])
vec_1958[5] <- gsub("(\\d+\\.\\d{2}) (\\d+\\.\\d{2}) (\\d+) (\\d+)", "\\2 \\3", this_vec[c(5)])
vec_1957[6] <- gsub("(\\d+\\.\\d{2}) (\\d+\\.\\d{2}) (\\d+\\.\\d{2}) (\\d+\\.\\d{2})", "\\1 \\3", this_vec[c(6)])
vec_1958[6] <- gsub("(\\d+\\.\\d{2}) (\\d+\\.\\d{2}) (\\d+\\.\\d{2}) (\\d+\\.\\d{2})", "\\2 \\4", this_vec[c(6)])
vec_1957[9] <- gsub("(\\d+) (\\d+) (\\d+) (\\d+)", "\\1 \\3", this_vec[c(9)])
vec_1958[9] <- gsub("(\\d+) (\\d+) (\\d+) (\\d+)", "\\2 \\4", this_vec[c(9)])
df_5758 <- as.data.frame(rbind(vec_1957, vec_1958))
colnames(df_5758)[1] <- "Year"
df_raw <- as.data.frame(rbind(df_raw[1:(ind_5758 - 1), ],
                              df_5758,
                              df_raw[(ind_5758 + 1):nrow(df_raw), ]))

ind_1959 <- grep("1959", df_raw$Year)
this_vec <- df_raw[ind_1959, ]
this_vec <- gsub("  ", " ", this_vec)
this_vec <- fix_repeatYr(this_vec)
df_raw[ind_1959, ] <- this_vec

ind_1964 <- nrow(df_raw)
this_vec <- df_raw[ind_1964, ]
this_vec <- gsub("\\. ", "", this_vec)
this_vec <- gsub("f", "0", this_vec)
this_vec <- fix_repeatYr(this_vec)
df_raw[ind_1964, ] <- this_vec

vec_1965 <- c("1965 57361", "49560", "26.5", "1315613", "1.35 1774537",
              "1.61 1.86", NA, NA, "832311 546")

df_raw <- as.data.frame(rbind(df_raw, vec_1965))

for(i in 1:ncol(df_raw)){
  df_raw[, i] <- trimws(df_raw[, i])
  df_raw[, i] <- gsub("  ", " ", df_raw[, i])
  df_raw[, i] <- gsub("  ", " ", df_raw[, i])
  
}

list_str1 <- strsplit(df_raw[, 1], " ")
list_str5 <- strsplit(trimws(df_raw[, 5]), " ")
list_str6 <- strsplit(trimws(df_raw[, 6]), " ")
list_str9 <- strsplit(df_raw[, 9], " ")
list_list <- list(list_str1, list_str5, list_str6, list_str9)
list_df <- list()
for(i in 1:length(list_list)){
  list_str <- list_list[[i]]
  this_df <- as.data.frame(do.call(rbind, list_str))
  list_df[[i]] <- this_df
}

list_out <- list(list_df[[1]],
                 df_raw[, 2:4],
                 list_df[[2]],
                 list_df[[3]],
                 list_df[[4]])

df_wheat_1919_1965 <- as.data.frame(do.call(cbind, list_out))
colnames(df_wheat_1919_1965) <- c("Year",
                                  "Area planted (1000 hectares)",
                                  "Area harvested (1000 hectares)", 
                                  "Yield (metric tons / hectare)",
                                  "Production (1000 metric tons)",
                                  "Producer Price (USD / metric ton)",
                                  "Farm value (1000 USD)",
                                  "Kansas Cty market price (USD / metric ton)",
                                  "Minneapolis market price (USD / metric ton)",
                                  "Exports (1000 metric tons)",
                                  "Imports (1000 metric tons)")
df_wheat_1919_1965 <- df_wheat_1919_1965 %>% mutate_if(is.factor, as.character)

vec_1919 <- c("1919", "77440", "73700", "12.9", "952097", "2.16", "2059421",
              "2.42", "3.00", "222030", "5511")
df_wheat_1919_1965 <- as.data.frame(rbind(vec_1919, df_wheat_1919_1965))
df_wheat_1919_1965 <- as.data.frame((apply(df_wheat_1919_1965, 2, as.numeric)))
df_wheat_1919_1965$Year <- as.integer(df_wheat_1919_1965$Year)
ind_1948 <- which(df_wheat_1919_1965$Year == "1948")
df_wheat_1919_1965[ind_1948, ncol(df_wheat_1919_1965) - 1] <- 505304
# Convert cents to dollars
df_wheat_1866_1940$`Kansas Cty market price (USD / metric ton)` <-
  df_wheat_1866_1940$`Kansas Cty market price (USD / metric ton)` / 100
df_wheat_1866_1940$`Minneapolis market price (USD / metric ton)` <-
  df_wheat_1866_1940$`Minneapolis market price (USD / metric ton)` / 100
df_wheat_1866_1940$`Producer Price (USD / metric ton)` <-
  df_wheat_1866_1940$`Producer Price (USD / metric ton)` / 100

#----------------------------------------------------------------------------
# Check
# Note units are still in bushels, acres, etc., despite labels
df_plot1 <- df_wheat_1919_1965[, c(1, 3:ncol(df_wheat_1919_1965))]
df_plot2 <- df_wheat_1866_1940[, c(1, 2:(ncol(df_wheat_1866_1940) - 2))]
df_plot1$sortie <- "sortie 2"
df_plot2$sortie <- "sortie 1"

df_plot <- as.data.frame(rbind(df_plot1, df_plot2))
df_plot <- df_plot %>% gather(Element, Value,
                              `Area harvested (1000 hectares)`:`Imports (1000 metric tons)`)

gg <- ggplot(df_plot, aes(x = Year, y = Value,
                          group = sortie, color = sortie))
gg <- gg + geom_line(lwd = 1.2)
gg <- gg + facet_wrap(~Element, ncol = 3, scales = "free_y")
gg
#----------------------------------------------------------------------------
# combine
df_wheat_1866_1940$`Area planted (1000 hectares)` <- NA
df_wheat_1866_1940 <- df_wheat_1866_1940[, c("Year",
                                             "Area planted (1000 hectares)",
                                             "Area harvested (1000 hectares)",
                                             "Yield (metric tons / hectare)",
                                             "Production (1000 metric tons)",
                                             "Producer Price (USD / metric ton)",
                                             "Farm value (1000 USD)",
                                             "Kansas Cty market price (USD / metric ton)",
                                             "Minneapolis market price (USD / metric ton)",
                                             "Exports (1000 metric tons)",
                                             "Imports (1000 metric tons)")]

ind <- which(df_wheat_1866_1940$Year <= 1918)
df_wheat_1866_1965 <- rbind(df_wheat_1866_1940[ind, ],
                            df_wheat_1919_1965)
rm(df_wheat_1866_1940, df_wheat_1919_1965); gc()
#----------------------------------------------------------------------------
# Convert units
df_wheat_1866_1965$`Area harvested (1000 hectares)` <-
  acres_to_has * df_wheat_1866_1965$`Area harvested (1000 hectares)`
df_wheat_1866_1965$`Yield (metric tons / hectare)` <-
  wheatBushels_to_mTons / acres_to_has * df_wheat_1866_1965$`Yield (metric tons / hectare)`
df_wheat_1866_1965$`Production (1000 metric tons)` <- 
  wheatBushels_to_mTons * df_wheat_1866_1965$`Production (1000 metric tons)`
df_wheat_1866_1965$`Producer Price (USD / metric ton)` <-
  df_wheat_1866_1965$`Producer Price (USD / metric ton)` / (100 * wheatBushels_to_mTons)
df_wheat_1866_1965$`Kansas Cty market price (USD / metric ton)` <-
  df_wheat_1866_1965$`Kansas Cty market price (USD / metric ton)` / (100 * wheatBushels_to_mTons)
df_wheat_1866_1965$`Minneapolis market price (USD / metric ton)` <-
  df_wheat_1866_1965$`Minneapolis market price (USD / metric ton)` / (100 * wheatBushels_to_mTons)
df_wheat_1866_1965$`Exports (1000 metric tons)` <- 
  df_wheat_1866_1965$`Exports (1000 metric tons)` * wheatBushels_to_mTons
df_wheat_1866_1965$`Imports (1000 metric tons)` <- 
  df_wheat_1866_1965$`Imports (1000 metric tons)` * wheatBushels_to_mTons
#----------------------------------------------------------------------------
# Deflate dollar amounts
# https://liberalarts.oregonstate.edu/spp/polisci/faculty-staff/robert-sahr/inflation-conversion-factors-years-1774-estimated-2024-dollars-recent-years/download-conversion-factors
# © 2018 Robert C. Sahr, Political Science, Oregon State University, e-mail: Robert.Sahr@oregonstate.edu
this_file <- "USD Deflator 1774-present.csv"
this_filepath <- paste0(this_dir, this_folder, this_file)
df_deflator <- read.csv(this_filepath, stringsAsFactors = F)
df_deflator <- df_deflator[-c(1:29), 1:27]
colnames(df_deflator) <- df_deflator[1, ]
df_deflator <- df_deflator[-1, ]
colnames(df_deflator)[1] <- "Year"
df_deflator <- subset(df_deflator, Year %in% df_wheat_1866_1965$Year)
df_deflator <- df_deflator[, c("Year", "2017$ CF")]
colnames(df_deflator)[2] <- "2017 USD conversion factor"
df_deflator$`2017 USD conversion factor` <- as.numeric(df_deflator$`2017 USD conversion factor`)

df_wheat_1866_1965 <- merge(df_wheat_1866_1965, df_deflator, by = "Year")

df_wheat_1866_1965$`Producer Price (2017 USD / metric ton)` <-
  df_wheat_1866_1965$`Producer Price (USD / metric ton)` / 
  df_wheat_1866_1965$`2017 USD conversion factor`
df_wheat_1866_1965$`Farm value (thousand 2017 USD)` <-
  df_wheat_1866_1965$`Farm value (1000 USD)` / 
  df_wheat_1866_1965$`Farm value (1000 USD)`
df_wheat_1866_1965$`Kansas Cty market price (2017 USD / metric ton)` <- 
  df_wheat_1866_1965$`Kansas Cty market price (USD / metric ton)` / 
  df_wheat_1866_1965$`2017 USD conversion factor`
df_wheat_1866_1965$`Minneapolis market price (2017 USD / metric ton)` <-
  df_wheat_1866_1965$`Minneapolis market price (USD / metric ton)` /
  df_wheat_1866_1965$`2017 USD conversion factor`
#----------------------------------------------------------------------------
# Merge with US population data
this_file <- "US census population 1610-2020.csv"
this_filepath <- paste0(this_dir, this_folder, this_file)
df_pop <- read.csv(this_filepath, stringsAsFactors = F)
df_pop$`Population (1000 persons)` <- df_pop$Population / 1000
df_pop <- df_pop[, c("Year", "Population (1000 persons)")]

df_wheat_1866_1965 <- merge(df_wheat_1866_1965, df_pop, by = "Year", all.x = T)
# Make per capita vars
df_wheat_1866_1965$`Production per capita (metric tons)` <-
  df_wheat_1866_1965$`Production (1000 metric tons)` / 
  df_wheat_1866_1965$`Population (1000 persons)`
df_wheat_1866_1965$`Area planted per capita (hectares)` <-
  df_wheat_1866_1965$`Area planted (1000 hectares)` /
  df_wheat_1866_1965$`Population (1000 persons)`
#============================================================================
df_plot <- df_wheat_1866_1965
gg <- ggplot(df_plot, aes(x = Year, y = `Production per capita (metric tons)`))
gg <- gg + geom_line(lwd = 1.3)
gg <- gg + geom_vline(xintercept = 1933)
gg
#Note sudden drop in 1933 due to Roosevelt's AAA payments to farmers for NOT planting.
#to reduce stocks which kept producer prices low.
gg <- ggplot(df_plot, aes(x = Year, y = `Kansas Cty market price (2017 USD / metric ton)`))
gg <- gg + geom_line(lwd = 1.3)
gg

df_plot <- df_plot[, c("Year", "Producer Price (2017 USD / metric ton)",
                       "Production per capita (metric tons)",
                       "Area planted per capita (hectares)",
                       "Yield (metric tons / hectare)")]
df_plot <- df_plot %>% gather(Element, Value,
                              `Producer Price (2017 USD / metric ton)`:`Yield (metric tons / hectare)`)
df_plot <- df_plot %>% group_by(Element) %>%
  mutate(Diff = c(NA, NA, diff(Value, 2))) %>% as.data.frame()
  
gg <- ggplot(df_plot, aes(x = Year, y = Diff,
                          group = Element, color = Element))
gg <- gg + geom_line(lwd = 1.2)
gg <- gg + facet_wrap(~Element, ncol = 1, scales = "free_y")
gg

df_plot$`Producer Price t - 1` <-
  c(df_plot$`Producer Price (2017 USD / metric ton)`[-(ncol(df_plot))], NA)
gg <- ggplot(df_plot, aes(x = `Producer Price t - 1`,
                          y = `Production per capita (metric tons)`))
gg <- gg + geom_point(lwd = 1.3)
gg

# df_ccf <- df_plot[, c("Year", "Element", "Diff")] %>% spread(Element, Diff)
# ind_na <- which(is.na(df_ccf$`Producer Price (2017 USD / metric ton)`))
# df_ccf <- df_ccf[-ind_na, ]
# 
# #pacf(df_ccf$`Producer Price (2017 USD / metric ton)`)
# y <- df_ccf$`Production per capita (metric tons)`
# x <- df_ccf$`Producer Price (2017 USD / metric ton)`
# ccf(x, y, na.action = na.pass)
# #library("astsa")
# astsa::lag2.plot(x, y, 5)
#============================================================================
this_file <- "USDA US Wheat 1866-1965.csv"
this_filepath <- paste0(this_dir, this_folder, this_file)
write.csv(df_wheat_1866_1965, this_filepath, row.names = F)
