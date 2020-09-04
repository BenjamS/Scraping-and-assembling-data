library(tabulizer)
#library(rvest)
library(tidyverse)
#-----------------------------------------------------------------------------
# Special thanks to Linda Eells and Frances Homans at the University of Minnesota
# for helping me track this down and scanning it and making it publicly available
# here: https://ageconsearch.umn.edu/record/287593
# (Here I use a shortened version of the pdf with just the pages with rice/corn
# yield, area, and production data, sent to my ciat email account by Linda)
#-----------------------------------------------------------------------------
# NOTE that, as this is a scanned pdf, you must OCR-convert the pages you want
# to extract tables from. You do this by opening the pdf and pressing
# the OCR button in the PDF Viewer (Adobe or PDF-X, etc.).
#-----------------------------------------------------------------------------
# Rice data is on pages 1-2
# Corn data is on page 16
#-----------------------------------------------------------------------------
this_dir <- "C:/Users/bensc/OneDrive/Documents/Data/"
this_folder <- "Philippines Rice and Corn Per Capita 1909-1963/"
this_file <- "Mangahas et al 1970 IRRI Technical Bulletin 9 p 168-199.pdf"
this_filepath <- paste0(this_dir, this_folder, this_file)
outlist <- extract_tables(this_filepath, pages = c(16))
#View(outlist[[1]])
#View(outlist[[2]])

this_df <- as.data.frame(outlist[[1]])
this_df <- this_df %>% mutate_if(is.factor, as.character)
this_df <- this_df[, c(15:ncol(this_df))]
colnames(this_df) <- this_df[1, ]
this_df[, 7] <- NULL
this_df <- this_df[-c(1, 2, 6), ]
this_df1 <- this_df[, 1:4]
this_df2 <- this_df[, 5:8]
this_df1$`Crop year`[5] <- "1913/14"
this_df1$`Crop year`[10] <- "1918/19"
this_df1[12, -1] <- this_df1[13, -1]
this_df1 <- this_df1[-13, ]
this_df1a <- this_df1[11:nrow(this_df1), ]
this_df1 <- this_df1[1:11, ]
list_str <- strsplit(as.character(this_df1[10, -1]), " ")
vec1 <- c(list_str[[1]][1], list_str[[2]][1], list_str[[3]][1])
vec2 <- c(list_str[[1]][2], list_str[[2]][2], list_str[[3]][2])
this_df1[10, -1] <- vec1
this_df1[11, -1] <- vec2
this_df1$`Crop year`[11] <- "1919/20"
this_df1 <- rbind(this_df1, this_df1a)
ind <- which(this_df1$`Crop year` == "1923/24")
this_df1 <- this_df1[-ind, ]
this_df1$`Crop year`[ind] <- "1923/24"
this_df1[ind, 3:4] <- c("533230", "14.68")
this_df1[ind - 1, 4] <- "14.04"
this_df1[ind + 1, 4] <- strsplit(this_df1[ind + 1, 4], " ")[[1]][2]
this_df1$`Crop year`[(ind + 1):(ind + 2)] <- c("1924/25", "1925/26")
this_df1$`Crop year`[nrow(this_df1)] <- gsub("5", "", this_df1$`Crop year`[nrow(this_df1)])
this_df1$`Crop year`[nrow(this_df1)] <- gsub("I", "1", this_df1$`Crop year`[nrow(this_df1)])
this_df1$`Crop year`[nrow(this_df1)] <- gsub("  ", " ", this_df1$`Crop year`[nrow(this_df1)])
yrs <- strsplit(this_df1[nrow(this_df1), 1], " ")[[1]]
this_df1$Production[nrow(this_df1)] <- gsub(" ,", ",", this_df1$Production[nrow(this_df1)])
this_df1$Area[nrow(this_df1)] <- gsub(" ", "", this_df1$Area[nrow(this_df1)])
this_df1$Area[nrow(this_df1)] <- gsub("(\\d+,\\d{3})(\\d+,\\d{3})(\\d+,\\d{3})", "\\1 \\2 \\3", this_df1$Area[nrow(this_df1)])
prod <- strsplit(this_df1[nrow(this_df1), 2], " ")[[1]]
area <- strsplit(this_df1[nrow(this_df1), 3], " ")[[1]]
yld <- strsplit(this_df1[nrow(this_df1), 4], " ")[[1]]
df_add <- data.frame(`Crop year` = c(yrs, "1929/30"),
                     Production = c(prod, "6339040"),
                     Area = c(area, "516970"),
                     Yield = c(yld, "12.26"))
#df_add <- df_add %>% mutate_if(is.factor, as.character)
colnames(df_add) <- c("Crop year", "Production", "Area", "Yield")
ind <- nrow(df_add) - 1
save_this <- df_add[ind, 2:3]
df_add[ind, 2:3] <- df_add[ind - 1, 2:3]
df_add[ind - 1, 2:3] <- save_this
this_df1 <- rbind(this_df1[-nrow(this_df1), ], df_add)
#---
ind <- which(this_df2$`Crop year` == "1936/37"):nrow(this_df2)
this_df2a <- this_df2[ind, ]
this_df2a[2, 2] <- "8782420"
yrs <- c("1939/40", "1940/41", "1941/42")
prod <- strsplit(this_df2a$Production[nrow(this_df2a)], " ")[[1]]
area <- strsplit(this_df2a$Area[nrow(this_df2a)], " ")[[1]]
yld <- c("11", "9.7", "10.24")
df_add <- data.frame(Year = yrs, Production = prod, Area = area, Yield = yld)
colnames(df_add)[1] <- "Crop year"
this_df2a <- rbind(this_df2a[-nrow(this_df2a), ], df_add)
df_ww2 <- as.data.frame(matrix(NA, nrow = 3, ncol = 4))
colnames(df_ww2) <- c("Crop year", "Production", "Area", "Yield")
df_ww2$`Crop year` <- c("1942/43", "1943/44", "1944/45")
df_4649 <- data.frame(Year = c("1945/46", "1946/47", "1947/48", "1948/49"),
                      Production = c("5811610", "8284100", "9105470", "9369600"),
                      Area = c("571090", "812300", "826490", "866200"),
                      Yield = c("10.18", "10.2", "11.02", "10.82"))
colnames(df_4649)[1] <- "Crop year"
this_df2a <- rbind(this_df2a, df_ww2, df_4649)
#--
ind <- 1:which(this_df2$`Crop year` == "1961/62")
this_df2b <- this_df2[ind, ]
df_temp <- data.frame(Year = c("1958/59", "1959/60"),
                      Production = c("17,823,000", "20,443,400"),
                      Area = c("2,106,980", "1,845,540"),
                      Yield = c("8.46", "11.08"))
colnames(df_temp)[1] <- "Crop year"
this_df2b <- rbind(this_df2b[1:9, ], df_temp, this_df2b[11:nrow(this_df2b), ])
this_df2b[nrow(this_df2b), 4] <- "11"
df_temp <- data.frame(Year = "1962/63",
                      Production = "22330700",
                      Area = "1949510",
                      Yield = "11.45")
colnames(df_temp)[1] <- "Crop year"
this_df2b <- rbind(this_df2b, df_temp)
#--
this_df2 <- rbind(this_df2a, this_df2b)
#---------------------
this_df3 <- as.data.frame(outlist[[2]])
this_df3 <- this_df3 %>% mutate_if(is.factor, as.character)
this_df3 <- this_df3[, c(10:ncol(this_df3))]
this_df3a <- this_df3[, c(1, 3, 5, 7)]
colnames(this_df3a) <- c("Crop year", "Production", "Area", "Yield")
this_df3b <- this_df3[, c(9, 12, 14, 16)]
colnames(this_df3b) <- c("Crop year", "Production", "Area", "Yield")
this_df3a$`Crop year` <- gsub(" Q", "", this_df3a$`Crop year`)
this_df3a <- this_df3a[-nrow(this_df3a), ]
this_df3a$`Crop year` <- gsub("(\\d+/\\d{2})(\\d+/\\d{2})", "\\1 \\2", this_df3a$`Crop year`)
this_df3a$Yield <- gsub(" \\.", "\\.", this_df3a$Yield)
this_df3a$Yield <- gsub("(\\d+\\.\\d{2})(\\d+\\.\\d{2})", "\\1 \\2", this_df3a$Yield)

u <- strsplit(as.character(this_df3a[1, ]), " ")
vec1 <- c(u[[1]][1], u[[2]][1], u[[3]][1], u[[4]][1])
vec2 <- c(u[[1]][2], u[[2]][2], u[[3]][2], u[[4]][2])

u <- strsplit(as.character(this_df3a[2, ]), " ")
vec3 <- c(u[[1]][1], u[[2]][1], u[[3]][1], u[[4]][1])
vec4 <- c(u[[1]][2], u[[2]][2], u[[3]][2], u[[4]][2])

u <- strsplit(as.character(this_df3a[3, ]), " ")
vec5 <- c(u[[1]][1], u[[2]][1], u[[3]][1], u[[4]][1])
vec6 <- c(u[[1]][2], u[[2]][2], u[[3]][2], u[[4]][2])

this_df3a <- as.data.frame(do.call(rbind, list(vec1, vec2, vec3, vec4, vec5, vec6)))
this_df3a <- this_df3a %>% mutate_if(is.factor, as.character)

colnames(this_df3a) <- c("Crop year", "Production", "Area", "Yield")
this_df3a$Production[nrow(this_df3a)] <- "6370690"
df_corn <- rbind(this_df1, this_df3a, this_df2)
#---------------------------------------------------------------------------
df_corn$Yield <- as.numeric(gsub(",", "\\.", df_corn$Yield))
df_corn$Production <- as.numeric(gsub(",", "", df_corn$Production))
df_corn$Area <- as.numeric(gsub(",| ", "", df_corn$Area))
df_corn$Year <- as.integer(gsub("/.*", "", df_corn$`Crop year`)) + 1
colnames(df_corn)[1] <- "Ag. Year"
df_corn <- df_corn[, c("Year", "Ag. Year", "Production", "Area", "Yield")]
# Convert cavans to metric tons (*44 / 1000)
df_corn$Production <- 44 / 1000 * df_corn$Production
df_corn$Yield <- 44 / 1000 * df_corn$Yield
colnames(df_corn)[3:5] <- c("Production (metric tons)", "Area (hectares)", "Yield (metric tons / hectare)")
df_corn$Item <- "Maize"
df_corn <- df_corn[, c("Year", "Ag. Year", "Item",
                       "Production (metric tons)",
                       "Area (hectares)",
                       "Yield (metric tons / hectare)")]
ind <- which(df_corn$`Ag. Year` == "1932/39")
df_corn$Year[ind] <- 1939
df_corn$`Ag. Year`[ind] <- "1938/39"
#===========================================================================
# Bring in the rice data
this_file <- "Philippines rice data 1909-1963.csv"
this_filepath <- paste0(this_dir, this_folder, this_file)
df_rice <- read.csv(this_filepath, stringsAsFactors = F)
colnames(df_rice) <- c("Ag. Year", "Year", "Production (metric tons)",
                       "Area (hectares)", "Yield (metric tons / hectare)")
df_rice$`Production (metric tons)` <- 44 / 1000 * df_rice$`Production (metric tons)` 
df_rice$`Yield (metric tons / hectare)` <- 44 / 1000 * df_rice$`Yield (metric tons / hectare)`
df_rice$Item <- "Rice"
df_rice <- df_rice[, c("Year", "Ag. Year", "Item",
                       "Production (metric tons)",
                       "Area (hectares)",
                       "Yield (metric tons / hectare)")]
#---------------------------------------------------------------------------
df_crop <- rbind(df_corn, df_rice)
#===========================================================================
# Now get Philippines population data 1909-1963
# url <- "https://en.wikipedia.org/wiki/Census_in_the_Philippines"
# site <- read_html(url)
# df_pop <- site %>%
#   html_nodes("table") %>%
#   .[[1]] %>%
#   html_table(fill = T)
# Better source: http://www.cicred.org/Eng/Publications/pdf/c-c42.pdf
this_folder <- "Philippines Rice and Corn Per Capita 1909-1963/"
this_file <- "Philippines Early Population.pdf"
this_filepath <- paste0(this_dir, this_folder, this_file)
outlist <- extract_tables(this_filepath, pages = 20)
#View(outlist[[1]])
df_pop <- as.data.frame(outlist[[1]])
df_pop <- df_pop %>% mutate_if(is.factor, as.character)
colnames(df_pop) <- df_pop[1, ]
df_pop <- df_pop[-c(1, 2), c("Year", "Population")]
df_pop$Population <- as.numeric(gsub(",|'", "", df_pop$Population))
df_pop <- subset(df_pop, Year >= 1850)
df_pop <- merge(data.frame(Year = min(df_pop$Year):max(df_pop$Year)),
                df_pop, by = "Year", all.x = T)
colnames(df_pop)[2] <- "Population (GoP)"
#---
this_dir <- "C:/Users/bensc/OneDrive/Documents/Data/"
this_folder <- "FAO Data/"
this_file <- "FAO_pop_all.csv"
this_filepath <- paste0(this_dir, this_folder, this_file)
df_pop_FAO <- read.csv(this_filepath, stringsAsFactors = F)
df_pop_FAO <- subset(df_pop_FAO, Element == "Total Population - Both sexes")
df_pop_FAO <- df_pop_FAO[, c("Area", "Year", "Unit", "Value")]
df_pop_FAO$Value <- df_pop_FAO$Value * 1000
colnames(df_pop_FAO)[ncol(df_pop_FAO)] <- "Population (FAO)"
#df_pop_FAO$`Population (FAO)` <- df_pop_FAO$`Population (FAO)` * 1000
df_pop_FAO$Unit <- NULL
df_pop_FAO <- subset(df_pop_FAO, Area == "Philippines")
df_pop_FAO$Area <- NULL
#---------------------------------------------------------------------------
df_pop <- merge(df_pop, df_pop_FAO, by = "Year", all.x = T)
df_pop$Population <- df_pop$`Population (GoP)`
# ind_fao <- which(!is.na(df_pop$`Population (FAO)`))
# df_pop$Population[ind_na] <- df_pop$`Population (FAO)`[ind_na]
df_pop$Population <- zoo::na.spline(df_pop$Population)
ind_na <- unique(c(which(is.na(df_pop$`Population (GoP)`)),
                 which(!is.na(df_pop$`Population (FAO)`))))
df_pop$Interpolated <- NA
df_pop$Interpolated[ind_na] <- df_pop$Population[ind_na]
df_pop <- df_pop[, c("Year", "Population (GoP)", "Population (FAO)", "Interpolated", "Population")]
#---------------------------------------------------------------------------
# Check pop
df_plot <- df_pop[-ncol(df_pop)]
colnames(df_plot)[2:3] <- c("Philippines Gov.", "FAO")
df_plot <- df_plot %>% gather(Type, Population, `Philippines Gov.`:Interpolated)
df_plot1 <- subset(df_plot, Type %in% c("FAO", "Interpolated"))
df_points <- subset(df_plot, Type == "Philippines Gov.")
gg <- ggplot()
gg <- gg + geom_line(data = df_plot1,
                     aes(x = Year, y = Population,
                         group = Type,
                         color = Type),
                     lwd = 1.3)
gg <- gg + geom_point(data = df_points,
                      aes(x = Year, y = Population,
                          group = Type,
                          color = Type),
                      size = 2)
gg <- gg + theme(axis.title.x = element_blank(),
                 legend.title = element_blank())
gg
#===========================================================================
#===========================================================================
df_pcap <- merge(df_crop, df_pop, by = "Year")
df_pcap$`Production / capita (metric tons)` <- df_pcap$`Production (metric tons)` / df_pcap$Population
#---------------------------------------------------------------------------
# Check
df_plot <- subset(df_pcap[, c("Year", "Item", "Production / capita (metric tons)")], Year >= 1910)
df_look <- df_plot %>% spread(Item, `Production / capita (metric tons)`)
df_plot[48:49,]
gg <- ggplot(df_plot, aes(x = Year,
                          y = `Production / capita (metric tons)`,
                          fill = Item))
gg <- gg + geom_area(position = "stack")
gg <- gg + theme(axis.title.x = element_blank(),
            legend.title = element_blank())
gg
#===========================================================================
#===========================================================================
this_folder <- "Philippines Rice and Corn Per Capita 1909-1963/"
this_file <- "Philippines Rice and Maize Per Capita 1909-1963.csv"
this_filepath <- paste0(this_dir, this_folder, this_file)
write.csv(df_pcap, this_filepath, row.names = F)
