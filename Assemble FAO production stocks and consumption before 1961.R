library(tabulizer)
library(tidyverse)
library(rvest)
#============================================================================
this_dir <- "C:/Users/bensc/OneDrive/Documents/Data/"
this_folder <- "FAO Data/FAO before 1961/"
#============================================================================
# For FAO, "coarse grains" = rye, barley, oats, maize
#============================================================================
# Scrape data from pdf
this_file <- "State of Food and Ag 1965.pdf"
this_filepath <- paste0(this_dir, this_folder, this_file)
cereal_stocks_table <- 237
cereal_prod_table <- 228
these_pages <- c(cereal_stocks_table, cereal_prod_table)
outlist_prod <- extract_tables(this_filepath, pages = cereal_prod_table, method = "stream")
outlist_stocks <- extract_tables(this_filepath, pages = cereal_stocks_table, method = "stream")
#----------------------------------------------------------------------------
# Production

#View(outlist_prod[[2]])

df_prod <- as.data.frame(outlist_prod[[2]])
df_prod <- df_prod %>% mutate_if(is.factor, as.character)
df_prod <- df_prod[1:5, ]
colheads <- as.data.frame(outlist_prod[[1]])
colheads <- apply(colheads, 2, paste, collapse = " ")
colheads <- gsub("  ", " ", colheads)
colheads <- trimws(colheads, which = "both")
colheads[length(colheads)] <- "1964/1965"
colheads[10] <- "1963/1964"
colnames(df_prod) <- c("Item", colheads)
df_prod$Item[grep("Rice", df_prod$Item)] <- "Rice"
df_prod[2, 4] <- 62.02
df_prod <- df_prod %>% gather(Year, `Production (million metric tons)`, `Prewar average`:`1964/1965`)
df_prod$`Production (million metric tons)` <- gsub(",", "\\.", df_prod$`Production (million metric tons)`)
df_prod$`Production (million metric tons)` <- as.numeric(df_prod$`Production (million metric tons)`)
df_prod$Year <- gsub("Average", "Avg.", df_prod$Year)
df_prod$Year <- gsub("Prewar average", "Avg. 1934-38", df_prod$Year)
df_prod <- subset(df_prod, Year != "Avg. 1958-62")
colnames(df_prod)[2] <- "Ag. Year"
df_prod$Year <- df_prod$`Ag. Year`
df_prod$Year <- as.character(as.numeric(gsub("/.*", "", df_prod$Year)) + 1)
ind_na <- which(is.na(df_prod$Year))
df_prod$Year[ind_na] <- gsub("-.*", "", df_prod$`Ag. Year`[ind_na])
df_prod$Year[ind_na] <- gsub("Avg. ", "", df_prod$Year[ind_na])
df_prod$Year <- as.integer(df_prod$Year)

df_prod$Year[which(df_prod$Year == 1953)] <- 1958
df_prod$Year[which(df_prod$Year == 1948)] <- 1953
df_prod$Year[which(df_prod$Year == 1934)] <- 1939

df_35_58 <- subset(df_prod, Year %in% c(1939:1958))
df_prod <- subset(df_prod, !(Year %in% c(1939:1958)))
df_35_58 <- df_35_58 %>% spread(Item, `Production (million metric tons)`)

df_35_58 <- merge(data.frame(Year = 1935:1958),
                 df_35_58, by = "Year", all.x = T)
ind_3438 <- which(df_35_58$Year %in% c(1935:1938))
df_35_58[ind_3438, 2:ncol(df_35_58)] <- df_35_58[which(df_35_58$Year == 1939), 2:ncol(df_35_58)]
ind_4952 <- which(df_35_58$Year %in% c(1949:1952))
df_35_58[ind_4952, 2:ncol(df_35_58)] <- df_35_58[which(df_35_58$Year == 1953), 2:ncol(df_35_58)]
ind_5457 <- which(df_35_58$Year %in% c(1954:1957))
df_35_58[ind_5457, 2:ncol(df_35_58)] <- df_35_58[which(df_35_58$Year == 1958), 2:ncol(df_35_58)]

df_35_58 <- df_35_58 %>% gather(Item, `Production (million metric tons)`, Barley:Wheat)
df_35_58 <- df_35_58[, colnames(df_prod)]
df_prod <- rbind(df_35_58, df_prod)

df_prodCgrains <- subset(df_prod, Item %in% c("Barley", "Oats", "Maize"))
df_prod <- subset(df_prod, !(Item %in% c("Barley", "Oats", "Maize")))

df_prodCgrains$Item[grep("Barley|Oats|Maize", df_prodCgrains$Item)] <- "Coarse grains"

df_prodCgrains <- df_prodCgrains %>% group_by(Year, `Ag. Year`) %>%
  summarise(`Production (million metric tons)` = sum(`Production (million metric tons)`)) %>%
  as.data.frame()
df_prodCgrains$Item <- "Coarse grains"
df_prodCgrains <- df_prodCgrains[, colnames(df_prod)]

df_prod <- rbind(df_prod, df_prodCgrains)
rm(df_prodCgrains)

df_prodTot <- df_prod %>% group_by(Year, `Ag. Year`) %>%
  summarise(`Production (million metric tons)` = sum(`Production (million metric tons)`)) %>%
  as.data.frame()
df_prodTot$Item <- "Total cereals"
df_prodTot <- df_prodTot[, colnames(df_prod)]

df_prod <- rbind(df_prod, df_prodTot)
rm(df_prodTot)
#=============================================================================
# Population data
url_pop <- "https://www.worldometers.info/world-population/world-population-by-year/"
site <- read_html(url_pop)
df_pop_wmeter <- site %>%
  html_nodes("table") %>%
  .[[1]] %>%
  html_table(fill = T)
df_pop_wmeter <- subset(df_pop_wmeter[, c(1:2)], Year >= 1800)
colnames(df_pop_wmeter) <- c("Year", "Population (Wmeter)")
df_pop_wmeter$`Population (Wmeter)` <- as.numeric(gsub(",", "", df_pop_wmeter$`Population (Wmeter)`))
df_pop_wmeter <- merge(data.frame(Year = c(min(df_pop_wmeter$Year):max(df_pop_wmeter$Year))),
                    df_pop_wmeter, by = "Year", all.x = T)
df_pop_wmeter$Interpolated <- zoo::na.spline(df_pop_wmeter$`Population (Wmeter)`)
df_pop_interp <- subset(df_pop_wmeter[, c("Year", "Interpolated")], Year %in% c(1900:1950))
df_pop_wmeter <- subset(df_pop_wmeter[, c("Year", "Population (Wmeter)")], Year %in% c(1951:2020))
colnames(df_pop_interp)[2] <- "World Population (million persons)"
colnames(df_pop_wmeter)[2] <- "World Population (million persons)"
df_pop_interp$Type <- "Interpolated"
df_pop_wmeter$Type <- "Worldometer"
df_pop <- rbind(df_pop_interp, df_pop_wmeter)
df_pop$`World Population (million persons)` <- df_pop$`World Population (million persons)` / 10^6

this_dir <- "C:/Users/bensc/OneDrive/Documents/Data/"
this_folder <- "FAO Data/"
this_file <- "FAO_pop_all_regions.csv"
this_filepath <- paste0(this_dir, this_folder, this_file)
df_pop_FAO <- read.csv(this_filepath, stringsAsFactors = F)
df_pop_FAO <- subset(df_pop_FAO, Element == "Total Population - Both sexes")
df_pop_FAO <- df_pop_FAO[, c("Area", "Year", "Unit", "Value")]
df_pop_FAO$Value <- df_pop_FAO$Value / 10^3
colnames(df_pop_FAO)[ncol(df_pop_FAO)] <- "Population (FAO million persons)"
#df_pop_FAO$`Population (FAO)` <- df_pop_FAO$`Population (FAO)` * 1000
df_pop_FAO$Unit <- NULL
df_pop_FAO <- subset(df_pop_FAO, Area == "World")
df_pop_FAO$Area <- NULL
df_pop <- merge(df_pop, df_pop_FAO, by = "Year", all.x = T)
# FAO and woldometer population data (1950-present) are the same
#----------------------------------------------------------------------------
# Check population
df_plot <- df_pop
gg <- ggplot(df_plot, aes(x = Year,
                          y = `World Population (million persons)`,
                          group = Type, color = Type))
gg <- gg + geom_line(lwd = 1)
gg <- gg + annotate("point", x = c(1900, 1927), y = c(1600, 2000), color = "cyan")
gg <- gg + theme(axis.title.x = element_blank(),
                 axis.text.x = element_blank(),
                 legend.title = element_blank())
gg
#----------------------------------------------------------------------------
df_pop_a <- subset(df_pop, Year %in% c(1935:1958))
df_pop_a$Avg <- NA
df_pop_a$Avg[which(df_pop_a$Year %in% c(1935:1939))] <- "Avg. 1935-39"
df_pop_a$Avg[which(df_pop_a$Year %in% c(1949:1953))] <- "Avg. 1949-53"
df_pop_a$Avg[which(df_pop_a$Year %in% c(1954:1958))] <- "Avg. 1954-58"
df_pop_a <- df_pop_a %>% 
  group_by(Avg) %>% 
  summarise(`World Population (million persons)` = mean(`World Population (million persons)`))
df_pop_a <- df_pop_a[which(!is.na(df_pop_a$Avg)), ]
colnames(df_pop_a)[1] <- "Year"
df_pop_b <- subset(df_pop[, c("Year", "World Population (million persons)")], Year > 1958)
df_pop <- rbind(df_pop_a, df_pop_b)
colnames(df_pop)[1] <- "Year char"
df_pop$Year <- df_pop$`Year char`
df_pop$Year[grep("-39", df_pop$Year)] <- "1939"
df_pop$Year[grep("-53", df_pop$Year)] <- "1953"
df_pop$Year[grep("-58", df_pop$Year)] <- "1958"
df_pop$Year <- as.integer(df_pop$Year)

df_pop <- merge(data.frame(Year = 1935:1965), df_pop, by = "Year", all.x = T)

ind_3538 <- which(df_pop$Year %in% c(1935:1938))
df_pop[ind_3438, 2:ncol(df_pop)] <- df_pop[which(df_pop$Year == 1939), 2:ncol(df_pop)]
ind_4952 <- which(df_pop$Year %in% c(1949:1952))
df_pop[ind_4952, 2:ncol(df_pop)] <- df_pop[which(df_pop$Year == 1953), 2:ncol(df_pop)]
ind_5457 <- which(df_pop$Year %in% c(1954:1957))
df_pop[ind_5457, 2:ncol(df_pop)] <- df_pop[which(df_pop$Year == 1958), 2:ncol(df_pop)]
#=============================================================================
# Production per capita
df_prodPcap <- merge(df_prod, df_pop, by = "Year", all.x = T)
df_prodPcap$`Production / capita (metric tons)` <-
  df_prodPcap$`Production (million metric tons)` / df_prodPcap$`World Population (million persons)`
df_prodPcap$`Year char` <- NULL
#=============================================================================
df_plot <- df_prodPcap
df_plot <- df_plot %>% group_by(Item) %>%
  mutate(`Index (100 = Pre-war)` = 100 * `Production / capita (metric tons)` / `Production / capita (metric tons)`[1]) %>%
  as.data.frame()
this_ind <- which(df_plot$Year == 1939 & df_plot$Item == "Total cereals")
prodPcap_cereals_preWar <- df_plot$`Production / capita (metric tons)`[this_ind]
df_plot <- subset(df_plot, Year >= 1949)

gg <- ggplot(df_plot,
             aes(x = Year,
                 y = `Index (100 = Pre-war)`,
                 group = Item, color = Item))
gg <- gg + geom_line(lwd = 1.5)
gg


df_plot <- subset(df_plot, Item != "Total cereals")
df_plot$Year <- as.integer(df_plot$Year)
gg <- ggplot(df_plot,
             aes(x = Year,
                 y = `Production / capita (metric tons)`,
                 fill = Item))
gg <- gg + geom_area(position = "stack")
gg <- gg + geom_hline(yintercept = prodPcap_cereals_preWar, lwd = 1, linetype = "dashed")
gg <- gg + labs(title = "Production per capita")
gg
#============================================================================
# Stocks
# "Asian Exporters" = Burma, Thailand, Vietnam
# Coarse grains = Barley, oats, sorghum, maize, rye
df_stocks <- as.data.frame(outlist_stocks[[1]])
df_stocks <- df_stocks %>% mutate_if(is.factor, as.character)
df_stocks <- df_stocks[4:7, -c(2, 3, 12)]
colnames(df_stocks) <- c("Area", 1952:1965)
list_str <- strsplit(df_stocks$`1958`, " ")
this_df <- as.data.frame(do.call(rbind, list_str))
df_stocks[, 8:9] <- this_df
year_vec <- colnames(df_stocks)[-1]
df_stocks <- df_stocks %>% gather(Year, Stocks, `1952`:`1965`)
df_stocks$Stocks <- gsub(",", "\\.", df_stocks$Stocks)
df_stocks$Area[grep("nited States", df_stocks$Area)] <- "United States"
df_stocks$Area[grep("anada", df_stocks$Area)] <- "Canada"
df_stocks$Area[grep("rgentina", df_stocks$Area)] <- "Argentina"
df_stocks$Area[grep("ustralia", df_stocks$Area)] <- "Australia"
df_stocks$Item <- "Wheat"
#---
asian_rice <- c(0.7, 1.4, 1.6, 0.8, 0.7, 0.6, 0.5, 0.5, 0.3, 0.2, 0.2, 0.3, NA, NA)
US_rice <- c(0.1, NA, 0.2, 0.8, 1.1, 0.6, 0.6, 0.5, 0.4, 0.3, 0.2, 0.2, 0.2, 0.3)
df_rice_asia <- data.frame(Area = "Asian Exporters", Year = year_vec, Stocks = asian_rice)
df_rice_US <- data.frame(Area = "United States", Year = year_vec, Stocks = US_rice)
df_rice <- rbind(df_rice_US, df_rice_asia)
df_rice$Item <- "Rice"
#df_look <- df_rice %>% spread(Year, Stocks)
#---
US_coarse <- c(18.5, 24.7, 29.4, 37.3, 39.3, 44.4, 53.8, 61.6, 68, 77.2, 65.3, 58.2, 62.5, 52.8)
Canada_coarse <- c(3.6, 5.1, 5.6, 3.7, 4.3, 6.6, 5.2, 5.1, 4.7, 4.5, 2.8, 4.4, 5.7, 4)
df_coarse_US <- data.frame(Area = "United States", Year = year_vec, Stocks = US_coarse)
df_coarse_Canada <- data.frame(Area = "Canada", Year = year_vec, Stocks = Canada_coarse)
df_coarse <- rbind(df_coarse_Canada, df_coarse_US)
df_coarse$Item <- "Coarse grains"
#df_look <- df_coarse %>% spread(Year, Stocks)
#---
df_stocks <- as.data.frame(do.call(rbind, list(df_stocks, df_rice, df_coarse)))
df_stocks$Year <- as.integer(df_stocks$Year)
df_stocks$Stocks <- as.numeric(df_stocks$Stocks)
colnames(df_stocks)[3] <- "Stocks (million metric tons)"
#============================================================================
# Consumption




#============================================================================
#============================================================================
#============================================================================
# 1955 report:
this_file <- "State of Food and Ag 1955.pdf"
this_filepath <- paste0(this_dir, this_folder, this_file)
# wheat_stocks <- 160
# coarse_stocks <- 167
# #rice_stocks <- None indicated
# #---
# # wheat_cons <-
# # coarse_cons <-
# # rice_cons <- 
# #---
# wheat_prod <- 160
# coarse_prod <- 155
# rice_prod <- 168
# 
# these_pages <- c(wheat_stocks, coarse_stocks,
#                  rice_prod, coarse_prod)
# outlist <- extract_tables(this_filepath, pages = these_pages)
# 
# View(outlist[[2]])
