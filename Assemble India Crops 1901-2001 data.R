library(tabulizer)
library(tidyverse)
library(patchwork)
library(rvest)
# citation info: http://hermes-ir.lib.hit-u.ac.jp/rs/handle/10086/18943
# Title: Compilation of Agricultural Production Data in Areas Currently in India, Pakistan, and Bangladesh from 1901/02 to 2001/02
# Authors: Kurosaki, Takashi  
#  "1901/02 refers to the agricultural year beginning on July 1, 1901, and ending on June 30, 1902. In figures with limited space, it is shown as 1902."
url <- "http://hermes-ir.lib.hit-u.ac.jp/rs/bitstream/10086/18943/1/gd10-169.pdf"
outlist <- extract_tables(url, pages = c(29:32), method = "stream")

#View(outlist[[2]])
# First two are area data
# Second two are production data
# Production units are 1000 metric tons
# Area units are 1000 hectares
element_vec <- c(rep("Area (1000 hectares)", 2), rep("Production (1000 metric tons)", 2))
list_df <- list()
for(i in 1:length(outlist)){
  this_df <- as.data.frame(outlist[[i]])
  this_df <- this_df %>% mutate_if(is.factor, as.character)
  colnames(this_df) <- this_df[1, ]
  these_cols <- which(this_df[1, ] == "")
  these_cols <- these_cols[-1]
  these_colnames <- apply(this_df[2:4, these_cols], 2, paste, collapse = "")
  these_colnames[grep("andmustard", these_colnames)] <- "Rape and mustard"
  these_colnames <- gsub("-", "", these_colnames)
  #these_colnames[grep("Sugar-cane", these_colnames)] <- "Sugarcane"
  colnames(this_df)[these_cols] <- these_colnames
  this_df <- this_df[-c(1:4), ]
  colnames(this_df)[1] <- "Agricultural Year"
  this_df$Year <- as.integer(gsub("/.*", "", this_df$`Agricultural Year`)) + 1
  this_df[, 2:ncol(this_df)] <- as.data.frame(apply(this_df[, 2:ncol(this_df)], 2, as.numeric))
  this_df <- this_df %>% gather(Item, Value, Rice:Jute)
  #print(unique(this_df$Item))
  this_df <- subset(this_df, Item != "")
  this_df$Element <- element_vec[i]
  list_df[[i]] <- this_df
}

df_kurosaki <- as.data.frame(do.call(rbind, list_df))
df_kurosaki <- df_kurosaki %>% spread(Element, Value)
#==========================================================================
# Now get India population 1901-49
#https://censusindia.gov.in/census_data_2001/India_at_Glance/variation.aspx
#Use rvest to scrape and then interpolate using na.spline or na.approx
# (For Philippines use https://en.wikipedia.org/wiki/Census_in_the_Philippines)
url_pop <- "https://censusindia.gov.in/census_data_2001/India_at_Glance/variation.aspx"
site <- read_html(url_pop)
df_pop_GoI <- site %>%
  html_nodes("table") %>%
  .[[3]] %>%
  html_table(fill = T)
df_pop_GoI <- df_pop_GoI[-c(1:4, nrow(df_pop_GoI)), c(1:2)]
colnames(df_pop_GoI) <- c("Year", "Population (GoI)")
df_pop_GoI$`Population (GoI)` <- as.numeric(gsub(",", "", df_pop_GoI$`Population (GoI)`))
df_pop_GoI$Year <- as.integer(gsub("\\*", "", df_pop_GoI$Year))
df_pop_GoI <- merge(data.frame(Year = c(min(df_pop_GoI$Year):max(df_pop_GoI$Year))),
                    df_pop_GoI, by = "Year", all.x = T)
df_pop_GoI$`Population (GoI)` <- df_pop_GoI$`Population (GoI)` / 1000
colnames(df_pop_GoI)[ncol(df_pop_GoI)] <- "Population (GoI 1000 persons)"
this_dir <- "C:/Users/bensc/OneDrive/Documents/Data/"
this_folder <- "FAO Data/"
this_file <- "FAO_pop_all.csv"
this_filepath <- paste0(this_dir, this_folder, this_file)
df_pop_FAO <- read.csv(this_filepath, stringsAsFactors = F)
df_pop_FAO <- subset(df_pop_FAO, Element == "Total Population - Both sexes")
df_pop_FAO <- df_pop_FAO[, c("Area", "Year", "Unit", "Value")]
colnames(df_pop_FAO)[ncol(df_pop_FAO)] <- "Population (FAO 1000 persons)"
#df_pop_FAO$`Population (FAO)` <- df_pop_FAO$`Population (FAO)` * 1000
df_pop_FAO$Unit <- NULL
df_pop_FAO <- subset(df_pop_FAO, Area == "India")
df_pop_FAO$Area <- NULL
#---------------------------------------------------------------------------
df_pop_GoI$Interpolated <- zoo::na.approx(df_pop_GoI$`Population (GoI 1000 persons)`)
df_pop <- merge(df_pop_GoI, df_pop_FAO, by = "Year", all = T)
df_pop$`Population (1000 persons)` <- 
  c(df_pop$Interpolated[which(df_pop$Year <= 1949)],
    df_pop$`Population (FAO 1000 persons)`[which(df_pop$Year > 1949)])
#----------------------------------------------------------------------------
# Check
df_plot <- df_pop
df_plot$`Population (1000 persons)` <- NULL
colnames(df_plot)[2:ncol(df_plot)] <- c("Gov. of India", "Interpolated",
                                        "FAO")
df_plot <- df_plot %>% gather(Type, `Population (1000 persons)`,
                             `Gov. of India`:FAO)
df_plotGoI <- subset(df_plot, Type == "Gov. of India")
df_plot <- subset(df_plot, Type != "Gov. of India")
gg <- ggplot()
gg <- gg + geom_line(data = df_plot, aes(x = Year,
                                  y = `Population (1000 persons)`,
                                  group = Type,
                                  color = Type),
                     lwd = 1)
gg <- gg + geom_point(data = df_plotGoI,
                      aes(x = Year,
                          y = `Population (1000 persons)`,
                          group = Type,
                          color = Type),
                      size = 2)
gg <- gg + theme(axis.title.x = element_blank(),
                  axis.text.x = element_blank(),
                  legend.title = element_blank())
gg
#---------------------------------------------------------------------------
# Alternatively, can hindcast 1901-1949 by fitting a logistic model, but this
# results in serious underestimates.
# hindcast_years <- c(1901:1949)
# t_start <- -(length(hindcast_years) - 1)
# t_seq_before <- seq(t_start, 0)
# t_seq_after <- 1:nrow(df_pop_FAO)
# t_seq <- c(t_seq_before, 1:nrow(df_pop_FAO))
# #---------------------------------------------------------------------------
# df_mod <- df_pop
# df_mod$t <- t_seq#1:nrow(df_mod)
# df_mod$lPop <- log(df_mod$Population)
# #options(na.action = "na.exclude")
# df_mod <- df_mod[-c(1:49), ]
# mod <- nls(lPop ~ a - log(1 + exp(-(t - m) / s)),
#            df_mod,
#            start = list(a = 22, m = 78, s = 38))#,
#            #control=list(maxiter=50, tol=5e-8, nlsTol, warnOnly=T),
#            #na.action = na.exclude)
# #summary(mod)
# # plot(Population ~ Year, df_pop)
# # df_pop$t <- t_seq
# #---------------------------------------------------------------------------
# # Check
# lPophat <- predict(mod, df_mod$t, type = "response")
# df_mod$Modeled <- exp(lPophat)
# df_plot <- df_mod[, c("Year", "Population", "Modeled")]
# df_plot_diff <- df_plot
# df_plot_diff$Difference <- df_plot_diff$Modeled - df_plot_diff$Population
# colnames(df_plot)[2] <- "FAO"
# gathercols <- c("FAO", "Modeled")
# df_plot <- df_plot %>% gather_("Type", "Population", gathercols)
# #---------------------------------------------------------------------------
# gg <- ggplot(df_plot, aes(x = Year, y = Population, group = Type, color = Type))
# gg <- gg + geom_line(lwd = 1)
# gg1 <- gg + theme(axis.title.x = element_blank(),
#                   axis.text.x = element_blank(),
#                   legend.title = element_blank())
# gg <- ggplot(df_plot_diff, aes(x = Year, y = Difference))
# gg <- gg + geom_line(lwd = 1)
# gg <- gg + geom_hline(yintercept = 0, lwd = 1)
# gg2 <- gg + theme(axis.title.x = element_blank())
# gg1 + gg2 + plot_layout(ncol = 1, heights = c(2, 1))
# #---------------------------------------------------------------------------
# # Now for the hindcast
# logistic_fn <- function(t, a, m, s){a / (1 + exp(-(t - m) / s))}
# coefs <- coef(mod)
# a <- exp(coefs["a"])
# m <- coefs["m"]
# s <- coefs["s"]
# pop_hindcast <- logistic_fn(t_seq_before, a, m, s)
# df_pop_hindcast <- data.frame(Year = hindcast_years, Modeled = pop_hindcast)
# df_pop <- df_pop[, c("Year", "Population (GoI)", "Population (FAO)")]
# colnames(df_pop)[2:3] <- c("GoI", "FAO")
# df_pop <- merge(df_pop, df_pop_hindcast, by = "Year", all = T)
# df_pop <- df_pop %>% gather("Type", "Population", GoI:Modeled)
# 
# #---------------------------------------------------------------------------
# # Check
# df_plot <- df_pop
# df_plot$Population <- df_plot$Population / 10^6
# colnames(df_plot)[3] <- "Population (millions)"
# df_plotGoI <- subset(df_plot, Type == "GoI")
# df_plot <- subset(df_plot, Type %in% c("FAO", "Modeled"))
# df_interpolate <- data.frame(Year = df_plotGoI$Year, 
#                              Type = "Interpolation",
#                              `Population (millions)` = zoo::na.spline(df_plotGoI$`Population (millions)`))
# colnames(df_interpolate)[ncol(df_interpolate)] <- "Population (millions)"
# df_plot <- rbind(df_plot, df_interpolate)
# 
# gg <- ggplot()
# gg <- gg + geom_line(data = df_plot,
#                      aes(x = Year,
#                          y = `Population (millions)`,
#                          group = Type,
#                          color = Type),
#                      lwd = 1)
# gg <- gg + scale_x_continuous(breaks = seq(hindcast_years[1], 2018, 10))
# gg <- gg + geom_point(data = df_plotGoI,
#                       aes(x = Year,
#                           y = `Population (millions)`,
#                           group = Type,
#                           color = Type),
#                       size = 2)
# gg <- gg + theme(axis.title.x = element_blank(),
#                  legend.position = "bottom",
#                  legend.title = element_blank(),
#                  legend.spacing.x = unit(0.5, 'cm'))
# gg
#===========================================================================
#colnames(df_pop)[ncol(df_pop)] <- "Population"
df_out <- merge(df_kurosaki, df_pop, by = "Year")
df_out$`Production per capita (metric tons)` <-
  df_out$`Production (1000 metric tons)` / df_out$`Population (1000 persons)`
# Jowar = Sorghum
# Ragi = Finger Millet
# Bajra = Pearl Millet
# Gram = a pulse, I think
#---------------------------------------------------------------------------
# Check production per capita
df_plot <- subset(df_out, Item %in% c("Wheat", "Rice", "Maize"))
gg <- ggplot(df_plot, aes(x = Year,
                          y = `Production per capita (metric tons)`,
                          group = Item,
                          color = Item))
gg <- gg + geom_line(lwd = 1.5)
gg
#===========================================================================
this_folder <- "Kurosaki India data 1901-2001/"
this_file <- "Kurosaki India data 1901-2001.csv"
this_filepath <- paste0(this_dir, this_folder, this_file)
write.csv(df_out, this_filepath, row.names = F)

#df_check <- read.csv(this_filepath, stringsAsFactors = F)
