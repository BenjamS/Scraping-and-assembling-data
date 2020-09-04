library(tabulizer)
library(tidyverse)

#All-India Monthly and  Seasonal Rainfall  Series:  1871-1993 
this_dir <- "C:/Users/bensc/OneDrive/Documents/Data/"
this_folder <- "India Rainfall 1873-1993/"
this_file <- "Parthasarathy 1994 All-India monthly and seasonal rainfall series- 1871-1993.pdf"
this_filepath <- paste0(this_dir, this_folder, this_file)
outlist <- extract_tables(this_filepath, pages = c(3:5), method = "stream")


get_monthly_data <- function(this_col, n_num){
  this_col <- gsub("I|l|t", "1", this_col)
  this_col <- gsub("\"", "", this_col, fixed = T)
  this_col <- gsub("~", "", this_col)
  this_col <- gsub(" \\. ", "\\.", this_col)
  this_col <- gsub(",", "\\.", this_col)
  this_col <- gsub(" \\.", "\\.", this_col)
  this_col <- gsub("  ", " ", this_col)
  this_col <- str_remove_all(this_col, "\\s")
  if(n_num == 2){
    this_pattern <- "(\\d+\\.\\d)(\\d+\\.\\d)"
    replace_rule <- "\\1 \\2"
  }
  if(n_num == 3){
    this_pattern <- "(\\d+\\.\\d)(\\d+\\.\\d)(\\d+\\.\\d)"
    replace_rule <- "\\1 \\2 \\3"
  }
  if(n_num != 1){
    this_col <- str_replace(this_col, this_pattern, replace_rule)
    list_months <- strsplit(this_col, " ")
    month1_vec <- c(); month2_vec <- c(); month3_vec <- c()
    for(i in 1:length(list_months)){
      months_vec <- list_months[[i]]
      month1_vec[i] <- months_vec[1]
      month2_vec[i] <- months_vec[2]
      if(n_num == 3){
        month3_vec[i] <- months_vec[3]
      }
    }
    if(n_num == 2){
      df_out <- data.frame(month1_vec, month2_vec)
    }
    if(n_num == 3){
      df_out <- data.frame(month1_vec, month2_vec, month3_vec)
    }
    }else{
      df_out <- data.frame(this_col)
    }
  df_out <- df_out %>% mutate_if(is.factor, as.character)
  df_out <- as.data.frame(apply(df_out, 2, as.numeric))
  return(df_out)

}

#=============================================================================
list_out <- list()
for(i in 1:3){
  this_df <- as.data.frame(outlist[[i]])
  this_df <- this_df %>% mutate_if(is.factor, as.character)
  if(i == 3){
    this_df <- this_df[-c(10:13), ]
  }
  this_df <- this_df[-c(1:2), 1:6]
  col1 <- this_df[, 1] 
  col1 <- gsub("I", "1", col1)
  year_vec <- substr(col1, start = 1, stop = 4)
  df_year <- data.frame(Year = year_vec)
  col1 <- sub(".*? ", "", col1)
  df_janFeb <- get_monthly_data(col1, 2)
  colnames(df_janFeb) <- c("Jan", "Feb")
  col2 <- this_df[, 2]
  if(i == 3){
    df_marAprMay <- get_monthly_data(col2, 3)
    colnames(df_marAprMay) <- c("Mar", "Apr", "May")
  }else{
    df_marApr <- get_monthly_data(col2, 2)
    colnames(df_marApr) <- c("Mar", "Apr")
    col3 <- this_df[, 3]
    df_may <- get_monthly_data(col3, n_num = 1)
    colnames(df_may) <- "May"
    df_marAprMay <- cbind(df_marApr, df_may)
  }
  col4 <- this_df[, 4]
  df_junJulAug <- get_monthly_data(col4, 3)
  colnames(df_junJulAug) <- c("Jun", "Jul", "Aug")
  col5 <- this_df[, 5]
  df_sept <- get_monthly_data(col5, n_num = 1)
  colnames(df_sept) <- "Sep"
  col6 <- this_df[, 6]
  df_octNovDec <- get_monthly_data(col6, 3)
  colnames(df_octNovDec) <- c("Oct", "Nov", "Dec")
  list_df <- list(df_year, df_janFeb, df_marAprMay, df_junJulAug, df_sept, df_octNovDec)
  df_out <- as.data.frame(do.call(cbind, list_df))
  df_out$Year <- as.integer(as.character(df_out$Year))
  list_out[[i]] <- df_out
}

df_rainIndia <- as.data.frame(do.call(rbind, list_out))
df_rainIndia <- df_rainIndia %>% 
  gather(Month, `Rainfall (mm)`, Jan:Dec)
df_rainIndia$Season <- NA
u <- df_rainIndia$Month
df_rainIndia$Season[which(u %in% c("Jan", "Feb"))] <- "Winter"
df_rainIndia$Season[which(u %in% c("Mar", "Apr", "May"))] <- "Summer"
df_rainIndia$Season[which(u %in% c("Jun", "Jul", "Aug", "Sep"))] <- "Southwest Monsoon"
df_rainIndia$Season[which(u %in% c("Oct", "Nov", "Dec"))] <- "Post Monsoon"
df_rainIndia$Date <- as.Date(paste(1, df_rainIndia$Month, df_rainIndia$Year, sep = "-"), "%d-%b-%Y")
#=============================================================================
df_plot <- subset(df_rainIndia, Year %in% c(1955:1970))

gg <- ggplot(df_plot, aes(x = Date, y = `Rainfall (mm)`))
gg <- gg + geom_line(lwd = 1)
gg <- gg + scale_x_date(date_labels = "%b-%Y", date_breaks = "6 month")
gg <- gg + theme(axis.text.x = element_text(angle = 60, hjust = 1))
gg

df_plot <- subset(df_rainIndia, Season == "Southwest Monsoon")
df_plot <- df_plot %>% group_by(Year) %>% 
  summarise(`Rainfall (mm)` = sum(`Rainfall (mm)`))
mu_rain <- mean(df_plot$`Rainfall (mm)`)
sd_rain <- sd(df_plot$`Rainfall (mm)`)
df_plot$`Anomaly (mm)` <- df_plot$`Rainfall (mm)` - mu_rain


df_famines <- data.frame(xmin = c(1876, 1896, 1918, 1943, 1965, 1970),
                         xmax = c(1879, 1902, 1919, 1945, 1966, 1973),
                         `Deaths India (low)` = c(6.1*10^6, 6.1*10^6,
                                                  15*10^6, 2*10^6, 0, 7*10^4),
                         `Deaths India (high)` = c(10.3*10^6, 19*10^6,
                                                  15*10^6, 2*10^6, 0, 7*10^4),
                         `Deaths China (low)` = c(9.5*10^6, 10, NA, NA, NA, NA),
                         `Deaths China (high)` = c(13*10^6, 10, NA, NA, NA, NA),
                         `Deaths Brazil (low)` = c(0.5, NA, NA, NA, NA, NA),
                         `Deaths Brazil (high)` = c(1, NA, NA, NA, NA, NA)
                         )
u <- colnames(df_famines)
colnames(df_famines) <- gsub("\\.", " ", u)
colnames(df_famines) <- gsub("  ", " ", colnames(df_famines))
colnames(df_famines) <- gsub(" $","", colnames(df_famines), perl=T)

# gg <- gg + geom_rect(data = df_probGradient, aes(xmin = xmin, xmax = xmax,
#                                                  ymin = -Inf, ymax = Inf, fill = predProbs), alpha = 0.7)
# gg <- gg + scale_fill_gradient2(low = "darkmagenta", mid = "white", high = "green", midpoint = 0.5)

gg <- ggplot()
gg <- gg + geom_rect(data = df_famines, aes(xmin = xmin, xmax = xmax,
                                                 ymin = -Inf, ymax = Inf), alpha = 0.7)
gg <- gg + geom_line(data = df_plot, aes(x = Year, y = `Anomaly (mm)`),lwd = 1)
gg <- gg + geom_hline(yintercept = c(0, 2 * sd_rain, -2 * sd_rain), color = "coral")
#gg <- gg + geom_vline(xintercept = c(1876, 1896, 1918, 1965, 1968, 1972), color = "red")
gg
#=============================================================================
this_file <- "India Rainfall 1871-1993.csv"
this_filepath <- paste0(this_dir, this_folder, this_file)
write.csv(df_rainIndia, this_filepath, row.names = F)