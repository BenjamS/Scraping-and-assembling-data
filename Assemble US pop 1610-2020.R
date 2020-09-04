library(rvest)
library(tidyverse)

url <- "https://en.wikipedia.org/wiki/Demographic_history_of_the_United_States"

site <- read_html(url)
df <- site %>%
  html_nodes("table") %>%
  .[[4]] %>%
  html_table()

df <- df[-1, ]
colnames(df) <- c("Year", "Population (US census)", "Growth rate (%)")

df$Year <- as.integer(gsub("(\\d{4})(.*)", "\\1", df$Year))
df$`Population (US census)` <- as.numeric(gsub(",", "", df$`Population (US census)`))
df$`Growth rate (%)`<- as.numeric(gsub("%", "", df$`Growth rate (%)`))
df <- merge(data.frame(Year = min(df$Year):max(df$Year)), df, by = "Year", all.x = T)
df$Population <- zoo::na.spline(df$`Population (US census)`)
df$Interpolated <- df$Population
df$Interpolated[which(!is.na(df$`Population (US census)`))] <- NA
df <- df[, c("Year", "Population", "Population (US census)", "Interpolated", "Growth rate (%)")]
#-----------------------------------------------------------------------------
# Check
df_plot <- df[, c("Year", "Population (US census)", "Interpolated")]
colnames(df_plot)[2:3] <- c("US census", "Interpolated")
df_plot <- df_plot %>% gather(Type, Population, `US census`:Interpolated)
df_plot1 <- subset(df_plot, Type == "US census")
df_plot2 <- subset(df_plot, Type == "Interpolated")

gg <- ggplot()
gg <- gg + geom_line(data = df_plot2,
                     aes(x = Year, y = Population,
                         group = Type, color = Type),
                     lwd = 1.2)
gg <- gg + geom_point(data = df_plot1, aes(x = Year, y = Population,
                                           group = Type, color = Type),
                      size = 2)
gg
#-----------------------------------------------------------------------------
write.csv(df, "US census population 1610-2020.csv", row.names = F)
#df_check <- read.csv("US census population 1610-2020.csv", stringsAsFactors = F)
