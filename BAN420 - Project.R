# Install packages:

# install.packages("quantmod")
# install.packages("rvest")
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("magrittr")
# install.packages("lubridate")
# install.packages("data.table")
# install.packages("ggplot2")

#-------------------------------------------------------------------------------

# Load packages:
library(quantmod)
library(rvest)
library(dplyr)
library(tidyr)
library(magrittr)
library(lubridate)
library(data.table)
library(ggplot2)

#-------------------------------------------------------------------------------

# Create values with tickers, from index:
dow30 <- read_html("https://www.cnbc.com/dow-30/") %>%
  html_nodes(".text a") %>%
  html_text()

nasdaq100 <- read_html("https://www.cnbc.com/nasdaq-100/") %>% 
  html_nodes(".text a") %>% 
  html_text()

#-------------------------------------------------------------------------------

# Function for downloading tickers:
tickers <- function(ticker,
                    index,
                    date.from,
                    date.to) {
    
# If date.to is included:
if(!is.na(date.to)) {
  getSymbols(c(ticker), from = date.from, to = date.to, auto.assign = TRUE)
} else{
  getSymbols(c(ticker), from = date.from, auto.assign = TRUE)}

# Create data frame and name columns:
eval(parse(text = paste("temp1 = data.frame(", gsub("\\^", "", ticker[1]), ")", sep = "")))
name <- c("Date", gsub(".*\\.", "", colnames(temp1)), "Ticker")
setDT(temp1, keep.rownames = TRUE)
temp1 <- cbind(temp1, ticker[1]) %>% set_colnames(.,name) 

# If the function includes more than one ticker, loop them all into one data frame
if(length(ticker)>1){
  for (i in 2:length(ticker)) {
  
eval(parse(text = paste("temp2 = data.frame(", ticker[i], ")", sep = "")))
setDT(temp2, keep.rownames = TRUE)
temp2 <- cbind(temp2, ticker[i]) %>% set_colnames(.,name)
temp1 <- bind_rows(temp1,temp2)
  }
}
  
# If the function includes an index, add the index to the data frame
if(!is.na(index)){
  if(!is.na(date.to)) {
    getSymbols(c(index), from = date.from, to = date.to, auto.assign = TRUE)
    } else{
    getSymbols(c(index), from = date.from, auto.assign = TRUE)}
  
eval(parse(text = paste("temp2 = data.frame(", gsub("\\^", "", index), ")", sep = "")))
setDT(temp2, keep.rownames = TRUE)
temp2 <- cbind(temp2, index) %>% set_colnames(., name)

# Combine ticker and index data frame
temp1 <- bind_rows(temp1, temp2)
  }

# Make date column dates
temp1 <- mutate(temp1, Date = ymd(temp1$Date))

# Add a column for normalized closing data
temp1 <- temp1 %>% group_by(Ticker) %>% mutate(Normalized = (Close/(Close[1]))*100)

}

#-------------------------------------------------------------------------------

# Create a function for plotting stocks
plot.stocks <- function(
  x,
  y,
  color,
  title,
  subtitle = "",
  ylab = y,
  xlab = x,
  data = stocks,
  theme = standard){ 
  
  ggplot(data, aes(x = eval(parse(text = x)), 
                     y = eval(parse(text = y)), 
                     color = Ticker)) +
    geom_line() +
    labs(title = title,
         subtitle = subtitle,
         y = ylab,
         x = xlab) +
    theme
}

#-------------------------------------------------------------------------------

# Define themes:
standard <- (theme(panel.grid.minor = element_blank()) +
  theme_minimal())

bloomberg <- theme(text = element_text(family = 'Gill Sans', color = "#444444")
                   ,panel.background = element_rect(fill = '#444B5A')
                   ,panel.grid.minor = element_line(color = '#4d5566')
                   ,panel.grid.major = element_line(color = '#586174')) 

#-------------------------------------------------------------------------------

# Execute the functions by inserting data here:

# Insert ticker, index, start date, end date (optional)
stocks <- tickers(ticker = dow30[6:10],
                  index = "^DJI",
                  date.from = "2018-01-01",
                  date.to = NA)

# Plot and enjoy:
plot.stocks(x = "Date", 
            y = "Normalized",
            title = "Tickers")

