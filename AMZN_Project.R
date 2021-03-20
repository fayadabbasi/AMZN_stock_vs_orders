library(dplyr)
library(plyr)
library(readr)
library(ggplot2)

# import data set of AMZN orders from 2006 to current date
df <- read.csv("/Users/fayadabbasi/Dropbox/Investing/AMZN_Order_Project/complete_order_history.csv")

# create a data set for stock prices

library(tidyquant)
library(quantmod)

getSymbols("AMZN", from="2006-1-2", to="2021-3-11", auto.assign = T)

AMZN <- data.frame(AMZN)
AMZN$Date <- row.names(AMZN)

# convert to Date format - this is variable to combine the two data sets on
AMZN$Date <- as.Date(AMZN$Date)
df$Order.Date <- as.Date(df$Order.Date, format="%m/%d/%y")

# merge the two data sets
AMZN.order.stock.combo.2 <- merge(df,AMZN, by.x="Order.Date", by.y="Date")
skinny_df <- AMZN.order.stock.combo.2 %>% select(
    Order.Date, Item.Total, AMZN.High
) 
# create item total column by removing $ sign
skinny_df$Item.Total.2 <- as.numeric(gsub('[$]','',skinny_df$Item.Total))

# create column that calculates how many shares you would have bought, based on the high price of the stock
skinny_df$shares <- skinny_df$Item.Total.2 / skinny_df$AMZN.High

# calculate the column shares.total which is running sum of shares
skinny_df <- skinny_df %>% mutate(shares.total = cumsum(skinny_df$shares))
# calculate the column spend.total which is running sum of dollars spent
skinny_df <- skinny_df %>% mutate(spend.total = cumsum(skinny_df$Item.Total.2))
# calculate stock.value which is the high price of stock * total shares from cumulative calculation
skinny_df$stock.value <- skinny_df$AMZN.High * skinny_df$shares.total

# plot the total value of portfolio over time
ggplot(skinny_df,aes(Order.Date, stock.value)) + geom_line()

# NEED TO MODIFY X AND Y AXIS LABELS AS WELL AS CHART TITLE
# MAYBE USE DIFFERENT TEMPLATE FOR VIEW
# Y AXIS NOT IN EXP FORMAT BUT IN DOLLARS

# more garbage 

# View(skinny_df)

# create a new data set for looking at the annual spend
annual_AMZN_exp <- tapply(skinny_df$Item.Total.2, format(skinny_df$Order.Date, "%Y"), sum)
# plot the annual spend on AMZN
barplot(annual_AMZN_exp) # NEED TO LABEL ALL THIS INFORMATION
# NEED TITLE, MODIFY Y AXIS TO HAVE $, MAYBE EVEN LABEL BAR TO SEE ANNUAL SPEND IN CHART
