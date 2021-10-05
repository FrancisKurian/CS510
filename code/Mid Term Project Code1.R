# Mid Term Project
# Read CSV file from data folder using the company name
# Do data exploration, apply data cleaning issues, do basic plots to understand
# apply testing using the r functions

# getwd()
library(ggplot2) 
library(tidyr)
library(plyr)
library(lubridate)
library(scales)
library(reshape2)
library(summarytools)

CompanyNames <- './data/CompanyNames.csv'
ForexFile <- './data/FederalReserve_CurrencyXchangeRate.csv'

BizSentimentIndexFile <- './data/Boeing.csv'


headers = read.csv(ForexFile, skip = 5 , header = F, nrows = 1)
head(headers)
df_forex = read.csv(ForexFile, skip = 6, header = F)
colnames(df_forex)= headers

#convert Time Period char field to date to merge with other daily data files

df_forex$period <- as.Date(df_forex$`Time Period`)

# convert char fields into numbers

df_forex$USDxINR <- as.numeric(df_forex$RXI_N.B.IN)
df_forex$USDxEUR <- as.numeric(df_forex$`RXI$US_N.B.EU`)
df_forex$USDxYEN <- as.numeric(df_forex$RXI_N.B.JA)

df_forex <- subset(df_forex,select = c(period, USDxINR, USDxEUR, USDxYEN))

# sort and remove any duplicate daily data points

df_forex <- df_forex[!duplicated(df_forex$period),]
sapply(df_forex, class)
head(df_forex)
summary(df_forex)


# merge Exchange rate and Sentiments data

xx <- merge(x=df_BSI, y=df_forex, by="period",all.x=TRUE ) 
head(xx)
sapply(xx, class)
summary(xx)

#process company level data and plot series to figure out any data issues
df_companies = read.csv(CompanyNames, header = T)
head(df_companies)

rm(df_c_all)
rm(df_bsi_all)

for (i in df_companies$ticker) {
  #process stock price/volume files from data directory
  #pick the csv file of every company in the input list and create a dataframe in a loop
  
  df_c <- read.csv(paste0("./data/",i, ".csv"), header = T) 
  df_c$period <- as.Date(df_c$Date) # add a date field 
  df_c$ticker <- i # attach ticker
  df_companies_subset <- subset(df_companies, ticker==i) 
  df_c <- merge(x=df_c, y=df_companies_subset, by="ticker",all.x=TRUE )  # merge with companies list to get the name
  
  assign(paste0("df_",i), df_c) #create a new data frame for each company for later use

# create a data frame to append  every data frame for graphs  
  
  if (exists("df_c_all")) {
    df_c_all <- rbind(df_c_all, df_c)
  
  }else {
    df_c_all=df_c}
  
  # Process Sentiments Index from data directory
  
  df_bsi <- read.csv(paste0("./data/",i, "_BSI.csv"), header = T) 
  df_bsi$period <- as.Date(parse_date_time(df_bsi$created_date, c('%Y-%m-%d', '%m/%d/%y')))
    df_bsi$ticker <- i # attach ticker
  df_bsi <- merge(x=df_bsi, y=df_companies_subset, by="ticker",all.x=TRUE )  # merge with companies list to get the name
  df_bsi <- df_bsi[c('ticker','period','name','bsi_score')]

  if (exists("df_bsi_all")) {
    df_bsi_all <- rbind(df_bsi_all, df_bsi)
    
  }else {
    df_bsi_all <- df_bsi}
  
  
  }

ggplot(data = df_c_all, aes(period, Close)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "steelblue") + 
  labs(title = "Time Series of Stock Prices by Company",
       subtitle = "(Visualization to check any obvious data issues)",
       y = "Stock Prices - Closing ", x = " Date") + 
  facet_wrap(~name,scales="free",ncol=2)

ggplot(data = df_c_all, aes(period, Volume)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "steelblue") + 
  labs(title = "Time Series of Stock trading volume",
       subtitle = "(Visualization to check any obvious data issues)",
       y = "Daily Trading Volume", x = " Date") + 
  facet_wrap(~name,scales="free",ncol=2) +
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6))


ggplot(data = na.omit(df_bsi_all), aes(period,bsi_score)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "steelblue") + 
  labs(title = "Time Series of Stock trading volume",
       subtitle = "(Visualization to check any obvious data issues)",
       y = "Daily Trading Volume", x = " Date") + 
  facet_wrap(~name,scales="free",ncol=3) 

head(df_forex)

ggplot(data = na.omit(df_forex), aes(period,USDxEUR)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "steelblue") + 
  labs(title = "Time Series of Stock trading volume",
       subtitle = "(Visualization to check any obvious data issues)",
       y = "Daily Trading Volume", x = " Date") 



df_forex2 <- melt(df_forex, id.vars="period")

ggplot(na.omit(df_forex2), aes(period,value)) + 
  geom_point() + 
  stat_smooth() +
  facet_wrap(~variable,scales="free",ncol=1)


# merge stock price, sentiment index and exchange rate data sets 
# ensure daily data is clean and missing values are treated

dfSummary(df_forex, style = "grid", plain.ascii = TRUE)
dfSummary(df_c, style = "grid", plain.ascii = TRUE)
dfSummary(df_bsi, style = "grid", plain.ascii = TRUE)

df_forex <- na.omit(df_forex)
dfSummary(df_forex, style = "grid", plain.ascii = TRUE)

# merge all three datasets

df_all <- merge(df_bsi, df_forex, by="period",all.x=TRUE )  # merge with Forex file

df_all <- merge(x=df_all, y=df_c, by="period",all.x=TRUE )  # merge with stock price 

df_all <- subset(df_all,select=c(period,	ticker.x,	name.x,	bsi_score,	USDxINR,	USDxEUR,	USDxYEN,	Close,	Volume))

dfSummary(df_all, style = "grid", plain.ascii = TRUE)


df_all <- df_all %>%
  rename(ticker = ticker.x,
         CompanyName = name.x,
         Stock.Price = Close,
         Stock.Volume= Volume )

write.csv(df_all, "./data/company_full_merge.csv")

dfSummary(df_all, style = "grid", plain.ascii = TRUE)

