# Mid Term Project
# Read CSV file from data folder using the company name
# Do data exploration, apply data cleaning issues, do basic plots to understand
# apply testing using the r functions

# getwd()
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

# Read Business Sentiment Index Data File

df_BSI = read.csv(BizSentimentIndexFile, header = T)
df_BSI$period <- as.Date(df_BSI$created_date,format='%m/%d/%Y')

head(df_BSI)
sapply(df_BSI, class)
summary(df_BSI)

# merge Exchange rate and Sentiments data

xx <- merge(x=df_BSI, y=df_forex, by="period",all.x=TRUE ) 
head(xx)
sapply(xx, class)
summary(xx)

#process company level data and plot series to figure out any data issues
df_companies = read.csv(CompanyNames, header = T)
head(df_companies)

cn <- 'GE'

for (i in df_companies$ticker) {
  
  #pick the csv file of every company in the input list and create a dataframe in a loop
  
  df_c <- read.csv(paste0("./data/",i, ".csv"), header = T) 
  df_c$period <- as.Date(df_c$Date) # add a date field 
  df_c$ticker <- i # attach ticker
  df_companies_subset <- subset(df_companies, ticker==i) 
  df_c <- merge(x=df_c, y=df_companies_subset, by="ticker",all.x=TRUE )  # merge with companies list to get the name
  
  assign(paste0("df_",i), df_c) #create a new data frame for each company for later use
  
  empty_df = df_c[FALSE,]
  empty_df
  
  if (exists("df_c_all")) {
    df_c_all <- rbind(df_c_all, df_c)
  
  }else {
    df_c_all=df_c}
  }
  
head(df_F)
library(ggplot2) 
ggplot(data = df_c_all, aes(period, Close)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "steelblue") + 
  labs(title = "New Marvel characters by alignment",
       subtitle = "(limited to characters with more than 100 appearances)",
       y = "Count of new characters", x = "") + 
#  facet_grid(name~ .) 
  facet_wrap(~name,scales="free",ncol=2)



# df3 <- rbind(df1, df2)
