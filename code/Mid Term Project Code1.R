# Mid Term Project
# Read CSV file from data folder using the company name
# Do data exploration, apply data cleaning issues, do basic plots to understand
# apply testing using the r functions

# getwd()

ForexFile <- './data/FederalReserve_CurrencyXchangeRate.csv'

BizSentimentIndexFile <- './data/Boeing.csv'


headers = read.csv(ForexFile, skip = 5 , header = F, nrows = 1)
head(headers)
df_forex = read.csv(ForexFile, skip = 6, header = F)
colnames(df_forex)= headers

#convert Time Period char field to date to merge with other daily data files

df_forex$period <- as.Date(df_forex$`Time Period`)
sapply(df_forex, class)
head(df_forex)

df_forex <- df_forex[!duplicated(df_forex$period),]
head(df_forex)
summary(df_forex)



# Read Business Sentiment Index Data File

df_BSI = read.csv(BizSentimentIndexFile, header = T)
df_BSI$period <- as.Date(df_BSI$created_date,format='%m/%d/%Y')

head(df_BSI)
sapply(df_BSI, class)

# merge Exchange rate and Sentiments data

xx <- merge(x=df_BSI, y=df_forex, by="period",all.x=TRUE ) 
head(xx)
sapply(xx, class)

#at data points are matched to the daily data, remove records if curresponding records are not available

