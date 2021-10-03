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
  
  #pick the csv file of the specific company in the list
  
  df_c <- read.csv(paste0("./data/",i, ".csv"), header = T)
  df_c$period <- as.Date(df_c$Date)
  df_c$ticker <- i
  df_companies_subset <- subset(df_companies, ticker==i)
  print(df_companies_subset )
  df_c <- merge(x=df_c, y=df_companies_subset, by="ticker",all.x=TRUE ) 
  #create a new data frame for each company for later use
  assign(paste0("df_",i), df_c)
  
  plot(x= df_c$period,y= df_c$Close,    type ='p',pch=19)
  
  }
head(df_c)

# df3 <- rbind(df1, df2)
