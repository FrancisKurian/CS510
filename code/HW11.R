
# Homework #11 Begins here

kc_house_dat <- read.csv(file = '00 kc_house_data.csv')
#head(kc_house_dat)

length(kc_house_dat)
nofRows=nrow(kc_house_dat)
nofRows

# generate the index numbers of 70% random sample
set.seed(0)
Train_index <- sample(seq(1,nofRows),floor(nofRows*.7))
nTrain =length(Train_index)
nTest = nofRows-nTrain

# subset the dataset using the 70% random sample index numbers

Train_kc <- kc_house_dat[Train_index,]
#head(Train_kc)
names(Train_kc)

# create test data set with remaining 30% records

Test_kc <- kc_house_dat[-Train_index,]
#head(Test_kc)
names(Test_kc)


# Number of records and # of variables in Training,Test and original datasets
dim(Train_kc)
dim(Test_kc)
dim(kc_house_dat)


#Average house price of the training data.
mean(Train_kc$price)

#Average house price of the Test data.
mean(Test_kc$price)


# problem 2
arrythmia_dat <- read.csv(file = 'arrythmia.csv',header=F,nrows=100)

dim(arrythmia_dat)

arrythmia_dat5 <- arrythmia_dat[1:5]

dim(arrythmia_dat5)
mean(arrythmia_dat5$V1)

arrythmia_dat5$V1Norm <-(arrythmia_dat5$V1-mean(arrythmia_dat5$V1))/sd(arrythmia_dat5$V1)
arrythmia_dat5$V2Norm <-(arrythmia_dat5$V2-mean(arrythmia_dat5$V2))/sd(arrythmia_dat5$V2)
arrythmia_dat5$V3Norm <-(arrythmia_dat5$V3-mean(arrythmia_dat5$V3))/sd(arrythmia_dat5$V3)
arrythmia_dat5$V4Norm <-(arrythmia_dat5$V4-mean(arrythmia_dat5$V4))/sd(arrythmia_dat5$V4)
arrythmia_dat5$V5Norm <-(arrythmia_dat5$V5-mean(arrythmia_dat5$V5))/sd(arrythmia_dat5$V5)

# print first 5 rows of V1
head(arrythmia_dat5)
# Print the means and SD of the normalized data
round(mean(arrythmia_dat5$V1Norm))
sd(arrythmia_dat5$V1Norm)

# A more elegant solution is to use the vectorization capability of R

# remove character fields to generate column level stats as a vector
df_all <- arrythmia_dat[1:5]
#head(df_all)

#calculate Mean as a vector for each column
df_mean <- colMeans(df_all)
#df_mean

#calculate Standard deviation as a vector for each column
df_sd <- apply(df_all, 2, sd)
#df_sd

# transpose the data table so mean and sd can be applied as a vector operation
df_z <- t((t(df_all)-df_mean)/df_sd)
head(df_z)








# Discretize each numerical attribute into 10 equi-width ranges. Compute the count of
# elements in each bin for all the columns.


NumCategories = 10
c1 = cut(arrythmia_dat5$V1,breaks=NumCategories)

(count1 = as.vector(table(c1)))

# problem 3
UScities_dat <- read.csv(file = 'RawDataUSCities.csv',header=T)

UScities_dat$PercentBlackZvalue <-(UScities_dat$PercentageBlack-mean(UScities_dat$PercentageBlack))/sd(UScities_dat$PercentageBlack)
UScities_dat$PercentHispanicZvalue <-(UScities_dat$PercentageHispanic-mean(UScities_dat$PercentageHispanic))/sd(UScities_dat$PercentageHispanic)
UScities_dat$PercentAsianZvalue <-(UScities_dat$PercentageAsian-mean(UScities_dat$PercentageAsian))/sd(UScities_dat$PercentageAsian)
UScities_dat$MedianAgeZvalue <-(UScities_dat$MedianAge-mean(UScities_dat$MedianAge))/sd(UScities_dat$MedianAge)
UScities_dat$UnemploymentRateZvalue <-(UScities_dat$UnemploymentRate-mean(UScities_dat$UnemploymentRate))/sd(UScities_dat$UnemploymentRate)
UScities_dat$PerCapitaIncomeThousandZvalue <-(UScities_dat$PerCapitaIncomeThousand-mean(UScities_dat$PerCapitaIncomeThousand))/sd(UScities_dat$PerCapitaIncomeThousand)
head(UScities_dat)

# A more elegant method would be to use vectorization capabilities of R 

# remove character fields to generate column level stats as a vector
df_all <- subset(UScities_dat, select = -c(City,X.) )
#head(df_all)

#calculate Mean as a vector for each column
df_mean <- colMeans(df_all)
#df_mean

#calculate Standard deviation as a vector for each column
df_sd <- apply(df_all, 2, sd)
#df_sd

# transpose the data table so mean and sd can be applied as a vector operation
df_z <- t((t(df_all)-df_mean)/df_sd)
head(df_z)



#function to scale the values from 0-1

normalize = function(x) {
  return( (x-min(x))/(max(x)-min(x)))}

#scale the fields
UScities_dat$PercentBlackScaled <-normalize(UScities_dat$PercentageBlack)
UScities_dat$PercentHispanicScaled <-normalize(UScities_dat$PercentageHispanic)
UScities_dat$PercentAsianScaled <-normalize(UScities_dat$PercentageAsian)
UScities_dat$MedianAgeScaled <-normalize(UScities_dat$MedianAge)
UScities_dat$UnemploymentRateScaled <-normalize(UScities_dat$UnemploymentRate)
UScities_dat$PerCapitaIncomeThousandScaled <-normalize(UScities_dat$PerCapitaIncomeThousand)

head(UScities_dat)


# Homework 12

# Calculation of Leibniz series
lval2=0
v_lei <- c()
v_n <- c()
v_pi <- c()
p=0
while (p<=200)
{
  
  lval = (((-1.0)**p)/(2.0*p+1.0))
  lval2=lval2+lval
  v_lei <- append(v_lei,lval2) 
  v_n  <- append(v_n,p)
  v_pi <- append(v_pi,pi/4)
  p =p+ 1
}
#v_lei


plot(v_n, v_lei,
     
     ylab="Sum",
     xlab="N",
     type="l",
     col="blue",
     ylim=c(-0.2,1)
)
lines(v_pi, type = "l", col = "blue")
v_diff <- v_lei-v_pi

lines(v_diff, type = "l", col = "blue")


text(100,1,"Leibniz Series",cex=1.0)
text(100,0.3,"difference between π/4 and sum S(n)",cex=1.0)



# QUESTION 2


# Define the Mean and Stdev
mean=1152
sd=84

#x values create a sequence of values gived standard deviation and meand that covers +- 3.5 SD
#y values create normal distribution of x

x <- seq(-3.5,3.5,length=100)*sd + mean
y <- dnorm(x,mean,sd)

#multiple plots using mfrow

old.par <- par(mfrow=c(1, 3))
plot(x, y, type="l")

# use polygon to mark/shade the shapes
polygon(c(x[x>=1250], max(x), 1250), c(y[x>=1250], 0, 0), col="red")
plot(x, y, type="l")
polygon(c(x[x<=1200], 1200, 1200), c(y[x<=1200], 0, 0), col="red")


plot(x,y,type="l")
x=seq(1000,1100,length=100)
y=dnorm(x,mean=1152,sd=84)
polygon(c(1000,x,1100),c(0,y,0),col="red")
par(old.par)

par(mfrow=c(1,1))


