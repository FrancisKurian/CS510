# Get content into a data frame 

fly_data= read.table("blowfly.txt", header=T)
sapply(fly_data,class)
fly_data$flies <- as.numeric(fly_data$flies)

#clean up the data frame -remove NA
fly_data <- na.omit(fly_data)

#Print Number of records
length(fly_data$flies)

#Print Number of unique records
length(unique(fly_data$flies))

# cummulative interest problem
n=0
#intial principal
p=10000
rate=6

while (p <= 1000000)
{
  p  <- p+(p*rate/100)+10000
  n=n+1
  
}
# Years taken to reach $1,Million
n
# Exact $ amount at the end of the period.
p

#3 World Floras
detach(wflora)
wflora= read.table("worldfloras.txt", header=T)
head(wflora)
attach(wflora)
# Grep/Reg Exp for Countries with 4th position as C
clist =as.vector(Country[grep("^.{3}c",as.character(Country))])
clist

# Calculation of Area 

area_p <- function(volume,radius){
  h=3*volume / (pi * radius**2)
  A =pi*radius*(sqrt(radius**2 + h**2))
  return (A)
  
}


v_a <- c()
v_r <- c()
p=0.1
while (p <= 10)
{
  v_a <- append(v_a,area_p(10,p)) 
  v_r <- append(v_r,p) 
  
  p=p+0.1
  
}
v_a
v_r

plot(v_r, v_a,
     
     ylab="Area",
     xlab="Radius",
     type="l",
     col="blue"
)


# recursive function for factorial

r_factorial <- function(num) {
  if (num == 0)    return (1)
  else           return (num * r_factorial(num-1))
}


v_fact =c()
p=1
while (p <= 10)
{
  v_fact <- append(v_fact,r_factorial(p)) 
  p=p+1
  
}
v_fact

# Read Cells and Multivariate Files


cells= read.table("cells.txt", header=T)
sapply(cells,class)
cells= read.table("cells.txt", header=F,skip=1,col.names=c("New_cells","New_smoker","New_age","New_sex","New_weight" ))
sapply(cells,class)


mvariate= read.table("multivariate.txt", header=T)
sapply(mvariate,class)

mvariate= read.table("multivariate.txt", header=F,skip=1,col.names=c("New_Temp","New_Industry","New_Population","New_Wind","New_Rain","New_Wet.days" ))
sapply(mvariate,class)

# write acscii file
write.table(cells,"cells_new.txt",row.names=FALSE,sep="\t", quote = FALSE)
write.table(mvariate,"multivariate_new.txt",row.names=FALSE,sep="\t", quote = FALSE)


# Write Binary File
save(cells,file="cells_new.dat")
save(mvariate,file="multivariate_new.dat")
