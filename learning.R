x <- matrix(1:6,2,3)
x[1,2, drop= FALSE]
y <- c(1,2,NA,4,5)
y[!is.na(y)]
good <- complete.cases(y)
y[good]

#a<- matrix(1,2,3)
#b <- matrix(4,5,6)
#a*b is NOT matrix multiplication
#a %*% b is a matrix multiplication 
a<- c(1,2,3)
b <- c(4,5,6)
cbind(a,b)
#row bind is add each input to a new roll
#col bind is add each input to a new col.
d <- c(1,2,5,10,15)
#d[d<4]==0
d[d%in%2:10]<-0
#as long as d is within 2:10 all put as 0. 
d
df<-read.csv("./hw1_data.csv")
dim(df)
tail(df)
df[47:50,]
df[is.na(df),2]
sum(is.na(df))
summary(df)
dtemp<-df[(df['Ozone']>31)&(df['Temp']>90),]
dt<-dtemp[!is.na(dtemp['Solar.R']),]
colMeans(dt['Solar.R'],) 
colMeans(df[df['Month']==6,]['Temp'])
max(df[df['Month']==5,]['Ozone'],na.rm = T)
