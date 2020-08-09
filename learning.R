x <- matrix(1:6,2,3)
x[1,2, drop= FALSE]
y <- c(1,2,NA,4,5)
y[!is.na(y)]
good <- complete.cases(y)
y[good]
in
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
unclass(d)
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

<<<<<<< HEAD
pmatch("z",   c("mean", "median", "mode")) # returns NA
pmatch("med", c("mean", "median", "mode")) # returns 2
c("mean","median")
x <- 1:10
if(x > 5) {
  x <- 0
}

f <- function(x) {
  g <- function(y) {
    y + z
  }
  z <- 4
  x + g(x)
}
z<-5
f(3)
x <- 5
y <- if(x < 3) {
  NA
} else {
  10
}
=======
a <- c(1,2,3)
b <- c(4,5)
c(a,b)
sample(c(a,b),3) #just from a list of 5 choose 3.
a[c(-1,-2)]
a[-c(1,2)]
my<- c(4,5)
dim(my)<-c(1,2)
#matrices can ONLY contain ONE class of data. 
>>>>>>> c17210f83c827d18a98e981aae1e7259f8a07750
