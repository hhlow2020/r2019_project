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
mapply(rep, 1:4,4:1)
traceback()
sum(flags[,17])

mean(iris[,'Sepal.Length'])
apply(iris[,1:4],2,mean)
debug(ok)
ok()
library(datasets)

fileurl<-"https://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD"
tdate<-date()
test<-paste("./",tdate,"+cameras.csv", sep="")
#formatting is ugly but this works i guess
download.file(fileurl,destfile = test,method="curl")
read.csv("cameras.csv")

fileurl<-"http://www.w3schools.com/xml/simple.xml"
download.file(fileurl, destfile = "./food.xml")
#install.packages("XML")
library(XML)
doc<-xmlTreeParse(file="./food.xml",useInternalNodes=TRUE)
rootnode<-xmlRoot(doc)
xmlName(rootnode)
names(rootnode) #5 food menu, but does not show price etc. 
rootnode[[1]] #shows the first element
rootnode[[1]][[2]] #name price desc calories etc.
xmlSApply(rootnode[[1]],xmlValue)
#learn xpath /node //node *get top value, get specific value etc

xpathSApply(rootnode,"//price",xmlValue)

#next part html
htmlurl<-"https://www.espn.com/nfl/teams/_/name/bal/baltimore-ravens/"
htmlurl2<-"https://www.premierleague.com/stats/records"
download.file(htmlurl,destfile = "./espn_raven.html")
download.file(htmlurl2,destfile = "./epl.html")
doc<-htmlTreeParse(file="./espn_raven.html",useInternalNodes = TRUE)
doc2<-htmlTreeParse(file="./epl.html",useInternalNodes = TRUE)
scores<-xpathSApply(doc2,"//table[@class='js-records-section']",xmlValue)
scores[[5]]
scores

install.packages("xml2")
library(xml2)
read_xml("./epl.html")

length(scores)

score2<-paste(scores, collapse="")
class(score2)
grep("Manchester United",score2)
install.packages("stringr")
library(stringr)
str_count(score2,"Manchester United")
#sep \n
class(score2[[1]])
score2
sscore<-strsplit(score2,"\n \n([0-9]+)*")
sscore
sscore[[1]][[2]]
for(i in seq_along(score2)){
  
}

class_vec <-c(15,16,17,18,19,20,16)

mean(class_vec)

median(class_vec)

getmode<-function(v){
  uniqv<-unique(class_vec)
  test<-uniqv[which.max(tabulate(match(c(15,17),uniqv)))]
  return(test)
}

getmode(class_vec)

curve(dnorm(x,mean = 0, sd =1), xlim = c(-3.5,3.5),ylab = 'Density', 
      main = "Standard Normal Density Function")

cv<-c(1,2,2,2,3,4,5,5,6,6,7)
counter = 0
init = 0
for(i in unique(cv)){
  temp<-cv[cv==i]
  print(temp)
  mmode = length(temp)
  if(mmode>init){
    print(paste(mmode,init, sep = ","))
    final<-mmode
    init<-mmode
  }
}

final

install.packages("combinat")
library(combinat)
combn(7,3)
choose(7,3)
install.packages('gtools')
library(gtools)
permutations(3,2)
nrow(permutations(3,2))

#lexical scoping
library(swirl)
install_from_swirl("Getting and Cleaning Data")
swirl()
cran

numbers <- c(0,1,2,2,1,3,3,8)
#getmode <- function(v){
  uniqv <- unique(numbers)
  tab <- tabulate(match(numbers,uniqv))
  uniqv[tab == max(tab)]
}
print(tab)
getmode (numbers)

1-pnorm(120,100,11)
pnorm(120,100,11,lower.tail = FALSE)
pnorm(115,100,sqrt(121))-pnorm(85,100,sqrt(121))
dbinom(5,14,0.13)
1-pbinom(50,120,0.1)
?pnorm
1-pnorm(20/121)
dbinom(2,9,0.1)

pnorm(230,210,sqrt(90))-pnorm(190,210,sqrt(90))

qnorm(0.95,203,(2.5^2)/20)
?dnorm
install.packages("fitur")
library(fitur)
pdunif(3,1,5)
ddunif(3,1,5)

pbinom(0:10,10,0.5)
qbinom(0.9453,10,0.5)

since n is 150, 
hhnp <-150*0.4 
hhvar <- (150*0.4*0.6)

pnorm(65,60,sqrt(hhvar),lower.tail = FALSE)
qnorm(0.95,60,6)

1-pnorm(50,12,sqrt(10.8))

sample_means = rep(NA, 1000)
for(i in 1:1000){
  sample_means[i] = mean(rexp(40,0.2))
}
mean(sample_means)

rexp # random generation of exponential

mean = 44
std dev<- sqrt(16^2/64)

1-pnorm(37000,36000,sqrt(7000^2/75))
pnorm(680,700,120/sqrt(144))
#WOW i forgot this. we take 7 samples, 
IQ test score, N(100,11^2)
where each sample has a sample size of 35 people from the population. 
what is the probability that 6 out of the 7 samples have mean less than 80?

i remember calculate probability of mean>80 for one sample then use binompdf  
calculate sample mean and std dev,then use binom? yes
  
sample_mean <-rep(NA,1000)
for(i in 1:1000){
  sample_mean[i]<-mean(rnorm(24,20,4.5))
}
mean(sample_mean)
var(sample_mean)

hist(sample_mean)

1-pnorm(303,300,5)

x<- 5
y<- if(x<3){
  NA
} else {
  10
}
y
dbinom(2,20,0.11)

1-pnorm(37000,36000,sqrt(7000^2/75))
pexp(1,1/3)

runif(10,1,3)
qnorm(0.9,134,16) #no such thing as 1-qnorm 

pnorm(91,80,21)
qnorm(0.05,80,21)
qnorm(0.05,47.5,sqrt(16/30))
z<-2.5/((4/sqrt(30)))
z

a <-c(160,162,162,165,170,172,172,172)
b<-c(160,160,162,163,165,167,168,169)
diff<-a-b
xbarA<-mean(a)
xbarB<-mean(b)
sdA<-sd(a)
sdB<-sd(b)
pooled_s<-((8-1)*sdA^2+(8-1)*sdB)/(14)
a <-c(160,162,162,165,170,172,172,172)
b<-c(160,160,162,163,165,167,168,169)
t.test(a,b,var.equal=TRUE)

a<-c(560,500,470,660,640)
mean = mean(a)
sd(a)/sqrt(5)
interval<-qt(0.025,df=5-1)*sqrt(var/5)
lower = mean+interval
upper = mean-interval
upper
lower

b<-c(160,160,162,163,165,167,168,169)
diff<-a-b
mean = mean(diff)
sd = sd(diff)
interval<-qt(0.025,df=8-1)*(sd/sqrt(8))
lower = mean+interval
upper =mean-interval
#0.956 and 4.29

t.test(diff)

paired_test
week1<-c(67,24,57,55,63,54,56,68,33,43)
week2<-c(70,38,58,58,56,67,68,77,42,38)
diff_week<-week2-week1
dbar<-mean(diff_week)
sd<-sd(diff_week)
interval<-qt(0.05,df=9)*(sd/sqrt(10))
dbar+interval
dbar-interval

p = 0.21
sd = sqrt(0.21*(1-0.21)/100)
interval = qnorm(0.005)
sd*interval
0.21+(interval*sd)
0.21-(interval*sd)



0.21+qnorm(1-0.005)*sqrt(0.21*(1-0.21)/100)
0.21-qnorm(1-0.005)*sqrt(0.21*(1-0.21)/100)

?prop.test
prop.test(x<-c(21,9),n<-c(100,75),conf.level = 0.99,correct=FALSE)


dpois(3,0.4)
1-ppois(2,0.4)
dpois(2,10/7)
pnorm(21,20,6/sqrt(62))-pnorm(20,20,6/sqrt(62))
1-pnorm(200,203,2.5/sqrt(2))

pnorm(405,406,sqrt(2*2.5^2))-pnorm(400,406,sqrt(2*2.5^2))
1-pnorm(200,203,2.5)
pbinom(70,80,0.88493)
80*0.88493
80*0.88493*(1-0.88493)
pnorm(70.5,70.7944,sqrt(8.146))

qnorm(0.975)*2.5/sqrt(20)

1.64484*2.5/sqrt(20)

1-pnorm(99.4,5*19.8,0.1*sqrt(5))
pnorm(0.2,19.8/2-9.8,sqrt(0.1^2+0.25*0.1^2))
2*50*19.8-90*9.8+198
0.1^2*50*4+90*0.1^2
dpois(1,4)

pnorm(2,2,sqrt(2/35))
interval <-qnorm(0.01)
20.2+interval *6/sqrt(50)
20.2-interval *6/sqrt(50)
qnorm(0.01,20.2,6/sqrt(50))
qnorm(0.99,20.2,6/sqrt(50))
-1.8/(6/sqrt(50))
pnorm(-2.12132)

(299.2-300)/sqrt(27.4/100)
pnorm(-1.528321)
qnorm(0.97,299.2,sqrt(27.4/100))
qnorm(0.01)
-2.326*std/sqrt(n)+mu

pt(2.5,14)-pt(-2.5,14)
qt(0.1,14)
-1.34
qnorm(0.02)
(108-110)/(sqrt((15/14*6.82^2))/sqrt(15))

heat<-c(13.2,12.2,11.4,14.5,11.6,12.9,12.4,10.3,12.3,11.8,11,13,12.1,12.6)
population must be normally distributed.
qt(0.1,13)
t.test(heat,conf.level = 0.9,alternative = "less")
mean(heat)
sd(heat)
(12.24-12)/(sd(heat)/sqrt(14))

(2.4-2.5)/(1.2/sqrt(50))

(50-47)/(10/sqrt(50))
qnorm(0.01)

stocks<-c(7.7,8,7.4,7.8,8.4,7.2,7.9,8.2,7.6)
sds= sd(stocks)
tstat= -0.2/(sds/3)
tstat = -1.5894
qt(0.05,8) -1.8595
pt(tstat,8)
0.305
t.test(stocks,conf.level = 0.95,mu=8,alternative = "less")

since u is within, do not reject null hypothesis
population is normally distributed.
?t.test()

pt(8.03398,8)

q8)h0: u = 40%
u is different from 40%
pnorm(0.43,0.4,sqrt(0.4*0.6/120))
1-pnorm(0.671)
pnorm(1.645)

qnorm(0.005)
since pnorm<0.995, do not reject h0. 
pnorm(1.645)

q6) to reject null hypothesis, u< 50.
qnorm(0.01)
u <-47
xbar=-2.3263*1.25+u
xbar

library("PASWR")
q9)
z.test()

q10)

?prop.test
?t.test

diff = 3
std deviation = 
qt(0.975,23)
interval= qt(0.975,23)*sqrt(5^2/16+6^2/9)
sqrt(5^2/16+6^2/9)
sqrt(28.6261*(1/16+1/9))
interval2 = qt(0.025,23)*sqrt(5^2/16+6^2/9)
interval
interval2
since falls within, do not reject.                

qt(0.05,8)

1-pnorm(0.589255)
pnorm(2.4)

qnorm(0.01)
pnorm(4.7263)


t.test()
?prop.test

xbar = 0.43
u = 0.4
std_dev = sqrt(0.4*0.6/120)
0.4-2.5758*std_dev
0.4+2.578*std_dev
qnorm(0.005)
z = 0.03/std_dev
qnorm(0.95)
1-pnorm(z)

diff=60.8-58.4
z= diff/sqrt(9.9^2/100+8.7^2/100)
z
qnorm(0.95)
do not reject

# Plotting US map with values
state_choropleth(loan_by_state, title = "           Total Loan Volume by State - Millions $")