# Install and Load Packages
packages = c('magrittr','lubridate','reshape','tidyverse', 'dplyr')
for (p in packages){
  if(!require(p,character.only = T)){
    install.packages(p)
  }
  library(p, character.only = T)
}
library(Rmisc)
library(PASWR)
library(BSDA)
library(car)
library(MASS)
library(fitur)
library(datasets)

#tidyr -->pivoting, rectangling nesting, 
popdata<-read_csv("./data/PopData2019_fat.csv")
#damn easy to do group by. 
popdata_long<-popdata %>% pivot_longer(c(3:21), names_to = "Age Group",values_to = "Population") 
#sick the regex
who_longer<-who %>% pivot_longer(cols = new_sp_m014:newrel_f65, names_to = c("diagnosis", "gender","age"), 
                                 names_pattern ="new_?(.*)_(.)(.*)",
                                 values_to = "count")

#wow read.csv is an OUTDATED way of reading csv. use read_csv
#project becomes project.name, no..of.units --your HEADING change already when u use read.csv
#read_csv is faster than read.csv
start.time <- Sys.time()
#realis19_1<-read.csv("./data/REALIS2019.csv")
end.time <- Sys.time()
time.taken <- end.time - start.time


unclean<-read_csv("./Desktop/ASAR/Data Cleaning - Distributed/R Data Cleaning.csv")
select(unclean,1:3)
unclean$`DAL Score`[unclean$`DAL Score`=='NIL']<-0
unclean$`R Score`[is.na(unclean$`R Score`)]<-0#removed na
unclean[['DAL Score']]= as.numeric(unclean$`DAL Score`)
unclean[['R Score']]= as.numeric(unclean$`R Score`)

#change date format
date1<-paste('01',unclean$Date,sep="-")
date2<-as.Date(date1,"%d-%b-%y")
unclean['year']<-year(date2)
unclean['month']<-month(date2)
unclean <- unclean[c(1,5,6,2,3,4)] #reorder the column

                       

str(unclean)
sapply(unclean, class)
#unclean$'DAL Score'= as.numeric(unclean$`DAL Score`)

unclean %>% group_by(Name,Date) %>%summarize(mysum=sum(`R Score`))

df<-unclean %>% group_by(Name) %>%summarize(mysum=sum(`R Score`))
rankdf<-dd[order(dd['mysum'],decreasing = TRUE),]
R.Class <- data.frame(
  Name = c("Jeff", "Harry", "Andrew"), R.Class = c("G2", "G1", "G1"))

newdf<-left_join(unclean,R.Class, by="Name")
?as.Date

#tutorial 5
sample <- c(160,162,162,165,170,172,172,172)
since n<30 pop var is uknown,use student t test
t.test(sample)
162.55,171.19

schA<-c(160,162,162,165,170,172,172,172)
schB<-c(160,160,162,163,165,167,168,169)
t.test(schA,schB,var.equal = TRUE)
-2.122,7.37

test1<-c(67,24,57,55,63,54,56,68,33,43)
mean(test1)
test2<-c(70,38,58,58,56,67,68,77,42,38)
mean(test2)
t.test(test1,test2,paired = TRUE,conf.level = 0.9)
-9.49,-0.907

5^2/16+6^2/9

prop.test(21,100,correct = FALSE,conf.level = 0.99)
0.1249, 33.12

prop.test(c(21,9),c(100,75),correct = FALSE,conf.level = 0.99)
#in the sample spade, it is NOT if i don't choose B, it auomatically goes to A.
-0.0526,0.2326

q6
barry<-c(126,110,138,142,146,136,94,103,140,162,108,97)
mary<-c(114,118,114,111,129,119,97,104,127,133,103,108)
t.test(barry,mary,paired = TRUE )

1.458,19.38
95% confident that there is a difference between the two expert opinion.

qnorm(0.975)
qnorm(0.025)

#tutorial 6
library("PASWR")
53.1+qnorm(0.9,53.1,10/8,FALSE)
53.1-qnorm(0.9,53.1,10/8,FALSE)

(53.1-52)/(10/8)
qnorm(0.9)
1-pnorm(0.88)
qnorm(0.975,13,4/sqrt(8))
1-pnorm(53.1,52,10/8)

qn2
0/(sqrt(16/8))
qnorm(0.975)
since 0.5<
x= c(8,8.5,8.6,9.2,10.8)
mean(x)
t.test(x,mu=8)
sd1=sd(x)
t.test(x,alternative="less",mu=8)
t.test(x,alternative="greater",mu=8)
mean(x)-2.1068*(sd1/sqrt(5))
mean(x)+2.1068*(sd1/sqrt(5))

qt(0.05,4)
-2.131847*(sd1/sqrt(5))+9.02

pnorm(160,147,23)
qnorm(0.05,147,23)
1-pnorm(7500,147*50,sqrt(50*23^2))
pnorm(6.8,6.7,sqrt(3.1^2/300))-pnorm(6.5,6.7,sqrt(3.1^2/300))
(3.1*qnorm(0.25)*5)^2
qnorm(0.92)*(18/sqrt(250))+45

1-pnorm(500,450,300/sqrt(100))

(pnorm(0/sqrt(7/8))-pnorm(-1/sqrt(7/8)))/(1-pnorm(-1/sqrt(7/8)))
pt(0,7)
-pnorm(12,12,sqrt(6/40))
1-pnorm(0,-1,sqrt(6/40+7/8))
-0.5
(pnorm(11,11,sqrt(7/8))-pnorm(10,11,sqrt(7/8)))/(1-(pnorm(10,11,sqrt(7/8))))

since pnorm(31.2,32,sqrt(1/18)) < 0.01,
we reject the null hypothesis,
statistically, we can conclude that the mean is less than 32. 

paired t test

?t.test
mary<-c(114,118,114,111,129,119,97,104,127,133,103,108)
barry<-c(126,110,138,142,146,136,94,103,140,152,108,97)
t.test(barry,mary,alternative = "two.sided",paired = TRUE,conf.level = 0.95)
t.test(mary,barry,alternative = "less",paired = TRUE)

group1<-c(80,75,90,60,55,78,59,88,75,90)
group2<-c(98,75,89,96,77,69,80,90,74,93)

t.test(group1,group2,alternative="less",conf.level=0.95)
1-pt(-1.7311,17.068)
since interval does not contain 0, difference. 
pbinom(0,2,0.8)

diff<-c(12,-8,24,31,17,17,-3,-1,13,19,5,-11)
mean_diff=mean(diff)
sd_diff<-sd(diff)
tstat<-mean_diff/(sd_diff/sqrt(12))

1-pexp(15,7.5)
tstat
1-exp(-15/7.5)
1-exp(-0.5)

pexp(15,8/60)

for two tailed test, just x2 to compare with 5%, or 
just use the answer and compare to 2.5%. 
(1-pt(tstat,11))*2

qt(0.975,11)
use the tstat NOT for the confidence interval. but to find the probability.

#additional exercise
q1
pie = 95/150
s = sqrt(95/150*(1-95/150)/150)
qnorm(0.005)*s+pie
qnorm(0.995)*s+pie

q2
ho: no diff
h1: diff
diff<-9.5-7.8
s = sqrt(1.9^2/52+1.5^2/35)
qnorm(0.975)*s+diff
qnorm(0.025)*s+diff
qnorm(0.975)
diff/s
since 0 is not in, conclude that there is a difference

assumptions, independently distributed, randomly drawn

q4
n >30, normally distributed
ztest<-(147-145)/(20/sqrt(200))
since z test <, we do not reject. 
qnorm(0.95)
pnorm(ztest)

q5
tstat<-(3.18-3.1)/(0.54/sqrt(20))
1-pt(tstat,19)
do not reject, cannot conclude it's higher'

q6
(2995-3262)/(1100/sqrt(50))
qnorm(0.05)
since z< -1.64484, do not reject. 

q7:
p1<- 0.58
p2<-0.21
d<-0.58-0.21
po = (351+41)/(605+195)
z= d/sqrt((po*(1-po)/605+po*(1-po)/195))
z

prop.test(x=c(351,41),n=c(605,195),correct=FALSE)
qnorm(0.975)
qnorm(0.025)

q14
s = sqrt((0.62*0.38/100+0.29*0.71/100))
diff<-0.62-0.29
qnorm(0.025)*s+diff
qnorm(0.975)*s+diff
since 0 not inside, yes different.

#pooled variance t-test
df<-21+25-2
qt(0.95,4)
stddev<-(20*1.3^2+24*1.16^2)/44
t = (3.27-2.53)/sqrt(stddev*(1/21+1/25))
t
sample size too small
sample deviation is too large compared to mean. 
usually after training, employer is interested in improvement, 
hence the test will be one-tailed, which will increase the rejection region  

q12
bef<-c(68,72,62,70,58,66,68,52,64,72,74,60,74,72,74)
aft<-c(70,80,77,76,76,76,78,78,82,64,74,92,74,88,84)
mean(bef)
mean(aft)
sd(aft)
sd(bef-aft)
t.test(aft,bef,alternative = "greater",paired = TRUE,conf.level = 0.95)
dependent samples!
qt(0.975,14)

#One-way F-test

install.packages("ggpubr")
install.packages("car")

n1=n2=n3=5
k = 3
n = n1+n2+n3
c1<-c(254,263,241,237,251)
c2<-c(234,218,235,227,216)
c3<-c(200,222,197,206,204)
xbar1 = mean(c1)
xbar2 = mean(c2)
xbar3 = mean(c3)
xbar = mean(c(xbar1,xbar2,xbar3))
SSG = n1*(xbar1-xbar)^2+n2*(xbar2-xbar)^2+n3*(xbar3-xbar)^2
MSG = SSG/(k-1)
SSW1 = sd(c1)^2*(n-1)
SSW2 = sd(c2)^2*(n-1)
SSW3 = sd(c3)^2*(n-1)
SSW = SSW1+SSW2+SSW3
MSW = SSW/(n-k)
F = MSG/MSW

alpha = 0.95
df1 = k-1
df2 = n-k
qf(0.95,2,12)

Since F> 3.88, we reject h0. 
there is indeed evidence that there is at least one mean differs from the res
turkey's range test HSD. honestly significant difference'

#analysis of variance
?aov
aov(Distance ~ Club, data = my_data)

q4
h0: same
h1: greater
(147-145)/(20/sqrt(200))
qnorm(0.95)
pnorm(1.414)
since z test is < 1.6445, do not reject h0. no evidence to suggest otherwise. 

q5
since n is small, t test. 
(3.18-3.1)/(0.54/sqrt(20))
qt(0.95,19) -->#one sided T test. 
since t-stat < 1.73, do not reject. 

q6
since n is large, use z test
h1: u< 3262
(2995-3262)/(1100/sqrt(50))
qnorm(0.05)
since z test is < -1.6448, reject null. indeed college seniors have less debt

q7
prop.test(c(351,41),c(605,195),alternative = "two.sided",conf.level=0.95, correct = FALSE)
since confidence interval does not contain 0, we can reject h0 and conclude that 
statistically, there is indeed a difference. 

q8
h0: men and women same (u1 = u2)
h1: women>men (u2<u1) because more effective
(0.38-0.51)
?prop.test
prop.test(c(38,102),c(100,200),conf.level = 0.99,alternative = "less",correct = FALSE)
since p-value less than 0.05,
reject h0. there is evidence to suggest that women more effective. 

pnorm(-5.16)


tutorial 8- ANOVA
h0: u1=u2=u3
h1: not all u are equal
my_data <- data.frame("Distance" = c(254,263,241,237,251,234,218,235,227,216,200,222,197,206,204), "Club" = c(rep("Club1",5),rep("Club2",5),rep("Club3",5)))

#just to see 
library(dplyr)
group_by(my_data, Club) %>% summarise(count = n(), mean = mean(Distance),sd = sd(Distance))


res.aov<-aov(Distance~Club,data=my_data)
res2<-summary(res.aov)
res.aov$coefficients
res.aov$xlevels
res2
            Df Sum Sq Mean Sq F value   Pr(>F)    
Club         2   4716  2358.2   25.27 4.99e-05 ***
Residuals   12   1120    93.3                     
---
so we know k = 3 (3 groups of clubs)
we know n = 15 (15 observations)

  
TukeyHSD(res.aov)
$Club
            diff       lwr        upr     p adj
Club2-Club1 -23.2 -39.49801  -6.901995 0.0066342
Club3-Club1 -43.4 -59.69801 -27.101995 0.0000342
Club3-Club2 -20.2 -36.49801  -3.901995 0.0159903

all clubs are significantly DIFFERENT, as proven by p-value
since diff club 2 less than club 1, club 3 less than club 2, 
the rank is 1,2,3. mu1>mu2>m3
if P-value not significant, the means are the SAME so you can put:
u1 = u2 if club2-club pvalue is not statistically significant.

library("car")
levenetest for homogeneity
leveneTest(Distance~Club,data=my_data)
Since p> 0.05, do not reject and conclude homogeneity

next run shapiro test
#res.aov<-aov(Distance~Club,data=my_data)
aov_residual <- residuals(object= res.aov)
shapiro.test(x=aov_residuals)
Shapiro-Wilk normality test

data:  aov_residuals
W = 0.94355, p-value = 0.4291

h0: sample come from normal, not violated
h1: non-normal

since p-value >0.05, do not reject. 

my_data<-PlantGrowth
my_data
#NOT TESTING if they have the same means, but testing whether they originate from same distribution
kruskal.test(weight~group,data=my_data)
data:  weight by group
Kruskal-Wallis chi-squared = 7.9882, df = 2, p-value = 0.01842
As the p-value is less than the significance level 0.05, 
we can conclude that there are significant differences between the treatment groups.

use wilcoxon rank test instead of tukey HSD

pairwise.wilcox.test(PlantGrowth$weight, PlantGrowth$group,
                     p.adjust.method = "BH")
data:  PlantGrowth$weight and PlantGrowth$group 

ctrl  trt1 
trt1 0.199 -    
  trt2 0.095 0.027

P value adjustment method: BH --go read up more if you want to. 
The pairwise comparison shows that,
only trt1 and trt2 are significantly different (p < 0.05).
trt1 ctrl no significant diff as p-value 0.199> 0.05
trt2 ctrl no significant diff as p-value 0.095> 0.05
trt2 > trt1 as p-value 0.027<0.05. 
mu trt2 > mu trt1
if p-value is > the mu are the same for the two! 

two way ANOVA is pretty similar, also use aov.
unbalanced anova test will NOT be tested.

tutorial 8, qn 4
my_data4 <- data.frame("Accuracy" = c(12,10,18,12,14,10,17,16,13,16,14,16,11,20,21), "Eye" = c(rep("Right",5),rep("Left",4),rep("Both",6)))
res.aov<-aov(Accuracy~Eye,data=my_data4)
res2<-summary(res.aov)
res2
get p value of F. 
1-pf(1.287,2,12)

slide7-pg18/37
nyse and nasdaq
?t.test
n1 = 21
n2 = 25
m1 = 3.27
m2= 2.53
sd1 = 1.3
sd2 = 1.16
sp = ((n1-1)*sd1^2+(n2-1)*sd2^2)/(n1+n2-2)
t = (m1-m2)/sqrt(sp/n1+sp/n2)
t
qt(0.975,44)..two tailed test! 
since t > qt, do not reject h0. 

#classpart3
my_data <- data.frame("Product" = c(10,15,8,12,15,14,18,21,15,17,16,14,15,17,15,18,17,15,17,15,16,15), "Technique" = c(rep("Tech1",5),rep("Tech2",4),rep("Tech3",7),rep("Tech4",6)))
my_data
res.aov<-aov(Product~Technique,data=my_data)
summary(res.aov)
TukeyHSD(res.aov)

Tutorial 8 question 3
my_data2 <- data.frame("Time" = c(10,5,15,4,13,8,20,10,14,7,20,20,12,5,28,15,20,12,18,15,30,8,30,35,20), 
                      "Age" = c(rep("a",4),rep("b",4),rep("c",3),rep("d",5),rep("e",5),rep("f",4)))
my_data2
res.aov2<-aov(Time~Age,data=my_data2)
summary(res.aov2)

pnorm(2)-pnorm(-2)
?t.test
prop

z<-c(560,500,470,660,640)
meanz<-mean(z)
sdz= sd(z)
meanz
sdz
qt(0.975,4)*sdz/sqrt(5)+meanz
qt(0.025,4)*sdz/sqrt(5)+meanz
qt(0.95,4)*sdz/sqrt(5)+meanz
pnorm(0)

qnorm(0.975)*(10/sqrt(50))+25

#ANOVA- whether MEAN is different

my_data <- data.frame("Distance" = c(254,263,241,237,251,234,218,235,227,216,200,222,197,206,204), 
                      "Club" = c(rep("Club1",5),rep("Club2",5),rep("Club3",5)))
res.aov=aov(Distance~Club,data= my_data)
summary(res.aov)
As p-value is less than, reject nullfile()

####
tutorial 9- multiple linear regression
###
Roller coaster

height<-c(400,415,377,318,310,263,259,245,240,235,230,224)
speed<-c(120,100,100,95,93,81,81,85,79,85,80,70)

cor(height,speed,method="pearson")
cor.test(height,speed)
p-value < 5% indicate that both are significantly correlated

my_data<-read.csv("./Downloads/MultipleRegression.csv")
fit<-lm(speed~height)
summary(fit)

fit1<-lm(y~., data=my_data)
summary(fit1)
library(MASS)
step<-stepAIC(fit1,direction="both") # the model after fitting excludes variable 3
#the conclusion is that without variable 3, the r^2 is the highest. 
summary(step)

fit3<-lm(y~1, data=my_data) #fit on intercept only- find mean of y. 
summary(fit3)

stepAIC(fit1,direction="backward")
stepAIC(fit3,direction="forward",scope=list(upper=fit1,lower=fit3))
stepAIC(fit3,direction="both",scope=list(upper=fit1,lower=fit3))

#AIC and #BIC are just optimization criterion, to minimize 

library(tidyverse)
library(broom)
install.packages("datarium")
library(datarium)
data("marketing",package="datarium")
model<-lm(sales~youtube,data=marketing)
library(broom)
model.met<-augment(model)
model.met
par(mfrow=c(2,2))
plot(model)
model2<-lm(log(sales)~youtube,data=marketing)
par(mfrow=c(2,2))
plot(model,5)
plot(model, 4, id.n = 5)

# to show cook's distance in the plot
df2 <- data.frame(
  x = c(marketing$youtube, 500, 600),
  y = c(marketing$sales, 80, 100)
)
df2
model3 <- lm(y ~ x, df2)
plot(model3,4)
plot(model3,5)

#chapter 10
Chisquare test1
r = 2
c = 2
dof = (r-1)*(c-1)
qchisq(0.95,dof)
qchisq(0.025,5,lower.tail = FALSE)
1-pchisq(23.05,6) 

Since the observed and expected frequencies of the first and last interval are less than 5, 
it is better to combine the 1st and 2nd 
as well as the last and second to last intervals.
pnorm(0,4,2.5)*90
(pnorm(1,4,2.5)-pnorm(0,4,2.5))*90

#Tutorial 10- Chi-Square

Q1
R1 <- c(12,108)
R2 <- c(24,156)
rows <- 2

Matriz <- matrix(c(R1,R2),nrow=rows, byrow =TRUE)

rownames(Matriz) <- c("Female","Male")
colnames(Matriz) <- c("Left-Handed","Right-Handed")
Matriz

Result <- chisq.test(Matriz,correct=FALSE)
print(Result)
1) Since p-value > 0.05, 2) we do not reject the null hypothesis. 3) statistically, 
we do not have enough evidence to conclude that there is an association between gender and hand preference

Q2
r1<-c(160,140,40)
r2<-c(40,60,60)
mat<-matrix(c(r1,r2),nrow=2,byrow = TRUE)
rownames(mat)<-c("sal","hour")
colnames(mat)<-c("p1","p2","p3")
mat

res<-chisq.test(mat,correct = FALSE)
res
49.632 is the chi square
at df 2, 0.05 alpha, 5.99 is the level
remember to use 0.95! 
qchisq(0.95,2)

compare 49.632 and 5.99. since outside, reject H0.

since p=value < 0.0.5, we reject the null hypothesis. Statistically, we can conclude that
there is an association between a&b

qchisq(0.95,2)<- This is NOT 0.975!
res$expected
res$observed
res$observed-res$expected
##***## sal tend to choose p1, hour choose p3. 
Q3

call.freq <- c(290,250,238,257,265,230,192)
call.prob <- c(1/7,1/7,1/7,1/7,1/7,1/7,1/7)
#donot have to manually calculate WOW impressive. 
res2<-chisq.test(call.freq, p=call.prob)
res2$statistic
res2$parameter
res2$residuals
res2$stdres
res2
since p-value <0.05, conclude there is a difference in calls between days

Q4
let x be the number of heads in 10 flips. x~b(10,0.5)
p(x=0)
flips = 10
x0= dbinom(0,10,0.5)
x1= dbinom(1,10,0.5)
x2= dbinom(2,10,0.5)
x3= dbinom(3,10,0.5)
x4= dbinom(4,10,0.5)
x5= dbinom(5,10,0.5)
x6= dbinom(6,10,0.5)
x7= dbinom(7,10,0.5)
x8= dbinom(8,10,0.5)
x9= dbinom(9,10,0.5)
x10= dbinom(10,10,0.5)
sum(coin.prob)
coin.freq <- c(1,2,3,9,18,26,21,13,5,2,0)
coin.prob <- c(x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10)
res3<-chisq.test(coin.freq,p=coin.prob,correct = FALSE)
chisq.test(coin.freq,p=coin.prob,correct = FALSE)
res4<-chisq.test(coin.freq,p=coin.prob,simulate.p.value = TRUE)
we do not reject the null. 
res3$parameter

heads<-c(0,1,2,3,4,5,6,7,8,9,10)
dbinom(heads,size=10,prob = 0.5)

1-pnorm(2)

x<-seq(3,30,5)
y<- x[x>16 | x<8]
y
z<-rep(rep(seq(-1,1),rep(2,3)),2)
w<-c(y,z)

library(datasets)
mtcars
apply(mtcars[,1:3],2,sd)

1-pnorm(20.75,20,sqrt(5^2/100))

expect = c(0,1,2,3,4,5,6,7,8,9,10)
expected = dbinom(expect,10,0.4)

observed = c(1,2,3,9,18,26,21,13,5,2,0)
chisq.test(observed,p=dbinom(x=expect,10,0.4))

res<-chisq.test(observed,p=expected)
res$statistic
pchisq(res$statistic,9,lower.tail = FALSE)
qchisq(0.95,df=9)

total=(observed-expected*100)^2/(expected*100)
qchistat<-sum(total)
df= 11-1-1 = 9
confidence= qchisq(0.95,9)
since qchistat > confidence, reject. 

repractise additional questions
q1
sd<-sqrt(95/150*(1-95/150)/150)
xbar<- 95/150
qnorm(0.995)
2.575829*sd+xbar
xbar-2.575829*sd

q2
n1=21
n2=25
x1=3.27
x2=2.53
sd1=1.3
sd2=1.16

sp2=(20*1.3^2+24*1.16^2)/(44)
sp2
t = (x1-x2)/(sqrt((sp2/21)+sp2/25))
t
qt(0.975,44)
since t>qt, reject h0.

x=c(6,20,3,0,4)
y=c(4,6,2,0,0)
t.test()

paired test
t.test(x,y,paired = TRUE,conf.level = 0.95)
qt(0.975,4)

speed<-c(30,50,40,55,30,25,60,25,50,55)
mile<-c(28,25,25,23,30,32,21,35,26,25)

cov(speed,mile,method="pearson")
cor(speed,mile)
fit<-lm(speed~mile)
summary(fit)

x<-c(160,240,290,210,256,150,200,140)
y<-c(56,92,97,82,95,30,73,56)
fit<-lm(y~x)
summary(fit)
sqrt(0.8218)
at the 0.05 level,
qt(0.975,6)  
##*** very important. Remember if you do t-test,
##*need to -2 df and use 0.975 for 5%.)***


manf<-c(8.8,10.5,12.5,9.7,9.6,13.2)
comp<-c(8.4,10.1,12,9.3,9,13)
resx<-t.test(manf,comp,paired = TRUE)
resx
t = 7.6787, df = 5, p-value = 0.0005971
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
  0.2771799 0.5561535
sample estimates:
  mean of the differences 
0.4166667 

to get confidence interval
#0.41667+qt(0.975,5)*res$stderr
resx$stderr

qt(0.975,5)*0.05426

pnorm(1.44)-pnorm(0)

z.test(x, y = NULL, alternative = "two.sided", 
       mu = 0, sigma.x = NULL,
       sigma.y = NULL, 
       conf.level = 0.95)
test if the difference 
between two samples is greater than
a fixed value? mu and sigma? 
  
t.test(group1,group2,alternative="two.sided",conf.level=0.95)

z.test()
pnorm(0)
pnorm(0.975)
qnorm(0.975)
x<-c(10,8,7,12,9,6,7,8)
z.test(x,alternative = "two.sided", mu= 13, sigma.x = 4, conf.level = 0.95)

prop.test(x=c(351,41),n=c(605,195),alternative="two.sided",conf.level = 0.95,correct = FALSE)

prop.test(x=c(38,102),n=c(100,200),alternative="less",conf.level = 0.99,correct = FALSE)

n>30, z test

(200-190)/(sqrt(400/100+1600/100))
qnorm(0.95) # one sided, more effective

bef<-c(68,72,62,70,58,66,68,52,64,72,74,60,74,72,74)
aft<-c(70,80,77,76,76,76,78,78,82,64,74,92,74,88,84)
mean(bef-aft)/(sd(bef-aft)/sqrt(15))
-4.0315

meanbef = mean(bef)
sdbef=sd(bef)
meanaft=mean(aft)
sdaft = sd(aft)
effectiveness, never say better or worse.

two tailed, t test dependent. 
qt(0.975,14)
qt(0.05,14)
since dbar outside, reject. test is effective. 

#extra assignment
pnorm(1.75,1.25,0.46)-pnorm(1,1.25,0.46)
0.568
qnorm(0.995,64,0.78)
66.00915
pexp(2100/4000,1) #both give same answer
pexp(2100,1/4000) #both give same answer 
pexp(2/3,1)
pexp(2,3)

stat hw 5
brandA<-c(17,29,18,14,21,25,22,29)
brandB<-c(21,38,15,19,22,30,31,37)
qt(0.95,7)
t.test(brandA,brandB,alternative = "less",conf.level = 0.95,paired = TRUE)
Do not reject null hypothesis, brand A not better. 
number of observations are independently sampled.
population is normally distributed

brand A minus brand B, can he infer that brand A makes less mistakes.
Therefore A-B <-0. therefore alternative hypothesis is LESS THAN. 


bef<-c(10,3,16,11,8,2,1,14,5,6)
aft<-c(5,0,7,4,6,4,2,3,5,1)
t.test(bef,aft,alternative="greater",conf.level = 0.95,paired = TRUE)
yes does better. 
independently sampled, population is normal. 

to reduce the variance in the testing. keep the same cars. 

man1<-c(8.8,10.5,12.5,9.7,9.6,13.2)
man2<-c(8.4,10.1,12,9.3,9,13)
t.test(man1,man2,alternative="two.sided",conf.level = 0.95,paired=TRUE)
WRONG!! this is the correct one.

q4
prop.test(x<-c(642,740), n<-c(1140,1060), conf.level = 0.99, alternative = "two.sided",correct = FALSE)

agree. since p-value < 0.01, reject null. there is a difference

5

speed<-c(30,50,40,55,30,25,60,25,50,55)
mileage<-c(28,25,25,23,30,32,21,35,26,25)

cor(,speed,method="pearson")
cor.test(height,speed)
p-value < 5% indicate that both are significantly correlated

fit<-lm(mileage~speed)
summary(fit)

yes. There is a relationship. as speed increases mileage decreaes by 0.286. 

q6
x<-c(160,240,290,210,256,150,200,140)
y<-c(56,92,97,82,95,30,73,56)
fit<-lm(y~x)
summary(fit)
summary(fit)
fit$model

qt(0.975,6)
when x = 180, y = 
  
10/sqrt(0.7^2*100)
pnorm(1.428571)
qnorm(0.97)*0.38
12.95-0.7147
pnorm(12.95,12.2353,0.38)
pnorm(-0.4,0.35,0.33)***
qnorm(0.03)
ppois(0,2)
dbinom(4,5,0.13533)
1-pnorm(-0.25)
qnorm(0.875,72,8)

pnorm(66,72,8/sqrt(14))

qt(0.975,5)
qt(0.95,5)

40/20
1-pnorm(2)
pnorm(-2.5)
qnorm(0.95)

qf(0.95,3,16)
yes! this is your qnorm for F test! 

sqrt(0.15*0.85/100)
pnorm(0.18,0.15,0.03570714)

qt(0.99,7)
2.997952*0.0381/sqrt(8)
diff<-c(-0.05,0.02,-0.06,-0.02,-0.1,0,-0.06,-0.03)
sd(diff)


prop.test(x<-c(42,34),n<-c(75,75),correct=FALSE)

pbinom(1,10,0.05)
1-pnorm(1,0.5,sqrt(0.5*(0.95)))
prop.test(x<-)

price<-c(16,20,21,9,10,22,20,15,10,10)
length<-c(520,680,740,200,400,800,750,500,300,350)
fit<-lm(price~length)
summary(fit)

(standard error)^2 * (n-2) = SSE
se = 1.186
se2 = 1.186^2
se2*8
1-SSE/SST = R^2
SST = 11.26/(1-0.9543)
SST




