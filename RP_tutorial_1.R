getOption('100')
pollutantmean<-function(directory,pollutant,id=1:332){
  filelist<- list.files(path=directory,full.names = TRUE)
  class(filelist) #vector of characters 
  values<- numeric()
  for(i in id){
    data<-read.csv(filelist[i])
    values <-c(values, mean(data[[pollutant]], na.rm = TRUE ))
  }
  print(length(values))
  mean(values,na.rm = TRUE)
}

c<-vector("hello")
d<-vector("world")
class(data[Pollutant]) #dataframe NOT numeric.
pollutantmean("specdata","sulfate",1:10)


pollutantmean2<-function(directory,pollutant,id=1:332){
  filelist<- list.files(path=directory,full.names = TRUE)
  class(filelist) #vector of characters 
  dat <- data.frame()
  for(i in id){
    dat<-rbind(dat, read.csv(filelist[i]))
  }
  mean(dat[, pollutant], na.rm = TRUE)
}

pollutantmean2('specdata','sulfate',1:10)

print('
The above two OBVIOUSLY has a very big difference.
one is equally weighted across all datapoints, 
the other is equally weighted across all dataframes 
10,10,10 and 4,4,4,4,4. 
1. Average across both dataframe is (10+4)/2 (pollutantmean)
2. Average across all datapoints is (50/8) (pollutantmean2) CORRECT
')

directory <- 'specdata'
complete<-function(directory,id=1:332){
  datf <- data.frame(id=integer(),nobs=integer())
  print(datf)
  filelist<- list.files(path=directory,full.names = TRUE)
  for(i in id){
    data<-read.csv(filelist[i])
    nobs <- sum(complete.cases(data))
    temp <- data.frame(i,nobs)
    datf<- rbind(datf, temp)
  }
  datf
}

complete('specdata',1:3)

x<-data[complete.cases(data),]
class(x)
#nrow(data[complete.cases(data),])
sum(complete.cases(data))

corvec<- vector()
threshold <- 150
directory <- 'specdata'
filelist<- list.files(path=directory,full.names = TRUE)
length(filelist)
data<-read.csv(filelist[5])
nobs <- sum(complete.cases(data))
if(nobs > threshold){
  data2 <- data[complete.cases(data),]
  corvec<-c(corvec,cor(data2['sulfate'],data2['nitrate']))
}
corvec

x<-list("hello world",2,3) 
#which is why you learn different structures because 
# if you want 2 list to be stored as a list
# you use matrix or dataframe, otherwise it will be combined
class(corvec)
#q3 if location monitor is above threshold append to dataframe
# use the entire dataframe to calculate cor. 
corr<-function(directory,threshold =0){
  filelist<- list.files(path=directory,full.names = TRUE)
  len<- length(filelist)
  tempdata <- data.frame()
  corvec <- vector()
  for(i in 1:len){
    data<-read.csv(filelist[i])
    nobs <- sum(complete.cases(data))
    if(nobs > threshold){
      data2 <- data[complete.cases(data),]
      #print(cor(data2['sulfate'],data2['nitrate']))
      #tempdata<- rbind(tempdata,data2)
      corvec<-c(corvec,cor(data2['sulfate'],data2['nitrate']))
    }
    
  }
  corvec #wow this is so important. don't put inside the for loop, 
  #it will return the last known which is corvec <- vector()
}

head(corr("specdata",400))
summary(corr("specdata",400))

cr <- corr("specdata")                
cr <- sort(cr)   
RNGversion("3.5.1")
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)
