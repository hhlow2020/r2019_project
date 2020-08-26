#install.packages("tidyverse")
#install.packages("psych")
install.packages("readr")

library("psych")
library(tidyverse)
library(readr)
library(dplyr)

#wow read.csv is an OUTDATED way of reading csv. use read_csv
#project becomes project.name, no..of.units --your HEADING change already when u use read.csv
#read_csv is faster than read.csv
start.time <- Sys.time()
realis19_1<-read.csv("./data/REALIS2019.csv")
end.time <- Sys.time()
time.taken <- end.time - start.time
realis19<-read_csv("./data/REALIS2019.csv") #in a tableau dataframe which is GREAT when using rmarkdown

#tidyr -->pivoting, rectangling nesting, 
popdata<-read_csv("./data/PopData2019_fat.csv")
#damn easy to do group by. 
popdata_long<-popdata %>% pivot_longer(c(3:21), names_to = "Age Group",values_to = "Population") 
#sick the regex
who_longer<-who %>% pivot_longer(cols = new_sp_m014:newrel_f65, names_to = c("diagnosis", "gender","age"), 
                                 names_pattern ="new_?(.*)_(.)(.*)",
                                 values_to = "count")